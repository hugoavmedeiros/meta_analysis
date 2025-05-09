#### arXiv ----

buscar_citacoes_arXiv_semantic <- function(arxiv_id) {
  if (is.na(arxiv_id) || arxiv_id == "") return(NA)

  # 🔹 Remover o sufixo da versão (ex: v1, v2, etc.)
  arxiv_id <- gsub("v[0-9]+$", "", arxiv_id)

  url <- paste0("https://api.semanticscholar.org/graph/v1/paper/arXiv:", arxiv_id, "?fields=citationCount")

  resposta <- tryCatch(GET(url), error = function(e) return(NA))

  if (http_status(resposta)$category == "Success") {
    dados <- content(resposta, as = "parsed", type = "application/json")

    # 🔹 Acessar corretamente `citationCount`
    if (!is.null(dados$citationCount)) {
      return(dados$citationCount)
    }
  }
  return(NA)
}

buscar_citacoes_arXiv_crossref <- function(link_doi) {
  if (is.na(link_doi) || link_doi == "") return(NA)

  # 🔹 Extrair o DOI do formato do arXiv
  doi <- gsub("http://dx.doi.org/", "", link_doi)  # Remove o prefixo
  doi <- gsub("\\s+", "", doi)  # Remove espaços extras

  url <- paste0("https://api.crossref.org/works/", doi)

  resposta <- tryCatch(GET(url), error = function(e) return(NA))

  if (http_status(resposta)$category == "Success") {
    dados <- content(resposta, as = "parsed", type = "application/json")

    # 🔹 Acessar corretamente `is-referenced-by-count`
    if (!is.null(dados$message[["is-referenced-by-count"]])) {
      return(dados$message[["is-referenced-by-count"]])
    }
  }
  return(NA)
}

#### pubmed ----
buscar_citacoes_pubmed_pmc <- function(pmid) {
  if (is.na(pmid) || pmid == "") return(NA)

  url <- paste0("https://api.ncbi.nlm.nih.gov/lit/ctxp/v1/pmc/?format=csl&id=", pmid)
  resposta <- tryCatch(GET(url), error = function(e) return(NA))

  if (http_status(resposta)$category == "Success") {
    dados <- content(resposta, as = "parsed", type = "application/json")
    return(ifelse(is.null(dados$'is-referenced-by-count'), NA, dados$'is-referenced-by-count'))
  } else {
    return(NA)
  }
}

# 🔹 Função para buscar citações no CrossRef (via DOI)
buscar_citacoes_pubmed_crossref <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)

  doi <- gsub("https://doi.org/", "", doi)  # Remove prefixo se existir
  doi <- gsub("\\s+", "", doi)

  url <- paste0("https://api.crossref.org/works/", doi)

  resposta <- tryCatch(GET(url), error = function(e) return(NA))

  if (http_status(resposta)$category == "Success") {
    dados <- content(resposta, as = "parsed", type = "application/json")
    if (!is.null(dados$message[["is-referenced-by-count"]])) {
      return(dados$message[["is-referenced-by-count"]])
    }
  }
  return(NA)
}

#### scielo ----
meses_lista <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez",
  "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December",
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# 🔹 Função para buscar artigos no SciELO
buscar_scielo <- function(query, num_paginas = 8) {

  base_url <- "https://search.scielo.org/?q="
  encoded_query <- URLencode(query)  # Codificar a query para URL
  all_articles <- list()

  for (page in 1:num_paginas) {
    message(paste("🔍 Buscando página", page, "..."))

    # Construir URL correta com `&from=` para evitar pular artigos
    from_value <- (page - 1) * 15 + 1  # Define o início dos artigos por página
    search_url <- paste0(base_url, encoded_query, "&lang=pt&count=15&from=", from_value, "&page=", page)

    # Ler a página HTML
    pagina <- tryCatch(read_html(search_url), error = function(e) return(NULL))
    if (is.null(pagina)) {
      message(paste("❌ Falha ao carregar a página", page))
      next
    }

    # 🔹 Extrair os blocos de artigos
    artigos <- pagina %>% html_nodes(".item")

    # 🔹 Criar listas para armazenar os dados
    ids <- artigos %>% html_attr("id")  # Pega o ID do artigo

    titulos <- artigos %>% html_nodes(".title") %>% html_text(trim = TRUE)

    # Capturar múltiplos autores corretamente
    autores <- artigos %>% map_chr(~{
      autores_lista <- .x %>% html_nodes(".line.authors .author") %>% html_text(trim = TRUE)
      if (length(autores_lista) == 0) return(NA) else return(paste(autores_lista, collapse = ", "))
    })

    # 🔹 Capturar e limpar corretamente as informações do periódico
    revista_info <- artigos %>% map_chr(~{
      info <- .x %>% html_nodes(".line.source") %>% html_text(trim = TRUE)
      if (length(info) == 0) return(NA) else return(info[1])
    }) %>%
      str_squish() %>%  # Remove espaços extras
      str_remove_all("Métricas do periódico|Sobre o periódico|SciELO Analytics") %>%  # Remove textos indesejados
      str_squish()  # Remove espaços extras restantes após remoção

    # 🔹 Separar `journal_title` e `year`, removendo os meses do `journal_title`
    journal_titles <- str_extract(revista_info, "^[^0-9]+") %>%
      str_trim() %>%
      str_remove_all(paste0("\\b(", paste(meses_lista, collapse = "|"), ")\\b")) %>%  # Remove meses
      str_squish()  # Remover espaços extras

    years <- str_extract(revista_info, "\\b\\d{4}\\b")  # Extrai o ano de publicação

    # 🔹 Capturar corretamente o resumo, verificando se está escondido
    resumos <- artigos %>% map_chr(~{
      resumo_full <- .x %>% html_nodes(".abstract") %>% html_text(trim = TRUE)

      # Se o resumo estiver ausente, verificar se há um link de resumo oculto
      if (length(resumo_full) == 0) {
        resumo_hidden <- .x %>% html_nodes(".abstract[style='display:none']") %>% html_text(trim = TRUE)
        if (length(resumo_hidden) > 0) {
          return(resumo_hidden)  # Retorna o resumo oculto
        }
        return(NA)  # Se ainda não houver resumo, retorna NA
      }

      return(paste(resumo_full, collapse = " "))
    })

    # Capturar o DOI corretamente
    dois <- artigos %>% map_chr(~{
      doi_link <- .x %>% html_nodes(".DOIResults a") %>% html_attr("href")
      if (length(doi_link) == 0) return(NA) else return(doi_link[1])
    })

    # 🔹 Garantir que todas as colunas tenham o mesmo número de elementos
    max_len <- max(length(ids), length(titulos), length(autores), length(revista_info), length(journal_titles), length(years), length(resumos), length(dois))

    ids <- c(ids, rep(NA, max_len - length(ids)))
    titulos <- c(titulos, rep(NA, max_len - length(titulos)))
    autores <- c(autores, rep(NA, max_len - length(autores)))
    revista_info <- c(revista_info, rep(NA, max_len - length(revista_info)))
    journal_titles <- c(journal_titles, rep(NA, max_len - length(journal_titles)))
    years <- c(years, rep(NA, max_len - length(years)))
    resumos <- c(resumos, rep(NA, max_len - length(resumos)))
    dois <- c(dois, rep(NA, max_len - length(dois)))

    # Criar um DataFrame com os dados coletados
    df <- tibble(
      article_id = ids,
      title = titulos,
      authors = autores,
      journal_info = revista_info,  # 🔹 Mantém a info original do periódico com o mês
      journal_title = journal_titles,  # 🔹 Nome do periódico limpo (sem meses)
      year = years,  # 🔹 Ano do artigo
      abstract = resumos,
      doi = dois,
      page_number = page  # 🔹 Número da página original
    )

    # Adicionar à lista de resultados
    all_articles[[page]] <- df
    Sys.sleep(2)  # 🔹 Pequeno delay para evitar bloqueios
  }

  # Combinar todas as páginas em um único DataFrame
  final_df <- bind_rows(all_articles) %>% distinct()  # 🔹 Remove duplicatas

  return(final_df)
}

extrair_keywords_frequencia <- function(texto) {
  if (nchar(texto) == 0) return(NA)

  palavras <- tibble(texto = texto) %>%
    unnest_tokens(word, texto) %>%
    anti_join(stop_words, by = "word") %>%
    count(word, sort = TRUE) %>%
    slice_head(n = 5) %>%
    pull(word)

  paste(palavras, collapse = ", ")
}
