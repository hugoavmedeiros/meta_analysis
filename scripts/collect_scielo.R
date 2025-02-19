library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# 🔹 Lista de meses em diferentes idiomas para remoção do título
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

# 🔹 Rodar busca no SciELO
query <- '"data science team" OR "data-science" OR "data science"'

query <- '"origin-destination matrix"'

df_scielo <- buscar_scielo(query, num_paginas = 3)
