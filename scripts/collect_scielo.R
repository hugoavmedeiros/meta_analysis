library(httr)
library(rvest)
library(dplyr)
library(stringr)

buscar_scielo_melhorado <- function(query, num_paginas = 3) {
  artigos <- list()

  for (pagina in 1:num_paginas) {
    # 🔹 Refinando a busca para termos exatos no título e resumo
    query_formatada <- paste0("(ab:(\"", query, "\")) OR (ti:(\"", query, "\"))")
    url <- paste0("https://search.scielo.org/?q=", URLencode(query_formatada),
                  "&lang=en&page=", pagina)

    # Simular um navegador real
    resposta <- GET(url, user_agent("Mozilla/5.0"))

    # Verificar se a página carregou corretamente
    if (http_status(resposta)$category != "Success") {
      message("Erro ao acessar a página: ", url)
      next
    }

    # Ler a página HTML
    pagina_web <- read_html(resposta)

    # 🔹 Extrair títulos corretamente
    titulos <- pagina_web %>% html_nodes(".title") %>% html_text(trim = TRUE)

    # 🔹 Extrair links para os artigos
    links <- pagina_web %>% html_nodes(".title a") %>% html_attr("href")
    links <- paste0("https://search.scielo.org", links)

    # 🔹 Capturar os resumos corretamente usando a classe `.abstract`
    resumos <- sapply(links, function(link) {
      if (!is.na(link)) {
        tryCatch({
          artigo_pagina <- read_html(link)
          resumo <- artigo_pagina %>% html_nodes(".abstract") %>% html_text(trim = TRUE)
          if (length(resumo) == 0) return(NA) else return(resumo)
        }, error = function(e) return(NA))
      } else {
        return(NA)
      }
    })

    # Garantir que todos os vetores tenham o mesmo tamanho preenchendo valores faltantes
    max_length <- max(length(titulos), length(links), length(resumos))
    titulos <- c(titulos, rep(NA, max_length - length(titulos)))
    links <- c(links, rep(NA, max_length - length(links)))
    resumos <- c(resumos, rep(NA, max_length - length(resumos)))

    # Criar um DataFrame temporário
    df_temp <- data.frame(
      title = titulos,
      abstract = resumos,
      link = links,
      stringsAsFactors = FALSE
    )

    # Adicionar à lista
    artigos[[pagina]] <- df_temp

    # 🔹 Aguardar 3 segundos para evitar bloqueios
    Sys.sleep(3)
  }

  # Combinar todas as páginas em um único DataFrame
  df_scielo <- bind_rows(artigos)
  return(df_scielo)
}

df_scielo <- buscar_scielo_melhorado("data science team", num_paginas = 3)

# Visualizar os primeiros artigos coletados
head(df_scielo)
