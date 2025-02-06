library(rvest)
library(dplyr)
library(stringr)

buscar_scholar_completo <- function(query, num_paginas = 1) {
  artigos <- list()

  for (pagina in 0:(num_paginas - 1)) {
    # Criar a URL de busca com a query refinada
    url <- paste0("https://scholar.google.com/scholar?start=", pagina * 10,
                  "&q=", URLencode(query), "&hl=en")

    # Ler a página
    pagina_web <- read_html(url)

    # Extrair títulos
    titulos <- pagina_web %>% html_nodes(".gs_rt a") %>% html_text(trim = TRUE)

    # Extrair links (se existirem)
    links <- pagina_web %>% html_nodes(".gs_rt a") %>% html_attr("href")

    # Extrair informações de autores e ano
    info_extra <- pagina_web %>% html_nodes(".gs_a") %>% html_text(trim = TRUE)

    # Extrair número de citações (preenchendo com NA caso não haja citação)
    citacoes <- pagina_web %>% html_nodes(".gs_fl a") %>%
      html_text(trim = TRUE) %>%
      str_extract("Cited by [0-9]+") %>%
      str_extract("[0-9]+") %>%
      as.numeric()

    # 🔹 **Obter o Resumo (Texto Inicial da Página do Artigo)**
    resumos <- sapply(links, function(link) {
      if (!is.na(link)) {
        tryCatch({
          pagina_artigo <- read_html(link)
          texto <- pagina_artigo %>% html_nodes("p") %>% html_text(trim = TRUE)

          # Pegar os primeiros 3 parágrafos como "resumo"
          resumo <- paste(texto[1:3], collapse = " ")
          return(resumo)
        }, error = function(e) return(NA))
      } else {
        return(NA)
      }
    })

    # Garantir que todos os vetores tenham o mesmo tamanho preenchendo valores faltantes
    max_length <- max(length(titulos), length(links), length(info_extra), length(citacoes), length(resumos))
    titulos <- c(titulos, rep(NA, max_length - length(titulos)))
    links <- c(links, rep(NA, max_length - length(links)))
    info_extra <- c(info_extra, rep(NA, max_length - length(info_extra)))
    citacoes <- c(citacoes, rep(NA, max_length - length(citacoes)))
    resumos <- c(resumos, rep(NA, max_length - length(resumos)))

    # Criar um dataframe temporário
    df_temp <- data.frame(
      title = titulos,
      authors_info = info_extra,
      abstract = resumos,  # 🔹 Novo campo: Resumo do artigo!
      link = links,
      citations = citacoes,
      stringsAsFactors = FALSE
    )

    # Adicionar à lista
    artigos[[pagina + 1]] <- df_temp
  }

  # Combinar todas as páginas em um único DataFrame
  df_scholar <- bind_rows(artigos)

  # 🔹 **Filtrar apenas artigos que contêm os termos no título ou no resumo**
  df_scholar_filtrado <- df_scholar %>%
    filter(
      str_detect(tolower(title), "data science team|times de ciência de dados|data science team roles|teams in data science|data science teams") |
        str_detect(tolower(abstract), "data science team|times de ciência de dados|data science team roles|teams in data science|data science teams")
    )

  return(df_scholar_filtrado)
}


df_scholar <- buscar_scholar_completo('"data science team" OR "times de ciência de dados" OR "Data Science Team Roles" OR "Teams in Data Science" OR "Data Science Teams"', num_paginas = 3)

# Visualizar os primeiros artigos coletados
head(df_scholar)
