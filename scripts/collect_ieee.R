
library(httr)
library(jsonlite)
library(dplyr)

ieee_key <- config::get("ieee_key")

buscar_ieee_api <- function(query, api_key, num_resultados = 50) {
  url <- paste0("https://ieeexploreapi.ieee.org/api/v1/search/articles?querytext=",
                URLencode(query),
                "&apikey=", api_key,
                "&max_records=", num_resultados)

  resposta <- GET(url, add_headers("Accept" = "application/json"))

  if (http_status(resposta)$category != "Success") {
    stop("Erro ao acessar a API do IEEE Xplore")
  }

  conteudo <- content(resposta, as = "text", encoding = "UTF-8")
  dados_json <- fromJSON(conteudo, flatten = TRUE)

  df_ieee <- dados_json$articles %>%
    select(title, abstract, authors, publication_title, publication_year, doi)

  return(df_ieee)
}

# 🔹 Testando a função (substitua "SUA_CHAVE_AQUI" pela API Key do IEEE)
api_key <- ieee_key

df_ieee_api <- buscar_ieee_api("IEEE International Conference on Big Data", api_key)

# 🔹 Visualizar os primeiros artigos coletados
head(df_ieee_api)
