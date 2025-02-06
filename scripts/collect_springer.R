library(httr)
library(jsonlite)
library(dplyr)

springer_key <- config::get("springer_key")

buscar_springer <- function(query, api_key, num_resultados = 50) {
  # 🔹 Ajustando a query para Springer API (removendo aspas simples e formatando corretamente)
  query_formatada <- gsub("'", "", query)  # Remove aspas simples
  query_formatada <- gsub(" OR ", "+OR+", query_formatada)  # Substitui "OR" por "+OR+"

  url <- paste0("http://api.springernature.com/meta/v2/json?q=",
                URLencode(query_formatada), "&api_key=", api_key, "&p=", num_resultados)

  # 🔹 Fazer a requisição HTTP
  resposta <- GET(url, add_headers("Accept" = "application/json"))

  # 🔹 Verificar se a resposta foi bem-sucedida
  if (http_status(resposta)$category != "Success") {
    stop("Erro ao acessar a API do Springer. Verifique sua chave de API ou tente novamente mais tarde.")
  }

  # 🔹 Converter JSON para DataFrame
  conteudo <- content(resposta, as = "text", encoding = "UTF-8")

  # Verifica se o JSON retornado está vazio ou tem erro
  if (nchar(conteudo) == 0) {
    stop("Erro: A resposta da API está vazia. Verifique a query.")
  }

  dados_json <- fromJSON(conteudo, flatten = TRUE)

  if (!"records" %in% names(dados_json)) {
    stop("Erro: A API não retornou dados válidos. Verifique se sua chave está correta e se a query está formatada corretamente.")
  }

  df_springer <- dados_json$records %>%
    select(title, abstract, journal = publicationName, year = publicationDate)

  return(df_springer)
}

# 🔹 Testar a função (substitua "SUA_CHAVE_AQUI" pela API Key do Springer)
api_key <- springer_key

df_springer <- buscar_springer("data science team OR 'times de ciência de dados'", api_key)

head(df_springer)
