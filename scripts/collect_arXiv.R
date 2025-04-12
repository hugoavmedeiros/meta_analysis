library(aRxiv)
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)

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

# termos_busca <- c(
#   "data science team",
#   "data-science team",
#   "data science teams",
#   "teams in data science",
#   "time de ciência de dados",
#   "data science roles")

termos_busca <- c(
  "Gestão do Investimento Público",
  "Gestão dos Investimentos Públicos",
  "Public Investment Management")

query <- paste0("ti:\"", termos_busca, "\" OR abs:\"", termos_busca, "\"", collapse = " OR ")

df_arxiv <- arxiv_search(query = query, limit = 200)

df_arxiv <- df_arxiv %>%
  mutate(
    title_clean = str_squish(tolower(title)),
    abstract_clean = str_squish(tolower(abstract)),
    found_in_title = sapply(title_clean, function(t) any(str_detect(t, termos_busca))),
    found_in_abstract = sapply(abstract_clean, function(a) any(str_detect(a, termos_busca))),

    citations_semantic = sapply(id, buscar_citacoes_semantic),
    citations_crossref = sapply(doi, buscar_citacoes_crossref),

  )

df_arxiv %>% glimpse()

df_arxiv %>% write.csv2(
  'data/df_dsteam_arxiv.csv'
)

#
# query <- paste0("ti:\"", termos_busca, "\" OR abs:\"", termos_busca, "\"", collapse = " OR ")
#
# df_arxiv <- arxiv_search(query = query, limit = 200)
#
# df_arxiv <- df_arxiv %>%
#   mutate(
#     title_clean = str_squish(tolower(title)),
#     abstract_clean = str_squish(tolower(abstract)),
#     found_in_title = sapply(title_clean, function(t) any(str_detect(t, termos_busca))),
#     found_in_abstract = sapply(abstract_clean, function(a) any(str_detect(a, termos_busca))),
#
#     citations_semantic = sapply(id, buscar_citacoes_arXiv_semantic),
#     citations_crossref = sapply(doi, buscar_citacoes_arXiv_crossref),
#
#   )
#
# df_arxiv %>% glimpse()
#
# df_arxiv %>% write.csv2(
#   'data/df_dsteam_arxiv.csv'
# )

