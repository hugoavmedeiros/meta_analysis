
library(dplyr)
library(easyPubMed)
library(httr)
library(jsonlite)
library(purrr)
library(stringr)
library(xml2)

termos_busca <- c(
  "data science team",
  "data-science team",
  "data science teams",
  "teams in data science",
  "time de ciência de dados",
  "data science roles"
)

# Criar query automaticamente para incluir todos os termos no título e resumo
query <- paste0("\"", termos_busca, "\"[Title/Abstract]", collapse = " OR ")

# Buscar artigos
lista_artigos <- get_pubmed_ids(query)

# Baixar os artigos em formato PubMed XML
artigos_pubmed <- fetch_pubmed_data(lista_artigos, format = "xml")

xml_data <- read_xml(artigos_pubmed)

# Encontrar todos os artigos no XML
artigos <- xml_find_all(xml_data, ".//PubmedArticle")

# Criar um DataFrame com os dados extraídos
# df_pubmed <- map_df(artigos, function(artigo) {
#   tibble(
#     pmid = xml_text(xml_find_first(artigo, ".//PMID")),
#     title = xml_text(xml_find_first(artigo, ".//ArticleTitle")),
#
#     # Extraindo todas as partes do abstract e concatenando
#     abstract = paste(xml_text(xml_find_all(artigo, ".//AbstractText")), collapse = " "),
#
#     journal = xml_text(xml_find_first(artigo, ".//Journal/Title")),
#     year = xml_text(xml_find_first(artigo, ".//PubDate/Year")),
#
#     # 🔹 Extraindo autores
#     authors = paste(xml_text(xml_find_all(artigo, ".//AuthorList//LastName")), collapse = ", "),
#
#     # 🔹 Extraindo DOI (se existir)
#     doi = xml_text(xml_find_first(artigo, ".//ArticleId[@IdType='doi']")),
#
#     # 🔹 Extraindo palavras-chave
#     keywords = paste(xml_text(xml_find_all(artigo, ".//Keyword")), collapse = ", "),
#
#     # 🔹 Extraindo tipo de publicação
#     publication_type = paste(xml_text(xml_find_all(artigo, ".//PublicationType")), collapse = ", ")
#   )
# })

buscar_citacoes_pmc <- function(pmid) {
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
buscar_citacoes_crossref <- function(doi) {
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

# 🔹 Criar um DataFrame com os dados extraídos
df_pubmed <- map_df(artigos, function(artigo) {
  pmid <- xml_text(xml_find_first(artigo, ".//PMID"))
  doi <- xml_text(xml_find_first(artigo, ".//ArticleId[@IdType='doi']"))

  tibble(
    pmid = ifelse(pmid == "", NA, pmid),
    title = xml_text(xml_find_first(artigo, ".//ArticleTitle")),
    abstract = paste(xml_text(xml_find_all(artigo, ".//AbstractText")), collapse = " "),
    journal = xml_text(xml_find_first(artigo, ".//Journal/Title")),
    year = xml_text(xml_find_first(artigo, ".//PubDate/Year")),
    authors = paste(xml_text(xml_find_all(artigo, ".//AuthorList//LastName")), collapse = ", "),
    doi = ifelse(doi == "", NA, doi),
    keywords = paste(xml_text(xml_find_all(artigo, ".//Keyword")), collapse = ", "),
    publication_type = paste(xml_text(xml_find_all(artigo, ".//PublicationType")), collapse = ", "),

    # 🔹 Buscar número de citações no PubMed Central (PMC) e CrossRef
    citations_pmc = buscar_citacoes_pmc(pmid),
    citations_crossref = buscar_citacoes_crossref(doi)
  )
})

# 🔹 Verificar onde os termos estão realmente aparecendo
df_pubmed <- df_pubmed %>%
  mutate(
    title_clean = str_squish(tolower(title)),
    abstract_clean = str_squish(tolower(abstract)),
    keywords_clean = str_squish(tolower(keywords)),

    found_in_title = sapply(title_clean, function(t) any(str_detect(t, termos_busca))),
    found_in_abstract = sapply(abstract_clean, function(a) any(str_detect(a, termos_busca))),
    found_in_keywords = sapply(keywords_clean, function(k) any(str_detect(k, termos_busca)))
  )

# Salvar em arquivo
df_pubmed %>% write.csv2(
  'data/df_dsteam_pubmed.csv'
)
