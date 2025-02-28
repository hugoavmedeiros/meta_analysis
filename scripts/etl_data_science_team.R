library(aRxiv)      # Pesquisa no arXiv
library(dplyr)      # Manipulação de dados
library(easyPubMed) # Busca no PubMed
library(httr)       # Requisições HTTP
library(jsonlite)   # Manipulação de JSON
library(purrr)      # Programação funcional
library(rvest)      # Scrape de dados
library(stringr)    # Manipulação de strings
library(tidytext)
library(tidyr)
library(xml2)       # Manipulação de XML

source('helpers/utils.R')

# 🔹 Definir termos de busca
termos_busca <- c(
  "data science role",
  "data-science role",
  "data science roles",
  "data-science roles",
  "data science team",
  "data-science team",
  "data science teams",
  "data-science teams",
  "data science worker",
  "data-science worker",
  "data science workers",
  "data-science workers",
  "teams in data science",
  "time de ciência de dados"
)

### 🔍 arXiv ----
query_arxiv <- paste0("ti:\"", termos_busca, "\" OR abs:\"", termos_busca, "\"", collapse = " OR ")

df_arxiv <- arxiv_search(query = query_arxiv, limit = 300) %>%
  mutate(
    title_clean = str_squish(tolower(title)),
    abstract_clean = str_squish(tolower(abstract)),
    keywords_extracted = map_chr(paste(title_clean, abstract_clean), extrair_keywords_frequencia),
    # found_in_title = map_lgl(title_clean, ~ any(str_detect(.x, termos_busca))),
    # found_in_abstract = map_lgl(abstract_clean, ~ any(str_detect(.x, termos_busca))),
    found_in_title = map_int(title_clean, ~ sum(str_count(.x, termos_busca))),
    found_in_abstract = map_int(abstract_clean, ~ sum(str_count(.x, termos_busca))),
    citations_semantic = map_int(id, buscar_citacoes_arXiv_semantic),
    citations_crossref = map_int(doi, buscar_citacoes_arXiv_crossref)
  )

df_arxiv %>% write.csv2('data/df_dsteam_arxiv.csv')

### 🔍 PubMed ----
query_pubmed <- paste0("\"", termos_busca, "\"[Title/Abstract]", collapse = " OR ")
lista_artigos <- get_pubmed_ids(query_pubmed)
artigos_pubmed <- fetch_pubmed_data(lista_artigos, format = "xml")
xml_data <- read_xml(artigos_pubmed)

df_pubmed <- map_df(xml_find_all(xml_data, ".//PubmedArticle"), function(artigo) {
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
    citations_pmc = buscar_citacoes_pubmed_pmc(pmid),
    citations_crossref = buscar_citacoes_pubmed_crossref(doi)
  )
}) %>%
  mutate(
    title_clean = str_squish(tolower(title)),
    abstract_clean = str_squish(tolower(abstract)),
    keywords_clean = str_squish(tolower(keywords)),
    found_in_title = map_int(title_clean, ~ sum(str_count(.x, termos_busca))),
    found_in_abstract = map_int(abstract_clean, ~ sum(str_count(.x, termos_busca))),
    # found_in_title = map_lgl(title_clean, ~ any(str_detect(.x, termos_busca))),
    # found_in_abstract = map_lgl(abstract_clean, ~ any(str_detect(.x, termos_busca))),
    found_in_keywords = map_lgl(keywords_clean, ~ any(str_detect(.x, termos_busca))),
    keywords_extracted = map_chr(paste(title_clean, abstract_clean), extrair_keywords_frequencia)
  )

df_pubmed %>% write.csv2('data/df_dsteam_pubmed.csv')

### 🔍 SciELO ----
query_scielo <- paste0('"', termos_busca, '"', collapse = " OR ")

#query_scielo <- '"data science team" OR "data-science" OR "data science"'

df_scielo <- buscar_scielo(query_scielo, num_paginas = 3)

df_scielo %>% write.csv2('data/df_dsteam_scielo.csv')

##### Pop ----
df_PoP_google <- read.csv2(
  'data/PoP_google - ds_team.csv',
  sep = ',')

#### tratamento ----
##### juntar bases ----
df_meta_data_science_team <-
  df_arxiv %>%
  mutate(
    fonte = 'arXiv'
  ) %>%
  select(
    fonte, title, abstract_clean, found_in_title, found_in_abstract,
    keywords_extracted, title_clean
  ) %>%
  rbind(
    df_pubmed %>%
      mutate(
        fonte = 'pubmed'
      ) %>%
      select(
        fonte, title, abstract_clean, found_in_title, found_in_abstract,
        keywords_extracted, title_clean
      )
  ) %>%
  distinct(
    title_clean, .keep_all = TRUE
  )

df_meta_data_science_team %>%
  writexl::write_xlsx('data/df_meta_data_science_team.xlsx')
