
library(dplyr)
library(easyPubMed)
library(purrr)
library(xml2)

# Criar uma query de busca para PubMed
query <- '"data science team"[Title/Abstract] OR "time de ciência de dados"[Title/Abstract] OR "Data Science Team Roles"[Title/Abstract] OR "Teams in Data Science"[Title/Abstract] OR "Data Science Teams"[Title/Abstract]'

# Buscar artigos
lista_artigos <- get_pubmed_ids(query)

# Baixar os artigos em formato PubMed XML
artigos_pubmed <- fetch_pubmed_data(lista_artigos, format = "xml")

# Salvar em arquivo
write(artigos_pubmed, file = "data/pubmed_data.xml")

xml_data <- read_xml(artigos_pubmed)

# Encontrar todos os artigos no XML
artigos <- xml_find_all(xml_data, ".//PubmedArticle")

# Criar um DataFrame com os dados extraídos
df_artigos <- map_df(artigos, function(artigo) {
  tibble(
    pmid = xml_text(xml_find_first(artigo, ".//PMID")),
    title = xml_text(xml_find_first(artigo, ".//ArticleTitle")),
    abstract = xml_text(xml_find_first(artigo, ".//AbstractText")),
    journal = xml_text(xml_find_first(artigo, ".//Journal/Title")),
    year = xml_text(xml_find_first(artigo, ".//PubDate/Year"))
  )
})

# Visualizar os primeiros artigos
head(df_artigos)
