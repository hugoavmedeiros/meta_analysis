
library(dplyr)
library(easyPubMed)
library(purrr)
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
df_pubmed <- map_df(artigos, function(artigo) {
  tibble(
    pmid = xml_text(xml_find_first(artigo, ".//PMID")),
    title = xml_text(xml_find_first(artigo, ".//ArticleTitle")),

    # Extraindo todas as partes do abstract e concatenando
    abstract = paste(xml_text(xml_find_all(artigo, ".//AbstractText")), collapse = " "),

    journal = xml_text(xml_find_first(artigo, ".//Journal/Title")),
    year = xml_text(xml_find_first(artigo, ".//PubDate/Year")),

    # 🔹 Extraindo autores
    authors = paste(xml_text(xml_find_all(artigo, ".//AuthorList//LastName")), collapse = ", "),

    # 🔹 Extraindo DOI (se existir)
    doi = xml_text(xml_find_first(artigo, ".//ArticleId[@IdType='doi']")),

    # 🔹 Extraindo palavras-chave
    keywords = paste(xml_text(xml_find_all(artigo, ".//Keyword")), collapse = ", "),

    # 🔹 Extraindo tipo de publicação
    publication_type = paste(xml_text(xml_find_all(artigo, ".//PublicationType")), collapse = ", ")
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
write(artigos_pubmed, file = "data/pubmed_data.xml")
