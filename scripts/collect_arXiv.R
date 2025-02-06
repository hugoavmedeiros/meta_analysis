library(aRxiv)

termos_busca <- c(
  "data science team",
  "data-science team",
  "data science teams",
  "teams in data science",
  "time de ciência de dados",
  "data science roles")

query <- paste0("ti:\"", termos_busca, "\" OR abs:\"", termos_busca, "\"", collapse = " OR ")

df_arxiv <- arxiv_search(query = query, limit = 50)

df_arxiv <- df_arxiv %>%
  mutate(
    title_clean = str_squish(tolower(title)),
    abstract_clean = str_squish(tolower(abstract)),
    found_in_title = sapply(title_clean, function(t) any(str_detect(t, termos_busca))),
    found_in_abstract = sapply(abstract_clean, function(a) any(str_detect(a, termos_busca)))
  )

df_arxiv %>% glimpse()

