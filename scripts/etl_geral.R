
library(dplyr)
library(data.table)

df_dsteam_google <- fread(
  'data/PoP_google - ds_team.csv'
)

df_dsteam_crossref <- fread(
  'data/PoP_crossref - ds_team.csv'
)

df_dsteam_semantic <- fread(
  'data/PoP_semantic - ds_team.csv'
)

df_dsteam_pubmed <- fread(
  'data/df_dsteam_pubmed.csv'
)

df_dsteam_arxiv <- fread(
  'data/df_dsteam_arxiv.csv'
)


