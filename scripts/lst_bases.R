
library(htmltools)
library(reactable)
library(readr)

# 🔹 Importar o CSV
df_bases <- read_csv2("data/bases_pesquisa.csv")

# 🔹 Criar função para exibir ícones ✅ e ❌ junto ao texto
pop_icons <- function(value) {
  if (value == "Sim") {
    return(HTML("<span style='color: green; font-size: 16px;'>✅ Sim</span>"))
  } else {
    return(HTML("<span style='color: red; font-size: 16px;'>❌ Não</span>"))
  }
}

# 🔹 Criar função para formatar pacotes R como badges (mantendo "Não existe" em cinza)
badge_pkg <- function(value) {
  if (value != "Não existe") {
    return(HTML(paste0("<span style='background-color: #0073e6; color: white; padding: 4px 8px;
                         border-radius: 12px; font-size: 12px; font-weight: bold;'>", value, "</span>")))
  } else {
    return(HTML("<span style='color: gray;'>Não existe</span>"))
  }
}

# 🔹 Criar tabela interativa personalizada
reactable(df_bases,
          searchable = TRUE,  # Permite busca na tabela
          striped = TRUE,  # Linhas zebradas para melhor visualização
          highlight = TRUE,  # Destaca linha ao passar o mouse
          defaultSorted = list(NOME = "asc"),  # Ordena automaticamente por nome
          defaultPageSize = 20,  # 🔹 Mostrar 20 linhas por página
          theme = reactableTheme(
            headerStyle = list(backgroundColor = "#f2f2f2", fontWeight = "bold")  # 🔹 Cabeçalhos em cinza
          ),
          columns = list(
            PoP = colDef(html = TRUE, cell = pop_icons, align = "center"),
            Pacote_R = colDef(html = TRUE, cell = badge_pkg, align = "center")
          ))
