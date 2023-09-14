# Importando bibliotecas do tidyverse
library(tidyverse)

# Lendo série histórica referente à taxa de desocupação da população brasileira por grupo de idade baixados do site do IBGE:
# https://www.ibge.gov.br/estatisticas/sociais/trabalho/17270-pnad-continua.html?=&t=series-historicas
# Fonte: PNAD Continua do IBGE
dt <- readxl::read_excel("dados/taxa_desocupacao_idade_2012-2013.xlsx", skip = 1)

# Iniciaremos a limpeza dos dados com dois passos:
#   1) Removendo 2 colunas com dplyr::select()  e;
#   2) Removendo NA values (valores ausentes, não disponíveis) com tidyr::drop_na().
#   Obs: o passo dois também pode obtido selecionadno linhas específicas com dplyr::slice(dt, c(2:7))
dt <- dt |>
        dplyr::select(-c(p,...2)) |>
        tidyr::drop_na()

# Nomeando a primeira coluna dos dados
names(dt)[1] <- "faixa_etaria"

# Arrumando os dados para formato tidy com tidyr::pivot_longer() conforme:
# - alocar os nomes de todas as colunas referentes aos trimestres para uma única coluna com nome 'trimestre'
# - alocar os valores de taxa de desocupação correspondentes para uma única coluna com nome 'taxa_desocupacao'
dt <- dt |>
        tidyr::pivot_longer(cols = `1º trimestre 2012`:`1º trimestre 2023`,
                            names_to = "trimestre", values_to = "taxa_desocupacao")

# Converter a classe da coluna taxa_desocupacao de 'character' para 'numeric' com dplyr::mutate() e as.numeric(),
# porém, é necessário substituir nos valores o caractere vírgula por ponto para que a conversão seja possível
# usando stringr::str_replace()
dt <- dt |>
  dplyr::mutate(taxa_desocupacao = as.numeric(stringr::str_replace(taxa_desocupacao, ',', '.')))

# Salvar o novo data frame, agora com dados já no formato tidy, para posterior uso
readr::write_csv(dt, "dados/taxa_desocupacao_idade_2012-2013_tidy.csv")
