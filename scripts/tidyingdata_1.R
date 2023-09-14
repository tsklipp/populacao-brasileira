# Importando bibliotecas do tidyverse
library(tidyverse)

# Lendo dados da população brasileira por grupo de idade baixados através do pacote 'basedosdados'
# Fonte: PNAD Continua do IBGE
dt <- readr::read_csv("dados/populacao_brasil_pnadc.csv")

# Iniciaremos a limpeza dos dados usando dplyr::filter()
populacao_total <- dt |> dplyr::filter(sexo == 'Total' | grupo_idade == 'Total')
dt <- dt |>
  dplyr::filter(sexo != 'Total' & grupo_idade != 'Total')

# Salvar o novo data frame, agora com dados já no formato tidy, para posterior uso
readr::write_csv(dt, "dados/populacao_brasil_pnadc_tidy.csv")

# Algumas operações de sumarização sobre os dados. Perceba que existe redundância no número de habitantes
# entre os níveis ddos grupos de idade, portanto, filtraremos os grupos para evitar sumarizações inconsistentes
summary_1 <- dt |>
              dplyr::filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
              dplyr::summarise(populacao = sum(populacao), .by = c(sexo, ano))

summary_2 <- dt |>
              dplyr::filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
              dplyr::summarise(minimo = min(populacao), media = mean(populacao), maximo = max(populacao), desvio_padrao = sd(populacao), .by = c(grupo_idade))

# Vamos comparar as sumarizações em 'summary_1' com as que excluímos dos dados ('populacao_total') através da junção
# desses conjuntos de dados por meio de dplyr::left_join
dplyr::left_join(summary_1, populacao_total, suffix = c("_total_summary_1", "_total_antigo") , by = c("ano", "sexo")) |>
  select(-grupo_idade)
