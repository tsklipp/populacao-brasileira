## ----message=FALSE--------------------------------------------------------------------------------------------------------
# Importando bibliotecas
library(tidyverse)
library(plotly)
library(scales)
library(ggsci)


## ---- message=FALSE-------------------------------------------------------------------------------------------------------
dt <- readr::read_csv('dados/populacao_brasil_pnadc_tidy.csv')


## -------------------------------------------------------------------------------------------------------------------------
p1_a <- dt |>
        filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
        summarise(total = sum(populacao), .by = c(sexo, ano)) |>
        ggplot(aes(x = ano, y = total, fill = sexo)) +
        geom_col() +
        facet_wrap(~sexo)
p1_b <- dt |>
  filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  summarise(total = sum(populacao), .by = c(sexo, ano)) |>
  ggplot(aes(x = ano, y = total / 10^6, fill = sexo)) +
  geom_col() +
  facet_wrap(~sexo)
p1_a


## -------------------------------------------------------------------------------------------------------------------------
p1_a + scale_x_continuous(breaks = unique(dt$ano)) + theme_classic()


## -------------------------------------------------------------------------------------------------------------------------
p1_a <- p1_a +
  scale_x_continuous(breaks = unique(dt$ano)) +
  scale_y_continuous(labels = label_comma(big.mark = '.', decimal.mark = ',')) +
  theme_classic()
p1_a


## -------------------------------------------------------------------------------------------------------------------------
p1_b <- p1_b +
    scale_x_continuous(breaks = unique(dt$ano)) +
    scale_y_continuous(labels = number_format(suffix = "M")) +
    theme_classic()
p1_b


## -------------------------------------------------------------------------------------------------------------------------
p1 <- p1_a + ylab("População Brasileira") + theme(axis.title.x = element_blank())
p1


## -------------------------------------------------------------------------------------------------------------------------
ggplotly(p1)


## ---- warning=FALSE-------------------------------------------------------------------------------------------------------
dt |>
  filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  summarise(total = sum(populacao), .by = c(sexo, ano)) |>
  plot_ly(x = ~ano, 
          y = ~total,
          color = ~sexo,
          type = 'bar') |>
  layout(yaxis = list(title = "População Brasileira"),
         xaxis = list(title = ""))
# group_by(sexo) |>
# group_map(~ plot_ly(data=., x = ~ano, y = ~total, color = ~sexo, type = "bar"), .keep=TRUE) |>
# subplot(nrows = 1, shareX = TRUE, shareY=TRUE)


## -------------------------------------------------------------------------------------------------------------------------
dt |>
  filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  filter(sexo == "Mulheres") |>
  #filter(sexo == "Homens") |>
  ggplot(aes(ano, populacao)) +
  geom_col() +
  facet_wrap(sexo ~ grupo_idade, nrow = 2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = .9),
        strip.text.x = element_text(margin = margin(.4,.2,.4,.2, "cm")))


## -------------------------------------------------------------------------------------------------------------------------
faixa_etaria_labels = c("0 a 4 anos", "5 a 9 anos", "5 a 13 anos", "10 a 13 anos", "14 a 15 anos",
                        "14 a 17 anos", "16 a 17 anos",    "18 a 19 anos",    "20 a 24 anos",
                        "25 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos",
                        "60 a 64 anos", "60 anos ou mais", "65 anos ou mais")
p2 <- dt |>
  filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
  mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria_labels)) |>          # alternativa 1
  #mutate(grupo_idade = fct_reorder(factor(grupo_idade), populacao)) |>               # alternativa 2
  ggplot(aes(x = ano, y = populacao * 10^-6, fill = sexo)) +
  geom_col() +
  facet_grid(sexo ~ grupo_idade) +
  ylab("População Brasileira (milhões de habitantes)") +
  theme_classic() +
  theme(legend.position = "top", 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 65, vjust = .5),
        strip.text.x = element_text(margin = margin(.1,.5,.1,.5, "cm")))
p2


## -------------------------------------------------------------------------------------------------------------------------
ggplotly(p2)


## -------------------------------------------------------------------------------------------------------------------------
dt |>
    filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
    mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria_labels)) |>          # alternativa 1
    #mutate(grupo_idade = fct_reorder(factor(grupo_idade), populacao)) |>               # alternativa 2
    group_by(sexo, grupo_idade) |>
    plot_ly(x = ~ano, 
            y = ~populacao,
            color = ~sexo,
            split = ~grupo_idade,
            type = "bar") |>
    layout(yaxis = list(title = "População Brasileira"),
           xaxis = list(title = ""))


## ---- warning=FALSE-------------------------------------------------------------------------------------------------------
dt |>
    filter(!(grupo_idade %in% c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais'))) |>
    #mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria_labels)) |>        # alternativa 1
    mutate(grupo_idade = fct_reorder(factor(grupo_idade), populacao)) |>               # alternativa 2
    group_by(grupo_idade) |>
    group_map(~ plot_ly(data=., 
                        x = ~ano, 
                        y = ~populacao, 
                        color = ~sexo, 
                        split = ~sexo, 
                        name = ~grupo_idade,
                        type = "bar"), .keep=TRUE) |>
    subplot(nrows = 1, shareX = TRUE, shareY=TRUE, titleX = FALSE) |>
    layout(yaxis = list(title = "População Brasileira"))


## ---- message=FALSE-------------------------------------------------------------------------------------------------------
dt <- readr::read_csv('dados/taxa_desocupacao_idade_2012-2013_tidy.csv')


## -------------------------------------------------------------------------------------------------------------------------
p3 <- dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) -3, nchar(trimestre)))) |>
  mutate(trimestre = fct_reorder(factor(trimestre), ano)) |>
  ggplot(aes(x = trimestre, y = taxa_desocupacao, group = faixa_etaria, color = faixa_etaria)) +
  geom_line() +
  ylab("Taxa de Desocupação \n População Economicamente Ativa (%)") +
  scale_color_uchicago() +
  theme_classic() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), axis.title.x = element_blank())
p3


## -------------------------------------------------------------------------------------------------------------------------
ggplotly(p3)


## -------------------------------------------------------------------------------------------------------------------------
dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) -3, nchar(trimestre)))) |>
  mutate(trimestre = fct_reorder(factor(trimestre), ano)) |>
  plot_ly(x = ~trimestre, y = ~taxa_desocupacao, color = ~faixa_etaria, type = "scatter", mode = "lines+markers") |>
  layout(
    yaxis = list(title = 'Taxa de Desocupação \n População Economicamente Ativa (%)"'),
    xaxis = list(title = '')
  )


## -------------------------------------------------------------------------------------------------------------------------
dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) -3, nchar(trimestre)))) |>
  summarise(taxa_desocupacao = mean(taxa_desocupacao), .by = c(ano, faixa_etaria)) |>
  plot_ly(x = ~ano,
          y = ~taxa_desocupacao,
          color = ~faixa_etaria,
          type = "scatter",
          mode = "lines+markers") |>
  layout(yaxis = list(title = "Taxa de Desocupação \n População Economicamente Ativa (%)"),
         xaxis = list(title = ""))

