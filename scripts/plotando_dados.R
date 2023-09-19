## ----message=FALSE-----------------------------------------------------------------------------------------
# Importando bibliotecas
library(tidyverse)
library(plotly)
library(scales)
library(ggsci)     # permite obter paletas de cores oficiais de revistas científicas e relacionadas à cultura de ficção ciêntífica
library(patchwork) # permite combinar gráficos do pacote ggplot2 em uma mesma figura


## ----message=FALSE-----------------------------------------------------------------------------------------
dt <- readr::read_csv('dados/populacao_brasil_pnadc_tidy.csv')
glimpse(dt)


## ----------------------------------------------------------------------------------------------------------
grupos_redundantes <- c('5 a 13 anos', '14 a 17 anos', '60 anos ou mais')
p1_a <- dt |>
        filter(!(grupo_idade %in% grupos_redundantes)) |>
        summarise(total = sum(populacao), .by = c(sexo, ano)) |>
        ggplot(aes(x = ano, y = total, fill = sexo)) +
        geom_col() +
        facet_wrap(~sexo)
p1_b <- dt |>
  filter(!(grupo_idade %in% grupos_redundantes)) |>
  summarise(total = sum(populacao), .by = c(sexo, ano)) |>
  ggplot(aes(x = ano, y = total * 10^-6, fill = sexo)) +
  geom_col() +
  facet_wrap(~sexo)
p1_a / p1_b # mostrar os plots p1_a e p1_b


## ----fig.width=10------------------------------------------------------------------------------------------
p1_a + scale_x_continuous(breaks = unique(dt$ano)) + theme_classic()


## ----fig.width=10------------------------------------------------------------------------------------------
  p1_a <- p1_a +
  scale_x_continuous(breaks = unique(dt$ano)) +
  scale_y_continuous(labels = label_comma(big.mark = '.', decimal.mark = ',')) +
  theme_classic()
p1_a


## ----fig.width=10------------------------------------------------------------------------------------------
p1_b <- p1_b +
    scale_x_continuous(breaks = unique(dt$ano)) +
    scale_y_continuous(labels = number_format(suffix = "M")) +
    theme_classic()
p1_b


## ----fig.width=10------------------------------------------------------------------------------------------
p1_a <- p1_a + ylab("População Brasileira") + theme(legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank())
p1_b  <- p1_b + ylab("População Brasileira") + theme(legend.position = "none", axis.title.x = element_blank())
p1_a / p1_b


## ----cache=TRUE--------------------------------------------------------------------------------------------
ggplotly(p1_a, width = 1000)


## ----warning=FALSE, cache=TRUE-----------------------------------------------------------------------------
dt |>
  filter(!(grupo_idade %in% grupos_redundantes)) |>
  summarise(total = sum(populacao), .by = c(sexo, ano)) |>
  plot_ly(x = ~ano, 
          y = ~total,
          color = ~sexo,
          type = 'bar') |>
  layout(yaxis = list(title = "População Brasileira"),
         xaxis = list(title = ""))


## ----cache=TRUE--------------------------------------------------------------------------------------------
p1 <- dt |>
  filter(!(grupo_idade %in% grupos_redundantes)) |>
  summarise(total = sum(populacao), .by = c(sexo, ano)) |>
  ggplot(aes(x = ano, y = total * 10^-6, color = sexo)) +
  geom_line() + 
  geom_point() +
  ylab("População Brasileira (em milhões)") +
  scale_x_continuous(breaks = unique(dt$ano)) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank(), axis.title.x = element_blank())
p1


## ----cache=TRUE, warning=FALSE-----------------------------------------------------------------------------
# ggplotly(p1)
# Ou usando apenas o plotly
dt |>
  filter(!(grupo_idade %in% grupos_redundantes)) |>
  summarise(total = sum(populacao), .by = c(sexo, ano)) |>
  plot_ly(x = ~ano, 
          y = ~total,
          color = ~sexo,
          type = "scatter",
          mode = "line+marker") |>
  layout(legend = list(orientation = "h", y = 1.1, x = .35),
         yaxis = list(title = "População Brasileira"),
         xaxis = list(title = ""))


## ----fig.width=12, cache=TRUE------------------------------------------------------------------------------
dt |>
  filter(!(grupo_idade %in% grupos_redundantes)) |>
  #filter(sexo == "Mulheres") |>
  #filter(sexo == "Homens") |>
  ggplot(aes(ano, populacao)) +
  geom_col() +
  facet_wrap(sexo ~ grupo_idade, nrow = 2) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = .9),
        strip.text.x = element_text(margin = margin(.1,.2,.1,.2, "cm")))


## ----fig.width=12, cache=TRUE------------------------------------------------------------------------------
faixa_etaria_labels = c("0 a 4 anos", "5 a 9 anos", "5 a 13 anos", "10 a 13 anos", "14 a 15 anos",
                        "14 a 17 anos", "16 a 17 anos",    "18 a 19 anos",    "20 a 24 anos",
                        "25 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos",
                        "60 a 64 anos", "60 anos ou mais", "65 anos ou mais")
p2 <- dt |>
  filter(!(grupo_idade %in% grupos_redundantes)) |>
  mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria_labels)) |>          # alternativa 1
  #mutate(grupo_idade = fct_reorder(factor(grupo_idade), populacao)) |>               # alternativa 2
  ggplot(aes(x = ano, y = populacao * 10^-6, fill = sexo)) +
  geom_col() +
  facet_grid(sexo ~ grupo_idade) +
  ylab("População Brasileira (milhões de habitantes)") +
  theme_classic()
p2


## ----fig.width=12, fig.height=8, cache=TRUE----------------------------------------------------------------
# Criando um tema com modificações de elementos gráficos
mytheme <- theme(legend.position = "top", legend.title = element_blank(),
                 legend.text = element_text(size = 12, family = 'arial', color = 'black'),
                 axis.title.y = element_text(size = 14, family = 'arial', color = 'black'),
                 axis.title.x = element_blank(),
                 axis.text.y = element_text(size = 12, family = 'arial'),
                 axis.text.x = element_text(size = 12, family = 'arial', angle = 65, vjust = .5),
                 strip.text.y = element_text(size = 10, family = 'arial', color = 'black'),
                 strip.text.x = element_text(size = 7.3, family = 'arial', color = 'black', margin = margin(.1,.4,.1,.4, "cm")))
p2 + mytheme


## ----out.width="100%", cache=TRUE--------------------------------------------------------------------------
ggplotly(p2 + mytheme + theme(legend.position = 'none'), width = 1100, height = 500)


## ----warning=FALSE, cache=TRUE-----------------------------------------------------------------------------
dt |>
    filter(!(grupo_idade %in% grupos_redundantes)) |>
    plot_ly(x = ~ano, 
            y = ~populacao,
            color = ~sexo,
            name = ~grupo_idade,
            text = ~sexo,
            type = "bar",
            width = 1100) |>
    layout(legend = list(orientation = "h", xanchor = "center", x = 0.5), # Podemos  dar orientação horizontal para a  legenda, mante-la na base da figura e centro do eixo x.
           yaxis = list(title = "População Brasileira"),
           xaxis = list(title = ""))


## ----warning=FALSE, cache=TRUE-----------------------------------------------------------------------------
dt |>
    filter(!(grupo_idade %in% grupos_redundantes)) |>
    #mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria_labels)) |>        # alternativa 1
    mutate(grupo_idade = fct_reorder(factor(grupo_idade), populacao)) |>               # alternativa 2
    group_by(grupo_idade) |>
    group_map(~ plot_ly(data=., 
                        x = ~ano, 
                        y = ~populacao, 
                        color = ~sexo, 
                        name =  ~grupo_idade,
                        text = ~sexo,
                        type = "bar", 
                        width = 1100), .keep=TRUE) |>
    subplot(nrows = 1, shareX = TRUE, shareY=TRUE, titleX = FALSE) |>
    layout(showlegend = FALSE, # Esconder legenda
           yaxis = list(title = "População Brasileira"),
           xaxis = list(title = ""))


## ----cache=TRUE--------------------------------------------------------------------------------------------
p2 <- dt |>
  filter(!(grupo_idade %in% grupos_redundantes)) |>
  mutate(grupo_idade = factor(grupo_idade, levels = faixa_etaria_labels)) |>          # alternativa 1
  ggplot(aes(x = ano, y = populacao * 10^-6, color = grupo_idade)) +
  geom_line() + geom_point() +
  facet_wrap(~sexo) +
  ylab("População Brasileira (milhões de habitantes)") +
  theme_classic() + theme(axis.title.x = element_blank())
ggplotly(p2)


## ----message=FALSE, cache=TRUE-----------------------------------------------------------------------------
dt <- readr::read_csv('dados/taxa_desocupacao_idade_2012-2013_tidy.csv')
glimpse(dt)


## ----fig.width=12------------------------------------------------------------------------------------------
dt |>
  ggplot(aes(x = trimestre, y = taxa_desocupacao, group = faixa_etaria, color = faixa_etaria)) +
  geom_line() +
  ylab("Taxa de Desocupação \n População Economicamente Ativa (%)") +
  scale_color_uchicago() +
  theme_classic() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), axis.title.x = element_blank())


## ----fig.width=12------------------------------------------------------------------------------------------
str_sub(dt$trimestre[1:3], nchar(dt$trimestre[1:3]) -3, nchar(dt$trimestre[1:3]))
p3 <- dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) - 3, nchar(trimestre)))) |>
  mutate(trimestre = fct_reorder(factor(trimestre), ano)) |>
  ggplot(aes(x = trimestre, y = taxa_desocupacao, group = faixa_etaria, color = faixa_etaria)) +
  geom_line() +
  ylab("Taxa de Desocupação \n População Economicamente Ativa (%)") +
  scale_color_uchicago() +
  theme_classic() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), axis.title.x = element_blank())
p3


## ----------------------------------------------------------------------------------------------------------
ggplotly(p3,  width = 1100, cache=TRUE)


## ----cache=TRUE--------------------------------------------------------------------------------------------
dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) -3, nchar(trimestre)))) |>
  mutate(trimestre = fct_reorder(factor(trimestre), ano)) |>
  plot_ly(x = ~trimestre,
          y = ~taxa_desocupacao,
          color = ~faixa_etaria, 
          type = "scatter", 
          mode = "lines+markers",
          width = 1100) |>
  layout(
    legend = list(orientation = "h", yanchor = "center", y = 1.1),
    yaxis = list(title = "Taxa de Desocupação \n População Economicamente Ativa (%)"),
    xaxis = list(title = ""))


## ----cache=TRUE--------------------------------------------------------------------------------------------
dt |>
  mutate(ano = as.numeric(str_sub(trimestre, nchar(trimestre) -3, nchar(trimestre)))) |>
  summarise(taxa_desocupacao = mean(taxa_desocupacao), .by = c(ano, faixa_etaria)) |>
  plot_ly(x = ~ano,
          y = ~taxa_desocupacao,
          color = ~faixa_etaria,
          type = "scatter",
          mode = "lines+markers",
          width = 1000) |>
  layout( legend = list(orientation = "h", xanchor = "center", x = .5),
          yaxis = list(title = "Taxa de Desocupação \n População Economicamente Ativa (%)"),
          xaxis = list(title = ""))

