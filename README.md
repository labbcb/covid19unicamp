---
title: "Modelos Preditivos para COVID-19"
author: "COVID-19 FT - UNICAMP"
date: "3/31/2020"
output:
  html_document:
    keep_md: yes
---



## Obtenção de Dados

Afim de simplificar o acesso aos dados, utilizou-se o pacote `coronabr`, que importa diariamente os dados compilados pela iniciativa [Brasil IO](http://www.brasil.io). Pode ser o caso de se considerar a redução de dependências e acessar os dados diretamente da plataforma supracitada, entretanto há facilidades implementadas no `coronabr` que podem ser úteis. Para que colaboradores que não utilizam o R como plataforma analítica, o arquivo `covid19_cidades.csv` possui os dados (por cidade) disponíveis hoje (Tue Mar 31 11:10:07 2020).


```r
library(coronabr)
library(tidyverse)
covid19_cidades = get_corona_br()
write_csv(covid19_cidades, "covid19_cidades.csv")
unlink("output", recursive=TRUE)
```

## Manipulação de dados

Abaixo, são apresentados dados da cidade de São Paulo, apenas por esta ter sido a cidade onde foi identificado o paciente zero do Brasil e haver um histórico maior de informações, que podem ser úteis para a modelagem de casos e óbitos.

#### TODO

  * Observar ocorrências de zeros na coluna `confirmed` (vide 24/03/2020)


```r
casos_sp = covid19_cidades %>% filter(city == "São Paulo") %>% 
  select(-state, -place_type, -is_last, -city_ibge_code)
casos_sp %>% head() %>% knitr::kable("markdown")
```



|date       |city      | confirmed| deaths| estimated_population_2019| confirmed_per_100k_inhabitants| death_rate|
|:----------|:---------|---------:|------:|-------------------------:|------------------------------:|----------:|
|2020-03-30 |São Paulo |      1233|    103|                  12252023|                       10.06364|     0.0835|
|2020-03-27 |São Paulo |      1044|     62|                  12252023|                        8.52104|     0.0594|
|2020-03-26 |São Paulo |       899|     53|                  12252023|                        7.33756|     0.0590|
|2020-03-25 |São Paulo |       722|     44|                  12252023|                        5.89290|     0.0609|
|2020-03-24 |São Paulo |         0|     36|                  12252023|                             NA|         NA|
|2020-03-20 |São Paulo |       306|      9|                  12252023|                        2.49755|     0.0294|

```r
casos_sp %>% select(date, confirmed, deaths) %>% gather(type, counts, -date) %>% 
  ggplot(aes(date, counts, colour=type)) + geom_point() + geom_line() + scale_y_log10() +
  theme_bw() + xlab("Data") + ylab("Contagem")
```

![](README_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Contato

Para maiores informações, utilize a plataforma [https://covid.ic.unicamp.br/](https://covid.ic.unicamp.br/). Em particular, procuramos documentar informações desta inciativa no seu respectivo tópico em Modelagem e Epidemiologia ([https://covid.ic.unicamp.br/c/tecnolC3B3gicas/ModelagemeEpidemiologia/11](https://covid.ic.unicamp.br/c/tecnolC3B3gicas/ModelagemeEpidemiologia/11)).
