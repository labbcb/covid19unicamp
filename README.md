---
title: "Modelos Preditivos para COVID-19"
author: "COVID-19 FT - UNICAMP"
date: "3/31/2020"
output:
  html_document:
    keep_md: yes
---



## Obtenção de Dados

Afim de simplificar o acesso aos dados, utilizou-se o pacote [`datacovidbr`](https://github.com/Freguglia/datacovidbr), que importa diariamente os dados compilados pela iniciativa [Brasil IO](http://www.brasil.io), sem maiores dependências. Para que colaboradores que não utilizam o R como plataforma analítica, o arquivo `dados/covid19_cidades.csv` possui os dados (por cidade) disponíveis hoje (Sat Apr  4 02:21:42 2020).


```r
library(datacovidbr)
library(tidyverse)
library(lubridate)
datapath = "dados"
covid19_cidades = brasilio() %>% filter(place_type == 'city')
write_csv(covid19_cidades, file.path(datapath, "covid19_cidades.csv"))
write_csv(covid19_cidades, file.path(datapath, paste0("covid19_cidades-", today(), ".csv")))
save(covid19_cidades, file=file.path(datapath, "covid19_cidades.rda"))
save(covid19_cidades, file=file.path(datapath, paste0("covid19_cidades-", today(), ".rda")))
```

## Manipulação de dados

Abaixo, são apresentados dados da cidade de São Paulo, apenas por esta ter sido a cidade onde foi identificado o paciente zero do Brasil e haver um histórico maior de informações, que podem ser úteis para a modelagem de casos e óbitos.

#### TODO

  * Observar ocorrências de zeros na coluna `confirmed` (vide 24/03/2020)


```r
casos_sp = covid19_cidades %>% filter(city == "São Paulo") %>% 
  select(-state, -place_type, -is_last, -city_ibge_code)
casos_sp = casos_sp %>% mutate(confirmed=case_when(
  date == as.Date("2020-03-24") ~ as.integer(717),
  TRUE ~ confirmed))
casos_sp %>% head() %>% knitr::kable("markdown")
```



|date       |city      | confirmed| deaths| estimated_population_2019| confirmed_per_100k_inhabitants| death_rate|
|:----------|:---------|---------:|------:|-------------------------:|------------------------------:|----------:|
|2020-04-03 |São Paulo |      3202|    186|                  12252023|                       26.13446|     0.0581|
|2020-04-02 |São Paulo |      2815|    164|                  12252023|                       22.97580|     0.0583|
|2020-04-01 |São Paulo |      2418|    144|                  12252023|                       19.73552|     0.0596|
|2020-03-31 |São Paulo |      1885|    121|                  12252023|                       15.38521|     0.0642|
|2020-03-30 |São Paulo |      1233|    103|                  12252023|                       10.06364|     0.0835|
|2020-03-27 |São Paulo |      1044|     62|                  12252023|                        8.52104|     0.0594|

```r
casos_sp %>% select(date, confirmed, deaths) %>% gather(type, counts, -date) %>% 
  ggplot(aes(date, counts, colour=type)) + geom_point() + geom_line() + scale_y_log10() +
  theme_bw() + xlab("Data") + ylab("Contagem") +
  ggtitle("Casos Confirmados e Óbitos na Cidade de São Paulo")
```

![](README_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Taxas de Mudança


```r
temp = casos_sp %>% select(date, confirmed, deaths) %>% arrange(date) %>% 
  mutate(dx=as.integer(date-lag(date, default=date[1])),
         dconf=confirmed-lag(confirmed, default=confirmed[1]),
         rate = dconf/dx)
ggplot(temp, aes(date, rate)) + geom_line() + geom_point() +
  theme_bw()
```

```
## Warning: Removed 1 row(s) containing missing values (geom_path).
```

```
## Warning: Removed 1 rows containing missing values (geom_point).
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Contato

Para maiores informações, utilize a plataforma [https://covid.ic.unicamp.br/](https://covid.ic.unicamp.br/). Em particular, procuramos documentar informações desta inciativa no seu respectivo tópico em Modelagem e Epidemiologia ([https://covid.ic.unicamp.br/c/tecnolC3B3gicas/ModelagemeEpidemiologia/11](https://covid.ic.unicamp.br/c/tecnolC3B3gicas/ModelagemeEpidemiologia/11)).
