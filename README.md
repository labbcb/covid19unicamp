---
title: "Modelos Preditivos para COVID-19"
author: "COVID-19 FT - UNICAMP"
output:
  html_document:
    keep_md: yes
---



## Obtenção de Dados

Afim de simplificar o acesso aos dados, utilizou-se o pacote [`datacovidbr`](https://github.com/Freguglia/datacovidbr), que importa diariamente os dados compilados pela iniciativa [Brasil IO](http://www.brasil.io), sem maiores dependências. Para que colaboradores que não utilizam o R como plataforma analítica, o arquivo `dados/covid19_cidades.csv` possui os dados (por cidade) disponíveis hoje (Sun Apr 19 12:54:00 2020).


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


```r
casos_sp = covid19_cidades %>% filter(city == "São Paulo") %>% 
  select(-state, -place_type, -is_last, -city_ibge_code)
# casos_sp = casos_sp %>% mutate(confirmed=case_when(
#   date == as.Date("2020-03-24") ~ as.integer(717),
#  TRUE ~ confirmed))
casos_sp %>% head() %>% knitr::kable("markdown")
```



|date       |city      | confirmed| deaths| estimated_population_2019| confirmed_per_100k_inhabitants| death_rate|
|:----------|:---------|---------:|------:|-------------------------:|------------------------------:|----------:|
|2020-04-18 |São Paulo |      9428|    686|                  12252023|                       76.95056|     0.0728|
|2020-04-17 |São Paulo |      8744|    643|                  12252023|                       71.36781|     0.0735|
|2020-04-16 |São Paulo |      7908|    603|                  12252023|                       64.54444|     0.0763|
|2020-04-15 |São Paulo |      7764|    558|                  12252023|                       63.36913|     0.0719|
|2020-04-14 |São Paulo |      6705|    512|                  12252023|                       54.72566|     0.0764|
|2020-04-13 |São Paulo |      6418|    456|                  12252023|                       52.38319|     0.0711|

```r
casos_sp %>% select(date, confirmed, deaths) %>% gather(type, counts, -date) %>% 
  ggplot(aes(date, counts, colour=type)) + geom_point() + geom_line() + scale_y_log10() +
  theme_bw() + xlab("Data") + ylab("Contagem") +
  ggtitle("Casos Confirmados e Óbitos na Cidade de São Paulo")
```

![](README_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
casos_sp %>% select(date, confirmed, deaths) %>% gather(type, counts, -date) %>% 
  ggplot(aes(date, counts, colour=type)) + geom_point() + geom_line() +
  theme_bw() + xlab("Data") + ylab("Contagem") +
  ggtitle("Casos Confirmados e Óbitos na Cidade de São Paulo")
```

![](README_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

## Taxas de Mudança


```r
temp = casos_sp %>% select(date, confirmed, deaths) %>% arrange(date) %>% 
  mutate(dx=as.integer(date-lag(date, default=date[1])),
         dconf=confirmed-lag(confirmed, default=confirmed[1]),
         rate = dconf/dx)
ggplot(temp, aes(date, weight=rate)) + geom_bar() +
  xlab("Data") + ylab("Taxa de Novos Casos") +
  theme_bw()
```

![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## Contato

Para maiores informações, utilize a plataforma [https://covid.ic.unicamp.br/](https://covid.ic.unicamp.br/). Em particular, procuramos documentar informações desta inciativa no seu respectivo tópico em Modelagem e Epidemiologia ([https://covid.ic.unicamp.br/c/tecnolC3B3gicas/ModelagemeEpidemiologia/11](https://covid.ic.unicamp.br/c/tecnolC3B3gicas/ModelagemeEpidemiologia/11)).
