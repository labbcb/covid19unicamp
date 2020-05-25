---
title: "Modelos Preditivos para COVID-19"
author: "COVID-19 FT - UNICAMP"
output:
  html_document:
    keep_md: yes
---



## Obtenção de Dados

Afim de simplificar o acesso aos dados, utilizou-se o pacote [`datacovidbr`](https://github.com/Freguglia/datacovidbr), que importa diariamente os dados compilados pela iniciativa [Brasil IO](http://www.brasil.io), sem maiores dependências. Para que colaboradores que não utilizam o R como plataforma analítica, o arquivo `dados/covid19_cidades.csv` possui os dados (por cidade) disponíveis hoje (Mon May 25 17:17:00 2020).


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
|2020-05-24 |São Paulo |     45527|   3534|                  12252023|                       371.5876|     0.0776|
|2020-05-23 |São Paulo |     44887|   3491|                  12252023|                       366.3640|     0.0778|
|2020-05-22 |São Paulo |     42973|   3352|                  12252023|                       350.7421|     0.0780|
|2020-05-21 |São Paulo |     41451|   3238|                  12252023|                       338.3196|     0.0781|
|2020-05-20 |São Paulo |     39466|   3135|                  12252023|                       322.1182|     0.0794|
|2020-05-19 |São Paulo |     37640|   3029|                  12252023|                       307.2146|     0.0805|

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
