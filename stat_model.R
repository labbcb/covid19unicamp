library(lubridate)
library(splines)
library(ciTools)
library(datacovidbr)
library(tidyverse)
library(broom)
library(stringr)

eweekdata = brasilio() %>%
  filter(place_type == "city", city != "Importados/Indefinidos") %>% 
  select(date, state, city, confirmed, deaths, estimated_population_2019, city_ibge_code) %>% 
  group_by(state, city) %>% 
  arrange(date) %>% 
  mutate(eweek = epiweek(date),
         daily_cases = confirmed - lag(confirmed, n=1, default=0),
         daily_deaths = deaths - lag(deaths, n=1, default=0),
         cidade = paste(city, state, sep="-")) %>% 
  select(-confirmed, -deaths) %>% 
  group_by(cidade, estimated_population_2019, city_ibge_code, eweek) %>% 
  summarise(wcases = sum(daily_cases, na.rm=TRUE),
            wdeaths = sum(daily_deaths, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(eweek < lubridate::epiweek(lubridate::today())) %>%
  group_by(cidade, estimated_population_2019, city_ibge_code) %>% 
  mutate(myweek = eweek - lag(eweek, 1, 0),
         myweek = ifelse(myweek > 8, 1, myweek),
         myweek = cumsum(myweek)) %>% 
  ungroup()

rm_few = eweekdata %>% 
  group_by(cidade, estimated_population_2019, city_ibge_code) %>% 
  summarise(tcases = sum(wcases), n=n()) %>% 
  filter(n < 2 | tcases < 100) %>%
  ungroup()

rm_neg = eweekdata %>%
  filter(wcases < 0) %>%
  select(cidade, estimated_population_2019, city_ibge_code) %>% 
  distinct()

eweekdata = eweekdata %>% anti_join(
  rm_few %>% bind_rows(rm_neg),
  by = "city_ibge_code"
)
eweekdata$cidade = relevel(factor(eweekdata$cidade), "São Paulo-SP")

fit = glm(wcases ~ ns(myweek, 3) + cidade + offset(log(estimated_population_2019)),
          family=poisson(link="log"), data = eweekdata)

model = tidy(fit)

mycoefs = model %>%
  filter(str_detect(term, "cidade")) %>% 
  mutate(term = str_remove(term, "cidade"))

eweekdata %>% 
  filter(cidade == "Campinas-SP") %>% 
  tail(1)

## automatizar predicoes (não utilizar intervalos de confianca e sim de predicao)
newdata = tibble(cidade = "Campinas-SP", estimated_population_2019=1204073,
                 eweek = 22:23, myweek = 11:12)
add_ci(newdata, fit)



### se for adicionar IDH
library(readxl)
IDH = read_excel("~/Downloads/atlas2013_dadosbrutos_pt.xlsx", sheet=2) %>%
  filter(ANO == 2010) %>% 
  select(ANO:Município, IDHM) %>% 
  mutate(QIDHM = case_when(
    IDHM < .5              ~ "Muito Baixo",
    IDHM >= .5 & IDHM < .6 ~ "Baixo",
    IDHM >= .6 & IDHM < .7 ~ "Médio",
    IDHM >= .7 & IDHM < .8 ~ "Alto",
    IDHM >= .8             ~ "Muito Alto"
  ),
  QIDHM = factor(QIDHM,
                 levels = c("Muito Alto", "Alto", "Médio", "Baixo", "Muito Baixo")))

eweekdata2 = eweekdata %>%
  inner_join(IDH, by=c('city_ibge_code'='Codmun7')) %>% 
  select(-ANO, -UF, -Codmun6, -Município)

fit2 = glm(wcases ~ ns(myweek, 3) + QIDHM + cidade + offset(log(estimated_population_2019)),
           family=poisson(link="log"), data = eweekdata2)

model2 = tidy(fit2)

mycoefs2 = model2 %>%
  filter(str_detect(term, "cidade")) %>% 
  mutate(term = str_remove(term, "cidade"))
