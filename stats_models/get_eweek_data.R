library(datacovidbr)
library(dplyr)
library(broom)
library(stringr)
library(lubridate)

# Data manipulations ------------------------------------------------------

eweekdata <- brasilio(cache = TRUE) %>%
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
  filter(eweek < epiweek(today())) %>%
  group_by(cidade, estimated_population_2019, city_ibge_code) %>% 
  mutate(myweek = eweek - lag(eweek, 1, 0),
         myweek = ifelse(myweek > 8, 1, myweek),
         myweek = cumsum(myweek)) %>% 
  ungroup()

rm_few <- eweekdata %>% 
  group_by(cidade, estimated_population_2019, city_ibge_code) %>% 
  summarise(tcases = sum(wcases), n=n()) %>% 
  filter(n < 2 | tcases < 100) %>%
  ungroup()

rm_neg <- eweekdata %>%
  filter(wcases < 0) %>%
  select(cidade, estimated_population_2019, city_ibge_code) %>% 
  distinct()

eweekdata <- eweekdata %>% anti_join(
  rm_few %>% bind_rows(rm_neg),
  by = "city_ibge_code"
)

eweekdata$cidade <- relevel(factor(eweekdata$cidade), "São Paulo-SP")



IDH <- readxl::read_excel("../dados_fixos/atlas2013_dadosbrutos_pt.xlsx", sheet=2) %>%
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

eweekdata <- eweekdata %>%
  inner_join(IDH, by=c('city_ibge_code'='Codmun7')) %>% 
  select(-ANO, -UF, -Codmun6, -Município)



