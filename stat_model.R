library(lubridate)
library(splines)
library(ciTools)
library(datacovidbr)
library(tidyverse)
library(broom)
library(stringr)
library(doMC)
registerDoMC(4)

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
          family=quasipoisson, data = eweekdata)

model = tidy(fit)

mycoefs = model %>%
  filter(str_detect(term, "cidade")) %>% 
  mutate(term = str_remove(term, "cidade"))

## automatizar predicoes (não utilizar intervalos de confianca e sim de predicao)
newdata = eweekdata %>%
  group_by(cidade, estimated_population_2019, city_ibge_code) %>%
  top_n(2, eweek) %>% 
  mutate(eweek = eweek + 2L,
         wcases = NA,
         wdeaths = NA,
         myweek = myweek + 2L) %>% 
  ungroup() %>% 
  select(-wcases)

rodpoisson = function(n, lambda, disp){
  rnbinom(n, size=(lambda/(disp-1)), mu=lambda)
}

getPI = function(data4pred, inputdata, model, nSim=1000, confs = c(.80, .90, .95)){
  wcases_expected = predict(model, type='response')
  preds = foreach(i=1:nSim, .combine = cbind) %dopar% {
    eweekdata_sim = inputdata %>%
      mutate(wcases = rodpoisson(length(wcases_expected), lambda = wcases_expected,
                                 disp = summary(model)$dispersion))
    ##mutate(wcases = rpois(length(wcases_expected), lambda = wcases_expected))
    fit_sim = glm(wcases ~ ns(myweek, 3) + cidade + offset(log(estimated_population_2019)),
                  family=quasipoisson, data = eweekdata_sim)
    wcases_pred = predict(fit_sim, newdata=data4pred, type='response')
    ##rpois(length(wcases_pred), lambda = wcases_pred)
    rodpoisson(length(wcases_pred), lambda = wcases_pred, disp = summary(fit_sim)$dispersion)
  }

  lwr = (1-confs)/2
  names(lwr) = sprintf("lwr%.3f", confs)
  upr = confs + lwr
  names(upr) = sprintf("upr%.3f", confs)
  
  PI = t(apply(preds, 1, quantile, probs = c(lwr, upr)))
  colnames(PI) = names(c(lwr, upr))
  cbind(pred = predict(model, newdata=data4pred, type='response'), PI) %>% 
    as_tibble()
}

tmp = newdata %>% getPI(eweekdata, fit)



### se for adicionar IDH
library(readxl)
IDH = read_excel("dados_fixos/atlas2013_dadosbrutos_pt.xlsx", sheet=2) %>%
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


newdata = eweekdata2 %>%
  group_by(cidade, estimated_population_2019, city_ibge_code) %>%
  top_n(2, eweek) %>% 
  mutate(eweek = eweek + 2L,
         wcases = NA,
         wdeaths = NA,
         myweek = myweek + 2L) %>% 
  ungroup() %>% 
  select(-wcases)

newdata %>%
  add_ci(fit2, yhatName = "wcases", names = c("lci", "uci")) %>% 
  add_pi(fit2, names = c("lpi", "upi")) %>% 
  select(cidade, eweek, wcases, lci, uci, pred, lpi, upi)
