# Esse script e utilizado apenas 
library(readr)
library(dplyr)

# Dados de https://github.com/seade-R/dados-covid-sp
DADOS_SP_URL = "https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv"

# Obter dados atualizados de arquivo CSV remoto, fazer filtros e calculos, gravar objeto em disco.
read_csv2(DADOS_SP_URL) %>%
  group_by(nome_ra, cod_ra, nome_drs, cod_drs, codigo_ibge, nome_munic, semana_epidem) %>% 
  summarise(obitos_novos_semana = as.integer(sum(obitos_novos)),
            casos_novos_semana = as.integer(sum(casos_novos)),
            pop = as.integer(max(pop)),
            pop60 = as.integer(max(pop_60)),
            area = as.integer(max(area))) %>% 
  filter(obitos_novos_semana > 0 | casos_novos_semana > 0) %>% 
  mutate(semana = 1:n(),
         casos_evolucao = casos_novos_semana/lag(casos_novos_semana, 1, 0),
         obitos_evolucao = obitos_novos_semana/lag(obitos_novos_semana, 1, 0),
         casos_evolucao = paste0(round((casos_evolucao-1)*100, 0), "%"),
         casos_evolucao = ifelse(grepl("\\-", casos_evolucao), casos_evolucao, paste0("+", casos_evolucao)),
         obitos_evolucao = paste0(round((obitos_evolucao-1)*100, 0), "%"),
         obitos_evolucao = ifelse(grepl("\\-", obitos_evolucao), obitos_evolucao, paste0("+", obitos_evolucao))
  ) %>% 
  ungroup() %>%
  saveRDS(file = "data/covid_semana.rds")
