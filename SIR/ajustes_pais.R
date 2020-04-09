rm(list = ls())
bibs <- c("tidyverse", "knitr", "datacovidbr", "deSolve", "xtable")
sapply(bibs, require, character.only = T)

wd <- "C:/Users/User/Dropbox/covid19/covid19unicamp/SIR"
source(file.path(wd, "functions.R"))

Nch = 13.86 * 10^8 
Nko = 51.47 * 10^6
Nbr = 21 * 10^7

wor <- CSSEGISandData()


# China -------------------------------------------------------------------

df <- wor %>%
  ungroup() %>%
  filter(Country.Region == "China", casosAcumulados > 0)  

ggplot(df, aes(x=data, y=recuperadosAcumulado)) +
  geom_point()
df %>% 
  mutate(casos = c(casosAcumulados[1], diff(casosAcumulados))) %>% 
  ggplot(aes(x = data, y = casos)) +
  geom_point()


infected  <- df$casosAcumulados - df$obitosAcumulado
recovered <- df$recuperadosAcumulado
N         <- Nko # população do pais
dias      <- df$data
obj       <- fit.model(infected = infected, 
                       recovered = recovered,
                       N = N, parms = c(0.1, 0.5), 
                       dates = dias)
res       <- summary.model(obj)

res$plot1


# Corea do Sul ------------------------------------------------------------

df <- wor %>%
  ungroup() %>%
  filter(Country.Region == "Korea, South", casosAcumulados > 0)  

ggplot(df, aes(x=data, y=recuperadosAcumulado)) +
  geom_point()
df %>% 
  mutate(casos = c(casosAcumulados[1], diff(casosAcumulados))) %>% 
  ggplot(aes(x = data, y = casos)) +
  geom_point()


infected  <- df$casosAcumulados - df$obitosAcumulado
recovered <- df$recuperadosAcumulado
N         <- Nko # população do pais
dias      <- df$data
obj       <- fit.model(infected = infected, 
                       recovered = recovered,
                       N = N, parms = c(0.5, 0.5), 
                       dates = dias)
res       <- summary.model(obj)
res$plot1


# Brasil ------------------------------------------------------------------
df <- wor %>%
  ungroup() %>%
  filter(Country.Region == "Brazil", casosAcumulados > 0)  

ggplot(df, aes(x=data, y=recuperadosAcumulado)) +
  geom_point()
df %>% 
  mutate(casos = c(casosAcumulados[1], diff(casosAcumulados))) %>% 
  ggplot(aes(x = data, y = casos)) +
  geom_point()


infected  <- df$casosAcumulados - df$obitosAcumulado
N         <- Nbr # população do pais
dias      <- df$data
obj       <- fit.model(infected = infected, N = N, parms = c(0.5, 0.5), 
                       dates = dias)
res       <- summary.model(obj)
res$plot1

