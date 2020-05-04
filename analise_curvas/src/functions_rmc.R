# devtools::install_github("Freguglia/datacovidbr")
# devtools::install_github("gabrielfranco89/covid19peakfit")
library(tidyverse)
library(datacovidbr)
library(covid19peakfit)
library(DT)

get_RMC <- function(){
  dd_raw = brasilio(silent=TRUE)
  rmc = c("Americana",
          "Artur Nogueira",
          "Campinas",
          "Cosmópolis", ##
          "Engenheiro Coelho",
          "Holambra",
          "Hortolândia",
          "Indaiatuba",
          "Itatiba",
          "Jaguariúna",
          "Monte Mor",
          "Morumgaba",##
          "Nova Odessa",
          "Paulínia",
          "Pedreira",##
          "Santa Bárbara d'Oeste",
          "Santo Antônio de Posse",##
          "Sumaré",
          "Valinhos",
          "Vinhedo")
  dd = dd_raw %>% filter(city %in% rmc) %>% 
    group_by(date) %>% 
    summarise(obitos = sum(deaths),
              confirmados = sum(confirmed)
    )
  dd
}

## Formas das curvas =======================
d0f <- function(x, pars){
  exp_denom = exp((pars[2]-x)/pars[3])
  pars[1]/(1+exp_denom)
}

d1f = function(x, pars){
  expterm = exp((pars[2]-x)/pars[3])
  num = pars[1]*expterm
  den = pars[3]*((1+expterm)^2)
  num/den
}

d2f = function(x, pars){
  expterm = exp((pars[2]-x)/pars[3])
  num = pars[1]*expterm*(expterm-1)
  den = (pars[3]^2)*((expterm+1)^3)
  num/den
}

## Função auxiliar ===============================
foo <- function(dd_rmc,
                t_treino,
                t_pred,
                my_init =  c(300000,42,8),
                my_pesos = c(.1,3,.3),
                my_liminf = c(5000, 40, 0)){
  ddp = covid19peakfit::prep_data(data = dd_rmc,
                                  cum_cases = "confirmados",
                                  date_var = "date")
  ddp$days= seq_along(ddp$date)
  if(t_treino+t_pred > nrow(dd_rmc)) stop("Treino e teste ultrapassam o número de dados disponíveis")
  dd_treino = ddp[1:t_treino,]
  dd_test = ddp[(t_treino+1):(t_treino+t_pred),]
  
  fit = covid19peakfit::covid19peakfit(
    data = dd_treino,
    n_cases = "num_cases",
    cum_cases = "cum_cases",
    date_var = "date",
    init_pars = my_init,
    weights = my_pesos,
    lim_inf = my_liminf
  )
  
  dd_pred = dd_test
  dd_pred$num_cases = d1f(dd_pred$days, fit$pars)
  dd_pred$cum_cases = d0f(dd_pred$days, fit$pars)
  dd_pred$d2 = d2f(dd_pred$days, fit$pars)
  
  ssq = mean((dd_pred$d2 - dd_test$d2)^2 + 
               (dd_pred$num_cases - dd_test$num_cases)^2 + 
               (dd_pred$cum_cases - dd_test$cum_cases)^2)
  
  data.frame(phi_1 = fit$pars[1],
             phi_2 = fit$pars[2],
             phi_3 = fit$pars[3],
             ssq= ssq,
             t_treino, 
             t_pred)
}


## Função auxiliar exp2 ===============================
foo2 <- function(dd_rmc,
                 t_treino,
                 t_pred,
                 phi1,
                 phi2,
                 phi3){
  
  phi=c(phi1,phi2,phi3)
  ddp = covid19peakfit::prep_data(data = dd_rmc,
                                  cum_cases = "confirmados",
                                  date_var = "date")
  ddp$days= seq_along(ddp$date)
  if(t_treino+t_pred > nrow(dd_rmc)) stop("Treino e teste ultrapassam o número de dados disponíveis")
  dd_treino = ddp[1:t_treino,]
  dd_test = ddp[(t_treino+1):(t_treino+t_pred),]
  
  dd_pred = dd_test
  dd_pred$num_cases = d1f(dd_pred$days, phi)
  dd_pred$cum_cases = d0f(dd_pred$days, phi)
  dd_pred$d2 = d2f(dd_pred$days, phi)
  
  ssq = mean((dd_pred$d2 - dd_test$d2)^2 + 
               (dd_pred$num_cases - dd_test$num_cases)^2 + 
               (dd_pred$cum_cases - dd_test$cum_cases)^2)
  ssq
  # 
  # data.frame(phi_1 = phi[1],
  #            phi_2 = phi[2],
  #            phi_3 = phi[3],
  #            ssq= ssq,
  #            t_treino, 
  #            t_pred)
}












