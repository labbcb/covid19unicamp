## Funções logística e suas derivadas
d0f <- function(x, p){
  exp_denom = exp((p[2]-x)/p[3])
  p[1]/(1+exp_denom)
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
# 
# ## Acumulados
# d0f <- function(x, p){
#   exp_denom = exp((p[2]-x)/p[3])
#   p[1]/(1+exp_denom)
# }
# 
# ## Novos casos
# d1f = function(x, pars){
#   expterm = exp((pars[2]-x)/pars[3])
#   num = pars[1]*expterm
#   den = pars[3]*((1+expterm)^2)
#   num/den
# }
# 
# ## Crescimento novos casos
# d2f = function(x, pars){
#   expterm = exp((pars[2]-x)/pars[3])
#   num = pars[1]*expterm*(expterm-1)
#   den = (pars[3]^2)*((expterm+1)^3)
#   num/den
# }
# 
# ## Função a ser otimizada
# my_func <- function(pars, data, pesos, log=FALSE, log_base = 10){
#   days <- seq_along(data$data)
#   data$pred_d1 <- d1f(days, pars)
#   data$pred_d2 <- d2f(days, pars)
#   data$pred_cum <- d0f(days, pars)
#   if(log){
#     data$pred_d1 <- log(data$pred_d1, base = log_base)
#     data$pred_d2 <- log(data$pred_d2, base = log_base)
#     data$pred_cum <- log(data$pred_cum, base = log_base)
#     data$d1 <- log(data$d1, base = log_base)
#     data$d2 <- log(data$d2, base = log_base)
#     data$casosAcumulados <- log(data$casosAcumulados, base = log_base)
#   }
#   
#   ssq = pesos[1]*mean((data$casosAcumulados - data$pred_cum)^2) +
#     pesos[2]* mean((data$pred_d1 - data$d1)^2) +
#     pesos[3]*mean((data$pred_d2 - data$d2)^2) 
#   ssq
# }
# 
# opt <- function(data, chute, pesos, lim_inf =c(0,0,0), use_log=FALSE, log_base = 10){
#   opt =optim(chute, my_func, 
#              data = data, 
#              pesos = pesos, 
#              log = use_log,
#              log_base = log_base,
#              lower = lim_inf, 
#              hessian=TRUE)
#   
#   dd_pred = data
#   days <- seq_along(dd_pred$data)
#   dd_pred$pred_d1 = d1f(days,opt$par)
#   dd_pred$pred_d2 = d2f(days,opt$par)
#   dd_pred$pred_cum = d0f(days,opt$par)
#   
#   dd_pred = data.frame(data = rep(dd_pred$data,times=3),
#                        var = rep(c("Acumulados","d1","d2"), each=length(days)),
#                        observado = c(dd_pred$casosAcumulados,dd_pred$d1,dd_pred$d2),
#                        estimado = c(dd_pred$pred_cum,dd_pred$pred_d1, dd_pred$pred_d2))
#   p = dd_pred %>% 
#     ggplot(aes(data,observado))+
#     geom_point(alpha=.3)+
#     geom_line(aes(y=estimado))+
#     facet_grid(var~., scales="free_y")
#   
#   list("plot"=p,
#        "pars" = opt$par ,
#        "pred" = dd_pred,
#        "parsSE" = sqrt(abs(diag(solve(opt$hessian)))))
# }
# 
# visu <- function(data, useData = TRUE){
#   if(useData)
#     days <- data$data
#   else
#     days <- seq_along(data$data)
#   data = data.frame( data = rep(days,times=3),
#                      var = rep(c("Acumulados","Confirmados (d1)","Dif. Dia Ant. (d2)"), each=length(days)),
#                      observado = c(data$casosAcumulados,data$d1,data$d2))
#   data %>% 
#     ggplot(aes(data,observado))+
#     geom_point()+
#     facet_grid(var~., scales="free_y")
# }
# 
# futuro <- function(optObj, n_fut=30){
#   dd_pred = optObj[["pred"]]
#   dd_pred[["days"]] = rep(seq_along(unique(dd_pred[["data"]])),3)
#   
#   futuro = n_fut
#   dias_fut = seq(from=max(dd_pred[["days"]])+1,
#                  to=max(dd_pred[["days"]])+futuro)
#   dd_append = data.frame(data = rep(rep(dd_pred[["data"]][1],futuro),3), ## inicializando
#                          var = rep(unique(dd_pred[["var"]]),each=futuro),
#                          observado = 0,
#                          estimado = c(d0f(dias_fut, optObj[["pars"]]), 
#                                       d1f(dias_fut, optObj[["pars"]]), 
#                                       d2f(dias_fut, optObj[["pars"]]) 
#                          ),
#                          days = rep(dias_fut,3))
#   dd_append[["data"]] =  max(optObj$pred$data)+seq_along(dias_fut)
#   dd_pred[["Dado"]] = "Observado"
#   dd_append[["Dado"]] = "Predito"
#   pred_br = rbind(dd_pred,dd_append)
#   pred_br %>% 
#     as.data.frame() %>% 
#     ggplot(aes(data,observado,col=Dado)) +
#     geom_point(alpha=.3) +
#     geom_line(aes(y=estimado)) +
#     facet_grid(var~., scales="free_y")
# }
