FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}
FF_pretty <- function(x){prettyNum(x, big.mark = ".", decimal.mark = ",")}

# Define EDO (modelo SIR) -------------------------------------------------
SIR <- function(time, state, parms)
{
  par <- as.list(c(state, parms))
  with(par, {
    dS <- -beta / N * I * S
    dI <- beta /  N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# Retorna RSS (I - I_hat)^2 ------------------------------------------------
RSS.model <- function(parms, init, times, infected, recovered=NULL)
{
  names(parms) <- c("beta", "gamma")
  out   <- ode(y = init, times = times, func = SIR, parms = parms)
  I_hat <- out[, 3]
  ll    <- sqrt( mean((infected - I_hat)^2)  )
  if(!is.null(recovered))
  {
    R_hat <- out[, 4]
    l1    <- sqrt( mean((infected - I_hat)^2)  )
    l2    <- sqrt( mean((infected - R_hat)^2)  )
    alpha <- 0.2 ## mais peso para casos recuperados
    ll    <- alpha * l1 + (1 - alpha) * l2
  }
  return(ll)
}


# Estima modelo SIR minimizando RSS ---------------------------------------

fit.model <- function(infected, recovered = NULL, N, parms, dates = NULL)
{
  if(is.null(dates)) {
    times <- 1:length(infected)
  }
  else{
    times <- 1:length(dates)
  }
  R_ini <- 0
  if(!is.null(recovered))
  {
    R_ini <- recovered[1]
  }
  
  ## Total de casos por dia
  cases <- c(infected[1], diff(infected))
  
  ## Condição inicial para EDO
  init  <- c(S = N-cases[1], I = cases[1], R = R_ini)
  
  ## Otimização
  out   <- optim(parms, RSS.model, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1),
                 init = init, times = times[-1], infected = cases[-1], recovered = recovered)
  
  est   <- setNames(out$par, c("beta", "gamma"))
  R0    <- setNames(est["beta"] / est["gamma"], "R0")
  
  # cat(ifelse(out$convergence == 0, "Convegencia obtida", "Convergencia falhou"), "\n")
  
  obj   <- list(infected = infected, 
                cases    = cases,
                recovered = recovered, 
                N = N, 
                init = init,
                times = times,
                dates = dates,
                convergence = out$convergence, 
                parms       = est,
                R0          = R0)
  return(obj)
}


# Extrai informações do modelo ajustado -----------------------------------

summary.model <- function(obj, h = 60)
{
  
  tobs  <- obj$times
  n     <- length(tobs)
  tpred <- 1:(n + h)
  tb    <- data.frame(ode(y = obj$init, times = tpred, func = SIR, parms = obj$parms))
  
  RMSE  <- paste0("RMSE = ", round(sqrt(mean( (tb$I[1:n] - obj$cases)^2 )), 2) )
  
  if(!is.null(obj$dates))
  {
    dates   <- seq(obj$dates[1], obj$dates[n] + h, l = n+h)
    tb$time <- dates
  }
  ## Estima máximo de infectados e tempo que isso ocorre
  I_max <- tb[which.max(tb$I), ][c("I", "time")]

  ## Gráfico do comportamento infectados, suscetiveis e recuperados  
  tb_long <- tb %>% 
    gather(status, estimate, -time) 
  #   mutate(status = case_when(status == "I" ~ "Infectados",
  #                             status == "R" ~ "Recuperados",
  #                             status == "S" ~ "Suscetiveis"))

  p_pred <- ggplot(tb_long, aes(x = time, y = estimate, col = status)) +
    geom_line(size = 1) +
    labs(x = "Dia", y = "Total", col = "") +
    theme_bw() +
    theme(text = element_text(size = 10), legend.position = "top") 
    # ggtitle("Predicao do modelo SIR")
  
  
  if(!is.null(obj$dates))
  {
    p_pred <- p_pred + scale_x_date(date_breaks = "3 week", date_labels = "%d/%b")
  }
  
  ## Gráfico do comportamento de infectados
  p_infec <- tb_long %>% 
    filter(status == "I") %>% 
    ggplot(aes(x = time, y = estimate)) +
      geom_line(size = 1) +
      geom_vline(xintercept = I_max$time, linetype = "dashed") +
      labs(x = "Dia", y = "Total de infectados", col = "") +
      theme_bw() +
      theme(text = element_text(size = 10), 
            legend.position = "top") 
  #ggtitle("Comportamento do total de infectados")
  
  
  if(!is.null(obj$dates))
  {
    p_infec <- p_infec + scale_x_date(date_breaks = "3 week", date_labels = "%d/%b")
  }
  
  ## Gráfico preditos versus observados
  p_fit <- tb_long %>% 
    filter(status == "I") %>%
    slice(1:n) %>% 
    mutate(obs = obj$cases) %>%
    ggplot(aes(x = time, y = obs)) +
      geom_point(size = 2) +
      geom_line(aes(x = time, y = estimate)) +
      labs(x = "Dia", y = "Total de casos infectados") +
      theme_bw() +
      theme(text = element_text(size = 10)) +
      ggtitle(paste0("Ajuste modelo SIR versus casos observados (", RMSE, ")"))
  
  
  if(!is.null(obj$dates))
  {
    p_fit <- p_fit + scale_x_date(date_breaks = "5 days", date_labels = "%d/%b")
  }

  ## Parâmetros estimados

  estimates <- data.frame(beta = FF(obj$parms[1]), gamma = FF(obj$parms[2]),
                          R0 = FF(obj$R0), Imax = FF_pretty(round(I_max$I)), 
                          Tmax = format.Date(I_max$time, "%d/%b/%Y"))
  names(estimates) <- c("$\\beta$", "$\\gamma$", "$R_0$", "$I_{\\max}$", "$T_{\\max}$")
  rownames(estimates) <- NULL

  
  lt = list(estimates = estimates, plot1 = p_fit, plot2 = p_pred, plot3 = p_infec)
  return(lt)
}



