FF <- function(x,Digits=4,Width=4){(formatC(x,digits=Digits,width=Width,format="f"))}
FF_pretty <- function(x){prettyNum(x, big.mark = ".", decimal.mark = ",")}

# Define EDO (modelo SIR) -------------------------------------------------
SIR <- function(time, state, parms)
{
  par <- as.list(c(state, parms))
  with(par, {
    dS <- -beta / N * I * S
    dI <- beta / N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# Retorna RSS (I - I_hat)^2 ------------------------------------------------
RSS.model <- function(parms, init, times, infected)
{
  names(parms) <- c("beta", "gamma")
  out <- ode(y = init, times = times, func = SIR, parms = parms)
  fit <- out[, 3]
  ll  <- sum((infected - fit)^2)
  return(ll)
}


# Estima modelo SIR minimizando RSS ---------------------------------------

fit.model <- function(infected, recovered = 0, N, parms, dates = NULL)
{
  if(is.null(dates)) {
    times <- 1:length(infected)
  }
  else{
    times <- 1:length(dates)
  }
  init  <- c(S = N-infected[1], I = infected[1], R = recovered)
  out   <- optim(parms, RSS.model, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), 
                 init = init, times = times[-1], infected = infected[-1])
  est   <- setNames(out$par, c("beta", "gamma"))
  R0    <- setNames(est["beta"] / est["gamma"], "R0")
  
  obj   <- list(infected = infected, 
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

summary.model <- function(obj, h = 50)
{
  
  tobs  <- obj$times
  n     <- length(tobs)
  tpred <- 1:(n + h)
  tb    <- data.frame(ode(y = obj$init, times = tpred, func = SIR, parms = obj$parms))
  
  if(!is.null(obj$dates))
  {
    dates   <- seq(obj$dates[1], obj$dates[n]+(h), l = n+h)
    tb$time <- dates
  }
  ## Estima total máximo de infectados e tempo que isso ocorre
  I_max <- tb[which.max(tb$I), ][c("I", "time")]

  ## Gráfico do comportamento infectados, suscetiveis e recuperados  
  tb_long <- tb %>% 
    gather(status, estimate, -time) %>% 
    mutate(status = case_when(status == "I" ~ "Infectados",
                              status == "R" ~ "Recuperados",
                              status == "S" ~ "Suscetiveis"))
  ylim <- range(c(0,tb_long$estimate))

  p_pred <- ggplot(tb_long, aes(x = time, y = estimate, col = status)) +
    geom_line(size = 1) +
    labs(x = "Dia", y = "Total acumulado", col = "") +
    scale_y_continuous(limits = ylim, breaks = round(seq(ylim[1], ylim[2], l = 5)), 
                       labels = FF_pretty(round(seq(ylim[1], ylim[2], l = 5)))) +
    theme_bw() +
    theme(text = element_text(size = 14), 
          legend.position = "top")
  
  if(!is.null(obj$dates))
  {
    p_pred <- p_pred + scale_x_date(date_breaks = "1 week", date_labels = "%d/%b")
  }
  
  ## Gráfico do comportamento de infectados
  tb_infec <- tb_long %>% 
    filter(status == "Infectados")
  ylim <- range(c(0,tb_infec$estimate))
  
  p_infec <- ggplot(tb_infec, aes(x = time, y = estimate)) +
    geom_line(size = 1) +
    geom_vline(xintercept = I_max$time, linetype = "dashed") +
    labs(x = "Dia", y = "Total da Populacao", col = "") +
    scale_y_continuous(limits = ylim, breaks = round(seq(ylim[1], ylim[2], l = 5)), 
                       labels = FF_pretty(round(seq(ylim[1], ylim[2], l = 5)))) +
    theme_bw() +
    theme(text = element_text(size = 14), 
          legend.position = "top")
  
  if(!is.null(obj$dates))
  {
    p_infec <- p_infec + scale_x_date(date_breaks = "1 week", date_labels = "%d/%b")
  }
  

  ## Gráfico preditos versus observados
  tb_inf <- tb_long %>% 
    filter(status == "Infectados") %>% 
    mutate(obs = c(obj$infected, rep(NA, h))) %>% 
    filter(!is.na(obs)) %>% 
    select(-status) %>% 
    gather(tipo, valor, -time) %>% 
    mutate(tipo = ifelse(tipo == "obs", "Observado", "Estimado"))
    
  ylim <- range(c(0, tb_inf$valor))
  
  p_fit <- ggplot(tb_inf, aes(x = time, y = valor, color = tipo)) +
    geom_point(size = 1) +
    geom_point() +
    labs(x = "Dia", y = "Total acumulado", col = "") +
    scale_color_manual(values = c("red", "blue")) +
    scale_y_continuous(limits = ylim, breaks = round(seq(ylim[1], ylim[2], l = 5)), 
                       labels = FF_pretty(round(seq(ylim[1], ylim[2], l = 5)))) +
    theme_bw() +
    theme(text = element_text(size = 14), 
          legend.position = "top")
  
  if(!is.null(obj$dates))
  {
    p_fit <- p_fit + scale_x_date(date_breaks = "1 week", date_labels = "%d/%b")
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



