---
title: "Modelo Derivadas"
author: "Gabriel Franco"
output:
  html_document:
    keep_md: yes
---




# Introdução

Queremos obter a melhor predição para as seguintes curvas ao longo dos dias de casos acumulados, novos casos e crescimento de novos casos, como no exemplo a seguir.


```r
## Setup
my_pars <- c(9000,42,4)
f0 <- d0f(1:80, my_pars)
f1 <- d1f(1:80, my_pars)
f2 <- d2f(1:80, my_pars)

## Plot
data.frame(Dias = rep(1:80,3),
           var = rep(factor(c("Acumulados", "Novos casos", "Cresc. Novos Casos"),
                            levels=c("Acumulados", "Novos casos", "Cresc. Novos Casos")), 
                        each=80),
           Valor = c(f0,f1,f2)) %>% 
  ggplot(aes(Dias,Valor)) +
  geom_line() +
  geom_vline(xintercept = 42, linetype=2, alpha=.3) +
  facet_grid(var~., scales="free_y")
```

<img src="modelo_derivadas_files/figure-html/plot_intro-1.png" style="display: block; margin: auto;" />

Seja $f(t)$ a curva acumulada de casos, podemos modelar por uma curva logística 

$$
f(t ; \boldsymbol \Phi) = \frac{\phi_1}{1 + \exp\left\{\frac{\phi_2 - t}{\phi_3}\right\}},
$$

com $\phi_1$ sendo o número máximo de casos ao longo do tempo, $\phi_2$ o tempo $t$ em que atingimos este número máximo e $1/\phi_3$ sua velocidade de crescimento. No exemplo acima, $\phi_1=9000$, $\phi_2 = 42$ e $\phi_3 = 4$.

Consequentemente, o número de novos casos por dia e seu crescimento são modelados pela primeira e segunda derivada de $f(t)$, respectivamente.

Assim, dado que estamos antes do pico $\phi_2$ e que temos dados de casos acumulados observados, qual a melhor maneira de estimar esses três parâmetros de forma a obter uma boa estimativa de $\phi_2$?

# Proposta

Gostaríamos os parâmetros fossem bons o suficientes para que as três curvas fossem bem ajustadas. Com isso, sejam 

- $d_0(t)$ o número de casos acumulados  observados no tempo $t$,
- $d_1(t)$ o número de novos casos observados no tempo $t$ e
- $d_2(t)$ o crescimento observado de novos casos no tempo $t$,

vamos minimizar a função a seguir em relação $\Phi$:

$$
S(\Phi ; \,w_1,w_2,w_3) 
= 
w_1\sum_t \big(f(t;\Phi) - d_0(t)\big)^2 +
w_2\sum_t \big(f^\prime(t ;\Phi) - d_1(t)\big)^2 +
w_3\sum_t \big(f^{\prime \prime}(t;\Phi) - d_2(t)\big)^2,
$$

sendo $w_1,w_2,w_3$ pesos pré-definidos (ou não). A princípio, propõe-se que $w_1$ seja o maior peso de todos para que a curva se ajuste melhor nos casos observados.

# Exemplos


```r
df <- CSSEGISandData() %>%
  filter(Country.Region %in% c("China", "Korea, South", "Brazil"),
         casosAcumulados > 0) %>%
  group_by(Country.Region) %>%
  mutate(d1 = c(0, diff(casosAcumulados)), 
         d2 = c(0, diff(d1)))
```


## Coreia, a referência

```r
dd_korea <- df %>% 
  filter(Country.Region=="Korea, South") %>% 
  mutate(days = seq_along(data))

opt_korea = opt(data = dd_korea, 
                chute = c(10000,30,1), 
                pesos = c(0.01,0.01,5), 
                log=TRUE)
opt_korea$plot
```

<img src="modelo_derivadas_files/figure-html/fit_coreia-1.png" style="display: block; margin: auto;" />

## China

O ajuste para China não é tão fácil quanto o da Coreia e precisamos de chutes iniciais melhores. Vejamos os dados observados


```r
dd_china <- df %>% 
  filter(Country.Region=="China") %>% 
  mutate(days = seq_along(data))

visu(dd_china)
```

<img src="modelo_derivadas_files/figure-html/obs_china-1.png" style="display: block; margin: auto;" />

Com o gráfico acima, temos uma noção do chute dos parâmetros: $\phi_1 = 83000$ e  $\phi_2=22$. Com isso, temos o resultado abaixo.


```r
chute_china = c(max(dd_china$casosAcumulados),
                22,
                4) ## chute completamente aleatório
opt_china = opt(data=dd_china,chute = chute_china, pesos=c(1,2,4))
opt_china$plot
```

<img src="modelo_derivadas_files/figure-html/fit_china-1.png" style="display: block; margin: auto;" />

```r
opt_china$pars
```

```
## [1] 81692.810467    18.899681     4.618908
```

## Brasil

O Brasil é o caso mais difícil pq não sabemos em que pé estamos.


```r
dd_br <- df %>% 
  filter(Country.Region=="Brazil") %>% 
  mutate(days = seq_along(data))

visu(dd_br)
```

<img src="modelo_derivadas_files/figure-html/brasil_visu-1.png" style="display: block; margin: auto;" />

Note que a curva de casos acumulados e novos casos não tem nenhuma indicação de que vai começar a desacelerar. Portanto, precisamos limitar os parâmetros conforme conhecimentos prévios e observações de cenários em outros países. Os chutes iniciais são bem ruins a princípio: $\phi_1 = 180000$ e $\phi_2 = 60$.


```r
chute_br = c(220000,#10*max(dd_br$casosAcumulados),
             60,
             4) ## chute completamente aleatório
opt_br = opt(data=dd_br,chute = chute_br, pesos=c(1,2,4), lim_inf = c(0,44,0))
opt_br$plot
```

<img src="modelo_derivadas_files/figure-html/br_opt-1.png" style="display: block; margin: auto;" />

### Predição 

Dados os chutes iniciais e os dados que temos, a data esperada de pico em 2020-05-02, com um total de casos estimados em 220000 (muito perto do chute inicial).




```r
dd_pred = opt_br$pred
dd_pred$days = rep(seq_along(unique(dd_pred$data)),3)

futuro = 30
dias_fut = seq(from=max(dd_pred$days)+1,
             to=max(dd_pred$days)+futuro)
dd_append = data.frame(data = rep(rep(dd_pred$data[1],futuro),3), ## inicializando
                       var = rep(unique(dd_pred$var),each=futuro),
                       observado = 0,
                       estimado = c(d0f(dias_fut, opt_br$pars), 
                                    d1f(dias_fut, opt_br$pars), 
                                    d2f(dias_fut, opt_br$pars) 
                                    ),
                       days = rep(dias_fut,3))
dd_append$data = dd_br$data[1] + dias_fut
dd_pred$Dado = "Observado"
dd_append$Dado = "Predito"
pred_br = rbind(dd_pred,dd_append)
pred_br %>% 
  as.data.frame() %>% 
  ggplot(aes(data,observado,col=Dado)) +
  geom_point(alpha=.3) +
  geom_line(aes(y=estimado)) +
  facet_grid(var~., scales="free_y")
```

<img src="modelo_derivadas_files/figure-html/br_pred-1.png" style="display: block; margin: auto;" />

### Resumo

- Data esperada de pico:  2020-05-02
- Número estimado de máximo de casos em um dia: 6227


****

# Estudo preditivo

Vamos estudar os casos de China e Coreia e verificar em quantos dias antes do pico é possível estimá-lo com melhor precisão.

## O experimento

Em construção...


# Observações e ideias

**Todas as estimativas acima precisam ser melhoradas!**

- Suavizar as curvas melhora as estimativas?
- Incerteza das estimativas pode ser dada pela Hessiana, mas é ruim
- Depende muito do chute inicial, principalmente o número total de infectados acumulados. Talvez pegar estimativas de outros estudos.

# A fazer

- Organizar melhor os dados
- Estudar inferência
- Testar com a Coreia e China o caso de quando ainda não passamos pelo pico