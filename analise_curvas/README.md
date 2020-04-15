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

Seja <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/27099e26220f898359382d05f75b941c.svg?invert_in_darkmode" align=middle width=28.539060000000003pt height=24.65759999999998pt/> a curva acumulada de casos, podemos modelar por uma curva logística 

<p align="center"><img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/087aefba3674b63de09b83f2526144c8.svg?invert_in_darkmode" align=middle width=191.51385pt height=50.317245pt/></p>

com <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/648af807e14f1259d294a13819fd4d1e.svg?invert_in_darkmode" align=middle width=16.347210000000004pt height=22.831379999999992pt/> sendo o número máximo de casos ao longo do tempo, <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/e8c65b8d4ccca28b6729b706fc85c469.svg?invert_in_darkmode" align=middle width=16.347210000000004pt height=22.831379999999992pt/> o tempo <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/4f4f4e395762a3af4575de74c019ebb5.svg?invert_in_darkmode" align=middle width=5.936155500000004pt height=20.222069999999988pt/> em que atingimos este número máximo e <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/5713eac7b5d6d965791ce29a1891e186.svg?invert_in_darkmode" align=middle width=32.7855pt height=24.65759999999998pt/> sua velocidade de crescimento. No exemplo acima, <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/0207863831d63dfc0d420fce53480bd4.svg?invert_in_darkmode" align=middle width=71.963595pt height=22.831379999999992pt/>, <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/cdfbdf69d9b63b4e3cfd45e5f95df92b.svg?invert_in_darkmode" align=middle width=55.52514pt height=22.831379999999992pt/> e <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/f4ea60c3008c6d6485e6a96f114e0c45.svg?invert_in_darkmode" align=middle width=47.30583pt height=22.831379999999992pt/>.

Consequentemente, o número de novos casos por dia e seu crescimento são modelados pela primeira e segunda derivada de <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/27099e26220f898359382d05f75b941c.svg?invert_in_darkmode" align=middle width=28.539060000000003pt height=24.65759999999998pt/>, respectivamente.

Assim, dado que estamos antes do pico <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/e8c65b8d4ccca28b6729b706fc85c469.svg?invert_in_darkmode" align=middle width=16.347210000000004pt height=22.831379999999992pt/> e que temos dados de casos acumulados observados, qual a melhor maneira de estimar esses três parâmetros de forma a obter uma boa estimativa de <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/e8c65b8d4ccca28b6729b706fc85c469.svg?invert_in_darkmode" align=middle width=16.347210000000004pt height=22.831379999999992pt/>?

# Proposta

Gostaríamos os parâmetros fossem bons o suficientes para que as três curvas fossem bem ajustadas. Com isso, sejam 

- <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/6912d7bdfdcd150df8520a69a02cef85.svg?invert_in_darkmode" align=middle width=34.65198pt height=24.65759999999998pt/> o número de casos acumulados  observados no tempo <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/4f4f4e395762a3af4575de74c019ebb5.svg?invert_in_darkmode" align=middle width=5.936155500000004pt height=20.222069999999988pt/>,
- <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/ee87b7d752fac334a1b3b505595de9b7.svg?invert_in_darkmode" align=middle width=34.65198pt height=24.65759999999998pt/> o número de novos casos observados no tempo <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/4f4f4e395762a3af4575de74c019ebb5.svg?invert_in_darkmode" align=middle width=5.936155500000004pt height=20.222069999999988pt/> e
- <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/9adfaa163a0ec720f200f135b373479c.svg?invert_in_darkmode" align=middle width=34.65198pt height=24.65759999999998pt/> o crescimento observado de novos casos no tempo <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/4f4f4e395762a3af4575de74c019ebb5.svg?invert_in_darkmode" align=middle width=5.936155500000004pt height=20.222069999999988pt/>,

vamos minimizar a função a seguir em relação <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/5e16cba094787c1a10e568c61c63a5fe.svg?invert_in_darkmode" align=middle width=11.872245000000005pt height=22.46574pt/>:

<p align="center"><img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/7832f959587b32c3111327d0faebb100.svg?invert_in_darkmode" align=middle width=700.2748499999999pt height=36.228555pt/></p>

sendo <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/c40772ff536329321c04e98faf3fa7bf.svg?invert_in_darkmode" align=middle width=71.218785pt height=14.155350000000013pt/> pesos pré-definidos (ou não). A princípio, propõe-se que <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/4b4518f1b7f0fb1347fa21506ebafb19.svg?invert_in_darkmode" align=middle width=18.321105000000006pt height=14.155350000000013pt/> seja o maior peso de todos para que a curva se ajuste melhor nos casos observados.

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
opt_korea<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/8376a40839fa5bb5d3a8d0abbc1fc897.svg?invert_in_darkmode" align=middle width=737.08305pt height=400.91172pt/>\phi_1 = 83000<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/255cfd9ac87fd4823df90f3b12d979a9.svg?invert_in_darkmode" align=middle width=7.6542015000000045pt height=14.155350000000013pt/>\phi_2=22<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/763bf2bc4a73d82f428439f7b7290f1b.svg?invert_in_darkmode" align=middle width=248.95150499999997pt height=47.67081pt/>casosAcumulados),
                22,
                4) ## chute completamente aleatório
opt_china = opt(data=dd_china,chute = chute_china, pesos=c(1,2,4))
opt_china<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/9e28af87c0557b8a1cdac4232908f74e.svg?invert_in_darkmode" align=middle width=732.9250499999999pt height=124.74858pt/>pars
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

Note que a curva de casos acumulados e novos casos não tem nenhuma indicação de que vai começar a desacelerar. Portanto, precisamos limitar os parâmetros conforme conhecimentos prévios e observações de cenários em outros países. Os chutes iniciais são bem ruins a princípio: <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/a51061288db09a7e3584414e08eccb1f.svg?invert_in_darkmode" align=middle width=88.401885pt height=22.831379999999992pt/> e <img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/4c98647d8d8ddfb67993f4d8147439c1.svg?invert_in_darkmode" align=middle width=55.52514pt height=22.831379999999992pt/>.


```r
chute_br = c(220000,#10*max(dd_br<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/53025b985131c1562009a936c6d6c1a1.svg?invert_in_darkmode" align=middle width=780.5968499999999pt height=47.671140000000015pt/>plot
```

<img src="modelo_derivadas_files/figure-html/br_opt-1.png" style="display: block; margin: auto;" />

### Predição 

Dados os chutes iniciais e os dados que temos, a data esperada de pico em 2020-05-02, com um total de casos estimados em 220000 (muito perto do chute inicial).




```r
dd_pred = opt_br<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/cc2f7fc89a6c59821c0bddf7463e8f68.svg?invert_in_darkmode" align=middle width=81.14699999999999pt height=22.831379999999992pt/>days = rep(seq_along(unique(dd_pred<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/263fd4c756015114fbf4c4f3563df5b2.svg?invert_in_darkmode" align=middle width=338.55838499999993pt height=48.858480000000014pt/>days)+1,
             to=max(dd_pred$days)+futuro)
dd_append = data.frame(data = rep(rep(dd_pred<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/b04623eab63234c7444d28b8d7884c03.svg?invert_in_darkmode" align=middle width=421.9045049999999pt height=24.65759999999998pt/>var),each=futuro),
                       observado = 0,
                       estimado = c(d0f(dias_fut, opt_br$pars), 
                                    d1f(dias_fut, opt_br$pars), 
                                    d2f(dias_fut, opt_br$pars) 
                                    ),
                       days = rep(dias_fut,3))
dd_append<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/8341485e600b76eb408e95f2b45f73c2.svg?invert_in_darkmode" align=middle width=85.37562pt height=22.831379999999992pt/>data[1] + dias_fut
dd_pred<img src="https://rawgit.com/in	git@github.com:labbcb/covid19unicamp/master/svgs/dd7827649178b57c1d329730812275cc.svg?invert_in_darkmode" align=middle width=222.371655pt height=22.831379999999992pt/>Dado = "Predito"
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
