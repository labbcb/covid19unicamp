Modelando o Número Total de Casos de COVID-19 para o Brasil
================

## Introdução

Uma estratégia bastante comum para modelar curvas de crescimento é o
emprego da função logística ou sigmóide. Neste caso:

![](https://render.githubusercontent.com/render/math?math=%5Cfrac%7B%5Cphi_1%7D%7B1%20%2B%20%5Cexp%5Cleft%5C%7B%5Cfrac%7B%5Cphi_2%20-%20x%7D%7B%5Cphi_3%7D%5Cright%5C%7D%7D)

Nesta expressão, o parâmetro phi1 é a assíntota da curva (número máximo
de casos), phi2 é o tempo em que atinge-se a metade dos casos e 1/phi3 é
a velocidade de crescimento da função logística.

## Ajuste para Brasil

| date       | confirmed | deaths | d |
| :--------- | --------: | -----: | -: |
| 2020-02-26 |         1 |      0 | 0 |
| 2020-02-27 |         1 |      0 | 1 |
| 2020-02-28 |         1 |      0 | 2 |
| 2020-02-29 |         2 |      0 | 3 |
| 2020-03-01 |         2 |      0 | 4 |
| 2020-03-02 |         2 |      0 | 5 |

![](logistica_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->![](logistica_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->![](logistica_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

|      |            x |
| ---- | -----------: |
| phi1 | 34291.008570 |
| phi2 |    42.964390 |
| phi3 |     5.677742 |

Estimativa do pico: 2020-04-08.

## Estado de São Paulo

``` r
indata = brasilio() %>%
  filter(place_type == "state", state=="SP") %>%
  select(date, confirmed, deaths) %>%
  ungroup() %>% prepData()
indata %>% head() %>% knitr::kable()
```

| date       | confirmed | deaths | d |
| :--------- | --------: | -----: | -: |
| 2020-02-25 |         1 |      0 | 0 |
| 2020-02-26 |         1 |      0 | 1 |
| 2020-02-27 |         1 |      0 | 2 |
| 2020-02-28 |         2 |      0 | 3 |
| 2020-02-29 |         2 |      0 | 4 |
| 2020-03-01 |         2 |      0 | 5 |

![](logistica_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->![](logistica_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->![](logistica_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

|      |            x |
| ---- | -----------: |
| phi1 | 11245.066449 |
| phi2 |    41.561301 |
| phi3 |     5.020329 |

Estimativa do pico: 2020-04-06.

## Observação

O modelo precisa ser melhorado, pois as estimativas de pico não estão
apropriadas.
