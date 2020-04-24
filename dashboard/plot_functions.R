library(ggplot2)
library(sf)
## considerar pacote geobr para mapa estatico

## either data from get_data_state or get_data_city
plot_accumulated_day <- function(data) {
  data %>%
    gather(type, counts,-date) %>%
    ggplot(aes(x = date, y = counts, color = type)) +
    geom_line() +
    geom_point() +
    labs(x = "Data", y = "Contagem", color = "Tipo")
}

## either data from get_data_state or get_data_city
plot_tax_increase <- function(data) {
  data %>%
    select(-deaths) %>%
    mutate(
      dx = as.integer(date - lag(date, default = date[1])),
      dconf = confirmed - lag(confirmed, default = confirmed[1]),
      rate = dconf / dx
    ) %>%
    ggplot(aes(date, rate)) +
    geom_line() +
    geom_point() +
    labs(x = "Data", y = "Taxa")
}

plot_brazil_map <- function(map_data, opt_type) {
  this_map = map_data %>%
    ggplot(aes_string(fill = opt_type)) + geom_sf() +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(8, "OrRd"))
  this_title = switch(opt_type,
                      confirmed1m = "Casos confirmados por milhão de habitantes",
                      deaths1m = "Óbitos por milhão de habitantes",
                      CFR = "Taxa de fatalidade entre confirmados")
  this_map + ggtitle(this_title)
}