library(sf)
library(highcharter)
library(ggplot2)

## usando estes pkgs para o mapa dinamico
library(leaflet)


## either data from get_data_state or get_data_city
plot_cumulative_cases <- function(data) {
  data %>%
    gather(type, counts,-date)  %>%
    mutate(type = ifelse(type == "confirmed", "Casos confirmados", "Mortes")) %>%
    rename(Contagem = counts, Data = date) %>%
    hchart("line", hcaes(Data, Contagem, group = type))
}

## either data from get_data_state or get_data_city
plot_tax_increase <- function(data) {
  data %>%
    select(-deaths) %>%
    mutate(
      dx = as.integer(date - lag(date, default = date[1])),
      dconf = confirmed - lag(confirmed, default = confirmed[1]),
      Taxa = dconf / dx
    ) %>%
    rename(Data = date) %>%
    hchart("line", hcaes(Data, Taxa))
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