library(sf)
## library(highcharter)
library(ggplot2)

## usando estes pkgs para o mapa dinamico
library(leaflet)


## either data from get_data_state or get_data_city
plot_cumulative_cases <- function(data) {
  data %>%
    select(date, confirmed, deaths) %>% 
    gather(type, counts, -date)  %>%
    mutate(type = ifelse(type == "confirmed", "Casos Confirmados", "Óbitos")) %>%
    rename(Contagem = counts, Data = date) %>%
    ggplot(aes(Data, Contagem, colour=type)) +
    geom_point() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_discrete(name="")
}

## either data from get_data_state or get_data_city
plot_daily_cases <- function(data) {
  data %>%
    select(-deaths) %>%
    rename(Data = date, Casos=confirmed_day) %>%
    ggplot(aes(Data)) + 
    geom_bar(aes(weight=Casos)) +
    ylab("Novos Casos por Dia") +
    theme_minimal()
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

## Mapas dinamicos
get_pallete = function(this_var, scheme="RdYlGn", nbins=5, this_rev=TRUE){
  bins = quantile(na.omit(this_var),
                  seq(0, 1, length.out = nbins + 1))
  bins[1] = floor(bins[1])
  bins[nbins+1] = ceiling(bins[nbins+1])
  bins = unique(round(bins, 0))
  colorBin(scheme, domain=this_var, bins=bins,
           reverse=this_rev, right=TRUE)  
}

get_labels = function(cities, values){
  values = round(values, 2)
  values = sprintf("%0.2f", values)
  values[values == "NA"] = "N/A"
  values = str_replace(values, "\\.", ",")
  sprintf("<strong>%s</strong><br/>%s casos / 100.000 habitantes", cities, values ) %>%
    lapply(htmltools::HTML)
}

create_dyn_map = function(input, var){
  pal = get_pallete(input[[var]])
  labels = get_labels(input[["name_muni"]],
                      input[[var]])
  form1 = as.formula(paste0("~pal(", var, ")"))
  form2 = as.formula(paste0("~", var))
  input %>% leaflet() %>% addTiles() %>% 
    addPolygons(fillColor=form1, weight=2, opacity=1, color="white",
                dashArray="3", fillOpacity=0.7,
                highlight=highlightOptions(
                  weight=5, color="#666", dashArray="",
                  fillOpacity=0.7, bringToFront=TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style=list("font-weight"="normal", padding="3px 8px"),
                  textsize = "15px", direction = "auto")) %>% 
    addLegend(pal=pal, values=form2, opacity=0.7, title="",
              position = "bottomright")
}

create_dyn_map2 = function(input, var){
  ## a ideia era poder trocar as variaveis que dao os valores das cores
  pal = get_pallete(input[[var]])
  labels = get_labels(input[["name_muni"]],
                      input[[var]])
  form1 = as.formula(paste0("~pal(", var, ")"))
  form2 = as.formula(paste0("~", var))
  input %>% leaflet() %>%
    addTiles(group="Casos confirmados por 100 mil habitantes") %>% 
    addProviderTiles(input[['confirmed']], group="Casos confirmados") %>% 
    addProviderTiles(input[['deaths']], group="Óbitos") %>% 
    addPolygons(fillColor=form1, weight=2, opacity=1, color="white",
                dashArray="3", fillOpacity=0.7,
                highlight=highlightOptions(
                  weight=5, color="#666", dashArray="",
                  fillOpacity=0.7, bringToFront=TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style=list("font-weight"="normal", padding="3px 8px"),
                  textsize = "15px", direction = "auto")) %>% 
    addLegend(pal=pal, values=form2, opacity=0.7, title="",
              labFormat = labelFormat(digits=2),
              position = "bottomright")
}