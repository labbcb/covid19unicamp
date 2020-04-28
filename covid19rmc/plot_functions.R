plot_counts = function(input, the_y, choice_city, log_10){
  my_plot = input %>% filter(city %in% choice_city) %>% 
    ggplot(aes_string("date", the_y)) +
    geom_line(aes(colour=city)) +
    geom_point(aes(colour=city)) + theme_bw() +
    xlab("") + ylab("") +
    theme(legend.position="bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  if (FALSE){
    if (the_y == "confirmed"){
      my_plot = my_plot + ylab("Casos Confirmados")
    }else if (the_y == "deaths"){
      my_plot = my_plot + ylab("Óbitos")
    }else if (the_y == "CFR"){
      my_plot = my_plot + ylab("Letalidade")
    }
  }
  
  if (log_10)
    my_plot = my_plot + scale_y_log10()
  
  my_plot
}

get_pallete = function(this_var, scheme="RdYlGn", nbins=5, this_rev=TRUE){
  bins = quantile(na.omit(this_var),
                  seq(0, 1, length.out = nbins + 1))
  bins[1] = floor(bins[1])
  bins[nbins+1] = ceiling(bins[nbins+1])
  bins = unique(round(bins, 0))
  colorBin(scheme, domain=this_var, bins=bins,
           reverse=this_rev, right=TRUE)  
}

get_labels2 = function(cities, values){
  values = round(values, 2)
  values = sprintf("%0.2f", values)
  values[values == "NA"] = "N/A"
  values = str_replace(values, "\\.", ",")
  sprintf("<strong>%s</strong><br/>%s casos / 100.000 habitantes", cities, values ) %>%
    lapply(htmltools::HTML)
}

get_labels = function(input){
  pmap_chr(input %>%
             as.data.frame() %>%
             select(name_muni, confirmed, deaths,estimated_population_2019, CFR, cases100k),
           function(name_muni, confirmed, deaths, estimated_population_2019, CFR, cases100k){
             estimated_population_2019 = prettyNum(estimated_population_2019, big.mark=".", decimal.mark=",")
             CFR = prettyNum(round(CFR, 2), big.mark=".", decimal.mark=",")
             cases100k = prettyNum(round(cases100k, 2), big.mark=".", decimal.mark=",")
             sprintf("<strong>%s</strong><br/>Casos Confirmados: %s<br/>Óbitos: %s<br/>Habitantes: %s <br/>Fatalidade: %s&percnt;<br/>Casos por 100mil habitantes: %s",
                     name_muni, confirmed, deaths, estimated_population_2019, CFR, cases100k)
           }) %>% as.list() %>% lapply(htmltools::HTML)
}

create_dyn_map = function(input, var){
  pal = get_pallete(input[[var]])
  #  labels = get_labels(input[["name_muni"]],
  #                      input[[var]])
  labels = get_labels(input)
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

