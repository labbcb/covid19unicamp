library(shinydashboard)
library(dplyr)
library(ggplot2)
library(datacovidbr)
library(tidyr)
library(brazilmaps)
library(sf)
## considerar pacote geobr para mapa estatico

data <- brasilio(silent = TRUE) %>%
  arrange(date)

## dados para mapa
map_data = data %>% 
  filter(place_type == "state", is_last) %>% 
  select(date, state, confirmed, deaths, estimated_population_2019,
         city_ibge_code) %>% 
  mutate(CFR = deaths/confirmed*100,
         deaths1m = deaths/estimated_population_2019*1e6,
         confirmed1m = confirmed/estimated_population_2019*1e6)
pal = RColorBrewer::brewer.pal(8, "OrRd")


map_data = get_brmap("State") %>%
  inner_join(map_data, by=c("State"="city_ibge_code"))

get_data_city <- function(keep_city = "São Paulo") {
  data %>% 
    filter(place_type == "city" & city == keep_city) %>%
    select(date, confirmed, deaths) %>%
    mutate(deaths = ifelse(is.na(deaths), 0, deaths))
}

get_last_date <- function() {
  format(last(data$date), "%d/%m/%Y")
}

# x must be ordered by date
get_today_increase_text <- function(x) {
  new_value <- last(x) - nth(x, -2)
  percent_value <- ceiling(new_value / last(x) * 100)
  percent_text <- paste0("(+", percent_value, "%)")
  paste("+", new_value, percent_text)
}

server <- function(input, output) {
  data_city <- get_data_city()
  
  output$confirmed_total <- renderValueBox({
    valueBox(last(data_city$confirmed), subtitle = "Total de Casos")
  })
  
  output$info_confirmed = renderInfoBox({
    infoBox(
      "Confirmados", last(data_city$confirmed), icon=icon("plus-square"), color="orange"
    )
    })

  output$info_deaths = renderInfoBox({
    infoBox(
      "Óbitos", last(data_city$deaths), icon=icon("bible"), color="red"
    )
  })
  
    
  output$confirmed_today <- renderValueBox({
    get_today_increase_text(data_city$confirmed) %>%
      valueBox(subtitle = "Novos Casos")
  })
  
  output$deaths_total <- renderValueBox({
    valueBox(last(data_city$deaths), subtitle = "Total de Mortes")
  })
  
  output$deaths_today <- renderValueBox({
    get_today_increase_text(data_city$deaths) %>%
      valueBox(subtitle = "Novas Mortes")
  })
  
  output$accumulated <- renderPlot({
    data_city %>%
      gather(type, counts, -date) %>%
      ggplot(aes(x = date, y = counts, color = type)) +
        geom_line() + 
        geom_point() +
        labs(x = "Data", y = "Contagem", color = "Tipo")
  })
  
  output$increase_rate <- renderPlot({
    data_city %>%
      select(-deaths) %>%
      mutate(dx=as.integer(date-lag(date, default=date[1])),
             dconf=confirmed-lag(confirmed, default=confirmed[1]),
             rate = dconf/dx) %>%
      ggplot(aes(date, rate)) + 
        geom_line() + 
        geom_point() +
        labs(x = "Data", y = "Taxa")
  })
  
  output$map = renderPlot({
    opt_type = input$mapBrChoice
    this_map = map_data %>%
      ggplot(aes_string(fill=opt_type)) + geom_sf() +
      theme_minimal() +
      scale_fill_gradientn(colours=pal)
    this_title = switch(opt_type,
                        confirmed1m="Casos confirmados por milhão de habitantes",
                        deaths1m="Óbitos por milhão de habitantes",
                        CFR="Taxa de fatalidade entre confirmados")
    this_map + ggtitle(this_title)
})
  
  
}

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Campinas", tabName="campinas", icon=icon("map")),
      menuItem("São Paulo", tabName="saopaulo", icon=icon("city")),
      menuItem("Brasil", tabName = "brasil", icon=icon("globe-americas"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      ## Campinas
      tabItem(tabName = "campinas",
              h2("Campinas")
              ),
      
      tabItem(tabName = "saopaulo",
              fluidRow(
                box(
                  # TODO: verificar data da ultima atualização
                  title = paste0("Números do Corona em São Paulo (", get_last_date(),")"),
                  width = 12,
                  valueBoxOutput("confirmed_total"),
                  valueBoxOutput("confirmed_today"),
                  valueBoxOutput("deaths_total"),
                  valueBoxOutput("deaths_today")
                  )
                ),
              box(
                title = "Números acumulados de casos confirmados e mortes por data",
                plotOutput("accumulated")
                ),
              box(
                title = "Taxa de aumento em relação ao dia anterior",
                plotOutput("increase_rate")
                ),
              fluidRow(
                infoBoxOutput("info_confirmed"),
                infoBoxOutput("info_deaths")
              )
              ),
      
      tabItem(tabName = "brasil",
              h2("Brasil"),
              fluidRow(
                box(
                  title = "",
                  radioButtons("mapBrChoice", "Métrica:",
                               c("Casos Confirmados"="confirmed1m",
                               "Óbitos"="deaths1m",
                               "CFR"="CFR")),
                  plotOutput("map")
                )
                
              )
      )
    )
    
    
))

shinyApp(ui = ui, server = server)