library(shinydashboard)
library(tidyverse)
library(datacovidbr)

data <- brasilio(silent = TRUE) %>%
  arrange(date)

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
}

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
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
    )
))

shinyApp(ui = ui, server = server)