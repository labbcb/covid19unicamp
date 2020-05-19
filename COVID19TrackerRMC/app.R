library(shiny)

jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = 'https://bcblab.shinyapps.io/covid19rmc/';});"

ui <- fluidPage(
    tags$head(tags$script(jscode)),
)

server <- function(input, output, session) {
    session$sendCustomMessage("mymessage", "mymessage")
}

shinyApp(ui,server)