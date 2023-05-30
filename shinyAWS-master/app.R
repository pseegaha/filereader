source("global.R")
source("module/shinyAWS.R")

## Basic App ##
ui <- fluidPage(awsUI("aws_ns"))
server <- function(input,output,session){
  rv <- reactiveValues()
  awsServer("aws_ns",rv)
}
app <- shinyApp(ui,server)
app %>% runApp() #%>% secure_app()
