# shinyAWS
Shiny Modules for AWS Integrations

![Alt text](images/imgfile.png?raw=true "Title")

| Package | Description | Status |
|---|---|---|
| [shinySQL](https://github.com/apprazv/shinySQL) | 💡 Shiny Modules to Enable Database Pulls in Shiny Apps | WIP |
| [shinyAWS](https://github.com/apprazv/shinyAWS) | ⏬ Shiny Module for AWS| WIP |
| [shinyEDA](https://github.com/apprazv/shinyEDA) | ✅ Shiny Module for Exploratory Data Analysis | WIP |
| [shinyCRM](https://github.com/apprazv/shinyCRM/) | 📝 Shiny based CRM | WIP |

```r
library(DBI)
library(dplyr)
library(shiny)
library(DT)
```
```{r}
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
```