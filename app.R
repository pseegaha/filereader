library(shiny)
library(DT)
library(data.table)
# Used for read_csv
library(readr)
# Used to check the file type
library(mime)
library(reactable)

csvFileServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      dataframe <- reactive({
        read.csv(userFile()$datapath,
                 header = input$heading,
                 quote = input$quote,
                 stringsAsFactors = stringsAsFactors)
      })
      
      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      # Return the reactive that yields the data frame
      return(dataframe)
    }
  )    
}

# Module UI function
csvFileUI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      csvFileUI("datafile", "User data (.csv format)")
    ),
    mainPanel(
      dataTableOutput("table")
      ,reactableOutput("reactab")
      
    )
  )
)

server <- function(input, output, session) {
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    data.table(datafile())
  })
  
  output$reactab <- renderReactable({
    reactable(datafile(),
              theme = theme <- reactableTheme(
                style = list(".dark &" = list(color = "#fff", background = "#282a36")),
                cellStyle = list(".dark &" = list(borderColor = "rgba(255, 255, 255, 0.15)")),
                headerStyle = list(".dark &" = list(borderColor = "rgba(255, 255, 255, 0.15)")),
                paginationStyle = list(".dark &" = list(borderColor = "rgba(255, 255, 255, 0.15)")),
                rowHighlightStyle = list(".dark &" = list(background = "rgba(255, 255, 255, 0.04)")),
                pageButtonHoverStyle = list(".dark &" = list(background = "rgba(255, 255, 255, 0.08)")),
                pageButtonActiveStyle = list(".dark &" = list(background = "rgba(255, 255, 255, 0.1)"))
              ),
              paginationType = "jump",
              striped = TRUE,
              resizable = TRUE,
              showSortable = TRUE,
              filterable = TRUE,
              defaultColDef = colDef(
                sortNALast = TRUE,
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8",borderColor = "#555")
              ),
              columns = list(
                Species = colDef(minWidth = 140)  # overrides the default
              ),
              bordered = TRUE,
              highlight = TRUE
    )
  })
}

runApp(shinyApp(ui, server),launch.browser = T)