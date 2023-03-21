# hchart(mtcars, type="point", hcaes(x=mpg, y=cyl,color=hp,group=gear,variable=carb), name = "carb",dataLabels = list(enabled = F))
# Run in an interactive R session
if (interactive()) {
  library(shiny)
  library(reactable)
  library(htmltools)
  ui <- fluidPage(
    actionButton("prev_page_btn", "Previous page"),
    actionButton("next_page_btn", "Next page"),
    reactableOutput("reactab"),
    verbatimTextOutput("table_state"),
    uiOutput("selected_row_details")
  )
  server <- function(input, output) {
    output$reactab <- renderReactable({
      reactable(
        MASS::Cars93[, 1:5],
        showPageSizeOptions = TRUE,
        selection = "multiple",
        onClick = "select"
      )
    })
    output$table_state <- renderPrint({
      state <- req(getReactableState("reactab"))
      print(state)
      print(names(state$sorted))
    })
    observeEvent(input$prev_page_btn, {
      # Change to the previous page
      page <- getReactableState("reactab", "page")
      if (page > 1) {
        updateReactable("reactab", page = page - 1)
      }
    })
    observeEvent(input$next_page_btn, {
      # Change to the next page
      state <- getReactableState("reactab")
      if (state$page < state$pages) {
        updateReactable("reactab", page = state$page + 1)
      }
    })
    output$selected_row_details <- renderUI({
      selected <- getReactableState("reactab", "selected")
      req(selected)
      details <- MASS::Cars93[selected, -c(1:5)]
      tagList(
        h2("Selected row details"),
        tags$pre(
          paste(capture.output(print(details, width = 1200)), collapse = "\n")
        )
      )
    })
  }
  runApp(shinyApp(ui, server),launch.browser = T)
  }