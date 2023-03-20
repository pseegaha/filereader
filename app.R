library(shiny)
library(DT)
library(data.table)
# Used for read_csv
library(readr)
# Used to check the file type
library(mime)
library(reactable)
library(highcharter)

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
        read.csv(userFile()$datapath
                 ,stringsAsFactors = stringsAsFactors)
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
    fileInput(ns("file"), label)
  )
}


library(bs4Dash)
ui <- dashboardPage(
  header = dashboardHeader(title =dashboardBrand(
    title = "Dashboard",
    color = "white",
    href = "https://www.intelliatx.com/",
    image = "https://www.intelliatx.com/wp-content/themes/intelliatx/assets/images/favicon.png"
  ),
                           sidebarIcon = shiny::icon("bars"),
                           controlbarIcon = shiny::icon("th")
                           ),
  sidebar = dashboardSidebar(
    skin = "dark",
    status = "primary",
    elevation = 4,
    collapsed = FALSE,
    minified = TRUE,
    expandOnHover = TRUE,
    fixed = TRUE,
    sidebarMenu(

      id = "sidebarMenu",
              menuItem(icon = icon('table'),
                # condition = "input.controlbarToggle == true",
                text = "Tab 1",
                tabName = "tab1"
              )
            )
  )

    ,
  body =   dashboardBody(
        tabItems(
          tabItem(
            tabName = "tab1",
            
            # box(width = 12,
            # tags$head(
              tags$table(
                class='table table-bordered km-table',
              # border = 5,
                                 # tags$thead(
                                 #   tags$tr(
                                 #     tags$th(colspan = 4, height = 100, width = 800,
                                 #             align = "center", "TABLE TITLE")
                                 #   )
                                 # ),
                                   tags$tr(class='w-2',
                                     tags$td(align = "center",class='w-25',
                                             valueBoxOutput("vbox1", width = 12)
                                     ),
                                     tags$td(align = "center",class='w-25',
                                             valueBoxOutput("vbox2", width = 12)
                                     ),
                                     tags$td(align = "center",class='w-25',
                                             valueBoxOutput("vbox3", width = 12)
                                     ),
                                     tags$td(align = "center",class='w-25',
                                             valueBoxOutput("vbox4", width = 12)
                                     )
                                   
                                   # ,tags$tr(
                                   #   tags$td(align = "center", "Data 1"),
                                   #   tags$td(align = "center", "Data 2")
                                   # )
                                 )
            )
            # )
          # )
          
          
            
            # fluidRow(
            #   column(width = 3, align = 'center',
            #          valueBoxOutput("vbox1", width = 4)
            #          )
            #   ,column(width = 3, align = 'center',
            #           valueBoxOutput("vbox2", width = 4))
            #   ,column(width = 3, align = 'center',
            #           valueBoxOutput("vbox3", width = 4)
            #   )
            #   ,column(width = 3, align = 'center',
            #           valueBoxOutput("vbox4", width = 4
            #                          )
            #   )
            # )
              # actionButton(inputId = "controlbarToggle", label = "Toggle Controlbar"),
              ,box(width = 12,
                  # Simple theme toggle button
                  tags$button(onclick = "document.querySelector('.themeable-tbl').classList.toggle('dark')",
                              "Toggle light/dark"),
                   tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('cars-table')"),
                  div(class = "themeable-tbl dark", reactableOutput("reactab"))
                                
              )
          ,box(width = 12,
               highchartOutput("hc_chart", height = "500px")
          )
            
          )
        )
  )
,controlbar = dashboardControlbar(
  id = "controlbar",
  collapsed = FALSE,
  overlay = TRUE
  ,    csvFileUI("datafile", "Data Uploader(.csv)")

),
title = "updateControlbar"
)

# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       csvFileUI("datafile", "User data (.csv format)")
#     ),
#     mainPanel(
#       dataTableOutput("table")
#       ,reactableOutput("reactab")
#       
#     )
#   )
# )

server <- function(input, output, session) {
  
  # observeEvent(input$controlbarToggle, {
  #   updateControlbar(id = "controlbar", session = session)
  # })
  output$hc_chart <- renderHighchart({
    # db_hc <-
    #   db[input$my_dt_rows_selected, ] # filter dataset according to select rows in my_dt
    db_hc<-mtcars
    hc <- highchart() %>%
      hc_add_series(name = "mpg", data = db_hc$mpg) %>%
      hc_add_series(name = "wt", data = db_hc$wt)
    hc
  })
  
  output$vbox1 <- renderValueBox({
    valueBox(
      value = 150,
      subtitle = "New orders",
      color = "primary",
      icon = icon("shopping-cart"),
      href = "#"
    )
  })
  
  output$vbox2 <- renderValueBox({
    
  valueBox(
    value = 150,
    subtitle = "New orders",
    color = "primary",
    icon = icon("cart-shopping")
  )
  })
  
  output$vbox3 <- renderValueBox({
      
  valueBox(
    value = "53%",
    subtitle = "New orders",
    color = "indigo",
    icon = icon("gears"),
    footer = div("Hello World")
  )
    })
    
      output$vbox4 <- renderValueBox({
        
  valueBox(
    value = "44",
    subtitle = "User Registrations",
    color = "teal",
    icon = icon("sliders")
  )
      })
      
      output$vbox5 <- renderValueBox({
        
        valueBox(
          value = "44",
          subtitle = "User Registrations",
          color = "teal",
          icon = icon("sliders")
        )
      })
      
  
  output$ibox <- renderInfoBox({
    infoBox(
      title = "Comments",
      fill = TRUE,
      gradient = TRUE,
      color = "success",
      value = 41410,
      icon = icon("comments")
    )
  })
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    data.table(datafile())
  })
  
  output$reactab <- renderReactable({
    reactable(datafile(),rownames = F,elementId = "cars-table",
              theme = theme <- reactableTheme(
                style = list(".dark &" = list(color = "#fff", background = "#282a36")),
                cellStyle = list(".dark &" = list(color = "black",borderColor = "rgba(255, 255, 255, 0.15)")),
                headerStyle = list(".dark &" = list(borderColor = "rgba(255, 255, 255, 0.15)")),
                paginationStyle = list(".dark &" = list(borderColor = "rgba(255, 255, 255, 0.15)")),
                rowHighlightStyle = list(".dark &" = list(background = "rgba(255, 255, 255, 0.04)")),
                pageButtonHoverStyle = list(".dark &" = list(background = "rgba(255, 255, 255, 0.08)")),
                pageButtonActiveStyle = list(".dark &" = list(background = "rgba(255, 255, 255, 0.1)"))
              ),
              paginationType = "jump",
               striped = F,
              resizable = TRUE,
              showSortable = TRUE,
              filterable = TRUE,
              defaultColDef = colDef(
                sortNALast = TRUE,
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 140
                # ,headerStyle = list(background = "#f7f7f8",borderColor = "#555")
              ),
              columns = list(

                mpg = colDef(
                  sticky = "left",
                  style = list(borderLeft = "1px solid #eee"),
                  headerStyle = list(borderLeft = "1px solid #eee")
                )
              ),
              bordered = TRUE,
              highlight = TRUE
    )
  })
}

runApp(shinyApp(ui, server),launch.browser = T)