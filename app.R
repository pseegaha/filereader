library(shiny)
library(DT)
library(data.table)
# Used for read_csv
library(readr)
# Used to check the file type
library(mime)
library(reactable)
library(highcharter)
library(htmltools)
library(shinyWidgets)

# hchart(mtcars, type="point", hcaes(x=mpg, y=cyl,color=hp,group=gear,variable=carb), name = "carb",dataLabels = list(enabled = F))

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
                text = "Preview",
                tabName = "tab1"
              ),
      menuItem(icon = icon('bar-chart-o'),
               # condition = "input.controlbarToggle == true",
               text = "Analytics",
               tabName = "tab2"
      )
            )
  )

    ,
  body =   dashboardBody(
        tabItems(
          tabItem(
            tabName = "tab1",
            # tags$table(
            #   class='table table-bordered km-table',
            #   tags$tr(class='w-2',
            #           tags$td(align = "center",class='w-25',
            #                   pickerInput(
            #                     inputId = "p2",
            #                     label = "Select all option / custom text",
            #                     choices = rownames(mtcars),
            #                     multiple = TRUE,
            #                     options = list(
            #                       `actions-box` = TRUE,
            #                       `deselect-all-text` = "None...",
            #                       `select-all-text` = "Yeah, all !",
            #                       `none-selected-text` = "zero"
            #                     )
            #                   )
            #           ),
            #           tags$td(align = "center",class='w-25',
            #                   pickerInput(
            #                     inputId = "p2",
            #                     label = "Select all option / custom text",
            #                     choices = rownames(mtcars),
            #                     multiple = TRUE,
            #                     options = list(
            #                       `actions-box` = TRUE,
            #                       `deselect-all-text` = "None...",
            #                       `select-all-text` = "Yeah, all !",
            #                       `none-selected-text` = "zero"
            #                     )
            #                   )
            #           ),
            #           tags$td(align = "center",class='w-25',
            #                   pickerInput(
            #                     inputId = "p2",
            #                     label = "Select all option / custom text",
            #                     choices = rownames(mtcars),
            #                     multiple = TRUE,
            #                     options = list(
            #                       `actions-box` = TRUE,
            #                       `deselect-all-text` = "None...",
            #                       `select-all-text` = "Yeah, all !",
            #                       `none-selected-text` = "zero"
            #                     )
            #                   )
            #           ),
            #           tags$td(align = "center",class='w-25',
            #                   pickerInput(
            #                     inputId = "p2",
            #                     label = "Select all option / custom text",
            #                     choices = rownames(mtcars),
            #                     multiple = TRUE,
            #                     options = list(
            #                       `actions-box` = TRUE,
            #                       `deselect-all-text` = "None...",
            #                       `select-all-text` = "Yeah, all !",
            #                       `none-selected-text` = "zero"
            #                     )
            #                   )
            #           )
            #   )
            # ),

            #   tags$table(
            #     class='table table-bordered km-table',
            #                        tags$tr(class='w-2',
            #                          tags$td(align = "center",class='w-25',
            #                                  valueBoxOutput("vbox1", width = 12)
            #                          ),
            #                          tags$td(align = "center",class='w-25',
            #                                  valueBoxOutput("vbox2", width = 12)
            #                          ),
            #                          tags$td(align = "center",class='w-25',
            #                                  valueBoxOutput("vbox3", width = 12)
            #                          ),
            #                          tags$td(align = "center",class='w-25',
            #                                  valueBoxOutput("vbox4", width = 12)
            #                          )
            # 
            #                      )
            # ),
          fluidRow(
              box(width = 12,
                  # Simple theme toggle button
                  tags$button(onclick = "document.querySelector('.themeable-tbl').classList.toggle('dark')",
                              "Toggle light/dark"),
                   tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('cars-table')"),
                  div(class = "themeable-tbl light", reactableOutput("reactab"))
                                
              )
          # ,box(width = 12,
          #      highchartOutput("hc_chart", height = "500px")
          # )
          )
            
          )
          ,tabItem(
            tabName = "tab2",
            tags$table(
              class='table table-bordered km-table',
              tags$tr(class='w-2',
                      tags$td(align = "center",class='w-25',
                              pickerInput(
                                inputId = "p2",
                                label = "Select the Subjects",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "Deselect All",
                                  `select-all-text` = "Select All",
                                  `none-selected-text` = "Select Subjects"
                                )
                              )
                      )
                      ,tags$td(align = "center",class='w-25',
                              pickerInput(
                                inputId = "p3",
                                label = "Select the Visits",
                                choices = rownames(mtcars),
                                multiple = TRUE,
                                options = list(
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "Deselect All",
                                  `select-all-text` = "Select All",
                                  `none-selected-text` = "Select Visits"
                                )
                              )
                      )
                      # ,tags$td(align = "center",class='w-25',
                      #         pickerInput(
                      #           inputId = "p2",
                      #           label = "Select all option / custom text",
                      #           choices = rownames(mtcars),
                      #           multiple = TRUE,
                      #           options = list(
                      #             `actions-box` = TRUE,
                      #             `deselect-all-text` = "None...",
                      #             `select-all-text` = "Yeah, all !",
                      #             `none-selected-text` = "zero"
                      #           )
                      #         )
                      # ),
                      # tags$td(align = "center",class='w-25',
                      #         pickerInput(
                      #           inputId = "p2",
                      #           label = "Select all option / custom text",
                      #           choices = rownames(mtcars),
                      #           multiple = TRUE,
                      #           options = list(
                      #             `actions-box` = TRUE,
                      #             `deselect-all-text` = "None...",
                      #             `select-all-text` = "Yeah, all !",
                      #             `none-selected-text` = "zero"
                      #           )
                      #         )
                      # )
              )
            ),
            tags$table(
              class='table table-bordered km-table',
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
              )
            )
            ,fluidRow(
              # box(width = 12,
              #     tags$button(onclick = "document.querySelector('.themeable-tbl').classList.toggle('dark')",
              #                 "Toggle light/dark"),
              #     tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('cars-table')"),
              #     div(class = "themeable-tbl light", reactableOutput("reactab"))
              #     
              # ),
              box(width = 12,
                   highchartOutput("hc_chart", height = "500px")
              )
            )
            
          )
        )
  )
,controlbar = dashboardControlbar(
  id = "controlbar",
  collapsed = FALSE,
  overlay = TRUE
  ,csvFileUI("datafile", "Data Uploader(.csv)")
  ,uiOutput("selected_row_details")
  ,verbatimTextOutput("table_state")


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
  observe({
    updatePickerInput(
      session = session, inputId = "p2",
      choices = datafile()$`Subject.ID`, selected = NULL
    )
    
    updatePickerInput(
      session = session, inputId = "p3",
      choices = datafile()$`Visit.Date`, selected = NULL
    )
  })
  


  
  output$vbox1 <- renderValueBox({
    valueBox(
      value = 150,
      subtitle = "Subjects",
      color = "primary",
      icon = icon("shopping-cart")
      # ,href = "#"
      ,footer = div("WIP")
    )
  })
  
  output$vbox2 <- renderValueBox({
    
  valueBox(
    value = 150,
    subtitle = "Site",
    color = "primary",
    icon = icon("cart-shopping")
    ,footer = div("WIP")
  )
  })
  
  output$vbox3 <- renderValueBox({
      
  valueBox(
    value = "53%",
    subtitle = "Country",
    color = "indigo",
    icon = icon("gears")
    ,footer = div("WIP")
  )
    })
    
      output$vbox4 <- renderValueBox({
        
  valueBox(
    value = "44",
    subtitle = "Site",
    color = "teal",
    icon = icon("sliders")
    ,footer = div("WIP")
  )
      })
      
      output$vbox5 <- renderValueBox({
        
        valueBox(
          value = "44",
          subtitle = "Shipped Count",
          color = "teal",
          icon = icon("sliders")
          ,footer = div("WIP")
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
              showPageSizeOptions = TRUE,
              selection = "multiple",
              onClick = "select",
              theme = theme <- reactableTheme(
                style = list(".dark &" = list(color = "#fff", background = "#282a36")
                             ),
                cellStyle = list(".dark &" = list(borderColor = "grey",fontcolor = "white",fontSize = "11px")
                                 ,".light &" = list(borderColor = "grey",fontcolor = "black",fontSize = "11px")
                                 ),
                headerStyle = list(".dark &" = list(borderColor = "grey",fontSize = "14px")
                                   ,".light &" = list(borderColor = "grey",fontSize = "14px")
                                   ),
                paginationStyle = list(".dark &" = list(color = "black",borderColor = "rgba(255, 255, 255, 0.15)")),
                rowHighlightStyle = list(".dark &" = list(background = "grey")
                                         ,".light &" = list(background = "grey")
                                         ),
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
                # style = function(value) list(value,fontWeight = 20),
                align = "center",
                minWidth = 100
                # ,headerStyle = list(background = "#f7f7f8",borderColor = "#555")
              ),

              columns = list(
                .rownames = colDef(name = "Subject ID", sortable = TRUE, align = "left"),
                character = colDef(
                # Show species under character names
                  cell = function(value) {

                    div(
                      div(style = "font-weight: 5", value)
                      )
                  }
                ),
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
  
  output$table_state <- renderPrint({
    state <- req(getReactableState("reactab"))
    print(state)
    print(names(state$sorted))
    print(names(datafile()[1]))
    
  })
  
  output$hc_chart <- renderHighchart({
    # db_hc <-
    #   db[input$reactab_rows_selected, ] # filter dataset according to select rows in my_dt
     # db_hcx<-names(datafile()[1])
     # db_hcy<-names(state$sorted) 
    hc <- hchart(datafile(), type="bar", hcaes(x=Subject.ID,y=Site ), name = "carb",dataLabels = list(enabled = F))
    
    # %>% hc_add_series(name = "series1", data = db_hc$mpg) %>%
    #   hc_add_series(name = "series2", data = db_hc$wt)
    hc
  })
  
  
  output$selected_row_details <- renderUI({
    selected <- getReactableState("reactab", "selected")
    req(selected)
    rowdetails <- datafile()[selected,]

    tagList(
      h2("Selected row details"),
      tags$pre(
        paste(capture.output(print(rowdetails, width = 200)), collapse = "\n")
      )
    )
  })
}

runApp(shinyApp(ui, server),launch.browser = T)