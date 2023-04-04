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

# selectPointsByDrag
s1 <- JS("/**
         * Custom selection handler that selects points and cancels the default zoom behaviour
         */
         function selectPointsByDrag(e) {
           var xArr = []
           // Select points
           Highcharts.each(this.series, function (series) {
             Highcharts.each(series.points, function (point) {
               if (point.x >= e.xAxis[0].min && point.x <= e.xAxis[0].max &&
                   point.y >= e.yAxis[0].min && point.y <= e.yAxis[0].max) {
                 xArr.push(point.x);
                 point.select(true, true);

               }
             });
           });
           Shiny.onInputChange('R_xArr', xArr);

           // Fire a custom event
           Highcharts.fireEvent(this, 'selectedpoints', { points: this.getSelectedPoints() });

           return false; // Don't zoom
           }")

# unselectByClick
s2 <- JS("/**
         * On click, unselect all points
         */
         function unselectByClick() {
           var points = this.getSelectedPoints();
           if (points.length > 0) {
             Highcharts.each(points, function (point) {
               point.select(false);
             });
           }
         }")

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
      # menuSubItem(
      #   csvFileUI("datafile1","Upload & Preview")
      #   ,
      #   tabName = "tab1"
      # ),
              menuItem(icon = icon('table'),
                # condition = "input.controlbarToggle == true",
                text = "Preview & Upload",
                tabName = "tab1"


              ),

      
      menuItem(icon = icon('bar-chart-o'),
               # condition = "input.controlbarToggle == true",
               text = "Analytics",
               tabName = "tab2"
      )
      # ,menuSubItem(
      #   text,
      #   tabName = NULL,
      #   href = NULL,
      #   newTab = NULL,
      #   icon = shiny::icon("angles-right"),
      #   selected = NULL
      # )
            )
  )

    ,
  body =   dashboardBody(
     tags$script('$(function(){$(\'[data-value="button1"]\').click(function(){$("#controlbar").css("color", "grey")})})'),
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
            wellPanel(style="align:center;background:white",
            # fluidRow(column(width=12, align = "center",
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
                                  `live-search`=TRUE,
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
                                label = "Select the Visit Name",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(
                                  `live-search`=TRUE,
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "Deselect All",
                                  `select-all-text` = "Select All",
                                  `none-selected-text` = "Select Visits"
                                )
                              )
                      )
              )
            # )
            # )
                      , 

                              
            fluidRow(column(width=12, align = "center",
                            div(style="display:inline-block; width: 220px;height: 75px;",
                              airDatepickerInput(
                        inputId = "multiple1",
                        label = "Select first visit date:",
                        placeholder = "Pick the first",
                        multiple = 1, clearButton = TRUE
                      )
                              )
                      , 
                      div(style="display:inline-block; width: 220px;height: 75px;",
                          
                      airDatepickerInput(
                        inputId = "multiple2",
                        label = "Select last visit date:",
                        placeholder = "Pick the last",
                        multiple = 1, clearButton = TRUE
                      )
                      )
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
                   highchartOutput("hc_chart", height = "500px"),
                  tableOutput("view")
              ),
              box(width = 12,
                  # Simple theme toggle button
                  tags$button(onclick = "document.querySelector('.themeable-tbl2').classList.toggle('dark')",
                              "Toggle light/dark"),
                  tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('cars-table2')"),
                  div(class = "themeable-tbl2 light", reactableOutput("reactab2"))
                  
              )
            )
            
          )
        )
  )
,controlbar = dashboardControlbar(
  id = "controlbar",
  width = 600,
  collapsed = FALSE,
  overlay = TRUE
  ,pinned = FALSE
  # ,controlbarItem(id = "button1", title = "button1"
  #                 ,csvFileUI("datafile1", NULL))
  ,fluidRow(column(width=12,align='center',tags$h4("Preview & Upload")
  ,csvFileUI("datafile", NULL)))
  ,
                   wellPanel(style="background:white;text-align:center",
                   uiOutput("selected_row_details")
  ,verbatimTextOutput("table_state"))
  


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
      choices = datafile()$`Visit.Name`, selected = NULL
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
              # selection = "multiple",
              # onClick = "select",
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
                )
                # ,mpg = colDef(
                #   sticky = "left",
                #   style = list(borderLeft = "1px solid #eee"),
                #   headerStyle = list(borderLeft = "1px solid #eee")
                # )
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
  

   fdf<-reactive({
     df <- read.csv("indata.csv")
     fdf = data.frame(
       x=df$`Subject.ID`
       ,y=df$`Visit.Name`
       ,z=df$`Visit.Date`
     )
     
     dat = fdf[fdf$x %in% input$p2 && fdf$y %in% input$p3,]
     return(dat)
     })
# 
#    observe({
#      print(fdf())
#    })
#    

  
  # output$hc_chart <- renderHighchart({
  #   highchart() %>%
  #     hc_chart(zoomType = 'xy', events = list(selection = s1, click = s2)) %>%
  #     hc_add_series(fdf(), "bar") %>%
  #     hc_add_event_point(event = "unselect")
  # })
  
  # output$hc_chart <- renderHighchart({
  #   # db_hc <-
  #   #   db[input$reactab_rows_selected, ] # filter dataset according to select rows in my_dt
  #    # db_hcx<-names(datafile()[1])
  #    # db_hcy<-names(state$sorted) 
  #   hc <- hchart(datafile(), type="scatter", hcaes(x=Subject.ID,y=Site ), name = "carb",dataLabels = list(enabled = F))%>%
  #     hc_chart(zoomType = 'xy', events = list(selection = s1, click = s2))%>%
  #     hc_add_event_point(event = "unselect")
  #   # %>% hc_add_series(name = "series1", data = db_hc$mpg) %>%
  #   #   hc_add_series(name = "series2", data = db_hc$wt)
  #   hc
  # })
  
  selected.points <- reactiveValues(x = NULL, y = NULL)
  
  output$view <- renderTable({
    if (is.null(selected.points$x) || is.null(selected.points$y)) {
      return(NULL)
    } else {
      data.table(x = selected.points$x, y = selected.points$y)  
    }
  })
  
  observeEvent(input$R_xArr, {
    selected.points$x <- sort(unique(c(selected.points$x, input$R_xArr)))
    selected.points$y <- df$y[df$x %in% selected.points$x]
  })
  
  observeEvent(input$plot_hc_unselect, {
    selected.points$x <- NULL 
    selected.points$y <- NULL
  })
  
  dfr<-reactive({
    df <- datafile()
    df <- df[df$`Subject.ID` %in% input$p2 & df$`Visit.Name` %in% input$p3,]
    return(df)
  })
  
  # output$hc_chart <- renderHighchart({
  #   # db_hc <-
  #   #   db[input$reactab_rows_selected, ] # filter dataset according to select rows in my_dt
  #    # db_hcx<-names(datafile()[1])
  #    # db_hcy<-names(state$sorted)
  #   hc <- hchart(dfr(), type="bar", hcaes(x=Subject.ID,y=Site ), name = "carb",dataLabels = list(enabled = F))
  #   # %>% hc_add_series(name = "series1", data = db_hc$mpg) %>%
  #   #   hc_add_series(name = "series2", data = db_hc$wt)
  #   hc
  # })
  
  output$hc_chart <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(categories = dfr()$`Visit.Name`) %>%
      hc_add_series(name="Visit Name",
                    data = dfr()$`Subject.ID`,
                    stack = "1") %>%
      hc_add_series(name="site",
                    data = dfr()$`Site`,
                    stack = "2") %>%
      hc_add_series(name="Country",
                    data = dfr()$`Country`,
                    stack = "3")%>%
      hc_add_series(name="Sample Status",
                    data = dfr()$`Sample.Status`,
                    stack = "4") %>%
      hc_add_series(name="Central Servicing Lab",
                    data = dfr()$`Central.Servicing.Lab`,
                    stack = "5") %>%
      hc_add_theme(hc_theme_ft())
    # hc <- hchart(dfr(), type="bar", hcaes(x=Subject.ID,y=Site ), name = "carb",dataLabels = list(enabled = F))
    # # %>% hc_add_series(name = "series1", data = db_hc$mpg) %>%
    # #   hc_add_series(name = "series2", data = db_hc$wt)
    # hc
  })
  
  observe({
    print(dfr())
  })
  
  output$reactab2 <- renderReactable({
    reactable(dfr(),rownames = F,elementId = "cars-table2",
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
                )
                # ,mpg = colDef(
                #   sticky = "left",
                #   style = list(borderLeft = "1px solid #eee"),
                #   headerStyle = list(borderLeft = "1px solid #eee")
                # )
              ),
              bordered = TRUE,
              highlight = TRUE
    )
  })
  
  
  output$selected_row_details <- renderUI({
    selected <- getReactableState("reactab2", "selected")
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