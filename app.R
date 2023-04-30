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
library(lubridate)
library(plotly)
# selectPointsByDrag
s1 <- JS(
  "/**
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
           }"
)

# unselectByClick
s2 <- JS(
  "/**
         * On click, unselect all points
         */
         function unselectByClick() {
           var points = this.getSelectedPoints();
           if (points.length > 0) {
             Highcharts.each(points, function (point) {
               point.select(false);
             });
           }
         }"
)

easeOutBounce  <- JS(
  "function (pos) {
  if ((pos) < (1 / 2.75)) {
    return (7.5625 * pos * pos);
  }
  if (pos < (2 / 2.75)) {
    return (7.5625 * (pos -= (1.5 / 2.75)) * pos + 0.75);
  }
  if (pos < (2.5 / 2.75)) {
    return (7.5625 * (pos -= (2.25 / 2.75)) * pos + 0.9375);
  }
  return (7.5625 * (pos -= (2.625 / 2.75)) * pos + 0.984375);
}"
)

# hchart(mtcars, type="point", hcaes(x=mpg, y=cyl,color=hp,group=gear,variable=carb), name = "carb",dataLabels = list(enabled = F))

csvFileServer <- function(id, stringsAsFactors) {
  moduleServer(id,
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
                            , stringsAsFactors = stringsAsFactors)
                 })
                 
                 # We can run observers in here if we want to
                 observe({
                   msg <- sprintf("File %s was uploaded", userFile()$name)
                   cat(msg, "\n")
                 })
                 
                 # Return the reactive that yields the data frame
                 return(dataframe)
               })
}

# Module UI function
csvFileUI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(fileInput(ns("file"), label))
}


library(bs4Dash)
ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
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
      menuItem(
        icon = icon('table'),
        # condition = "input.controlbarToggle == true",
        text = "Preview & Upload",
        tabName = "tab1"
        
        
      ),
      
      
      menuItem(
        icon = icon('bar-chart-o'),
        # condition = "input.controlbarToggle == true",
        text = "Subject Analytics",
        tabName = "tab2"
      )
      ,
      menuItem(
        icon = icon('bar-chart-o'),
        # condition = "input.controlbarToggle == true",
        text = "Visit Analytics",
        tabName = "tab3"
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
    tags$script(
      '$(function(){$(\'[data-value="button1"]\').click(function(){$("#controlbar").css("color", "grey")})})'
    ),
    tabItems(
      tabItem(tabName = "tab1",
              fluidRow(
                box(
                  width = 12,
                  # Simple theme toggle button
                  tags$button(onclick = "document.querySelector('.themeable-tbl').classList.toggle('dark')",
                              "Toggle light/dark"),
                  tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('cars-table')"),
                  div(class = "themeable-tbl light", reactableOutput("reactab"))
                  
                )
                # ,box(width = 12,
                #      highchartOutput("hc_chart", height = "500px")
                # )
              ))
      ,
      tabItem(
        tabName = "tab2",
        tags$table(
          class = 'table table-bordered km-table',
          tags$tr(
            class = 'w-2',
            tags$td(
              align = "center",
              class = 'w-25',
              valueBoxOutput("vbox1", width = 12)
            ),
            tags$td(
              align = "center",
              class = 'w-25',
              valueBoxOutput("vbox2", width = 12)
            ),
            tags$td(
              align = "center",
              class = 'w-25',
              valueBoxOutput("vbox3", width = 12)
            ),
            tags$td(
              align = "center",
              class = 'w-25',
              valueBoxOutput("vbox4", width = 12)
            )
          )
        )
        ,
        wellPanel(
          style = "align:center;background:white",
          fluidRow(column(width=6, align = "center",


                pickerInput(
                  inputId = "p2",
                  label = "Select the Subjects",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Deselect All",
                    `select-all-text` = "Select All",
                    `none-selected-text` = "Select Subjects"
                  )
                )
              )
              ,
              column(width=6, align = "center",
                pickerInput(
                  inputId = "p3",
                  label = "Select the Visit Name",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Deselect All",
                    `select-all-text` = "Select All",
                    `none-selected-text` = "Select Visits"
                  )
                )
 
            )
          )
            ,
            
            
            fluidRow(column(
              width = 6,
              align = "center",

                airDatepickerInput(
                  inputId = "multiple1",
                  label = "Select first visit date:",
                  placeholder = "Pick the first",
                  multiple = 1,
                  clearButton = TRUE
                )
            )
              
              ,
            column(
              width = 6, align = "center",
                airDatepickerInput(
                  inputId = "multiple2",
                  label = "Select last visit date:",
                  placeholder = "Pick the last",
                  multiple = 1,
                  clearButton = TRUE
                )
              
            ))
          
          
          
        ),
        fluidRow(
          # box(
          #   width = 12,
          #   highchartOutput("hc_chart", height = "500px"),
          #   tableOutput("view")
          # ),
          box(
            width = 12,
            plotlyOutput(
              outputId = 'plotly_chart',
              width = "100%",
              height = "400px",
              inline = FALSE,
              reportTheme = TRUE
            )
          ),
          box(
            width = 12,
            # Simple theme toggle button
            tags$button(onclick = "document.querySelector('.themeable-tbl2').classList.toggle('dark')",
                        "Toggle light/dark"),
            tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('cars-table2')"),
            div(class = "themeable-tbl2 light", reactableOutput("reactab2"))
            
          )
        )
        
      )
      ,tabItem(
        tabName = "tab3",
        tags$table(
          class = 'table table-bordered km-table',
          tags$tr(
            class = 'w-2',
            tags$td(
              align = "center",
              class = 'w-25',
              valueBoxOutput("svbox1", width = 12)
            ),
            tags$td(
              align = "center",
              class = 'w-25',
              valueBoxOutput("svbox2", width = 12)
            ),
            tags$td(
              align = "center",
              class = 'w-25',
              valueBoxOutput("svbox3", width = 12)
            ),
            tags$td(
              align = "center",
              class = 'w-25',
              valueBoxOutput("svbox4", width = 12)
            )
          )
        )
        ,
        wellPanel(
          style = "align:center;background:white",
          fluidRow(column(width=6, align = "center",


                          pickerInput(
                            inputId = "sp2",
                            label = "Select the Subjects",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              `live-search` = TRUE,
                              `actions-box` = TRUE,
                              `deselect-all-text` = "Deselect All",
                              `select-all-text` = "Select All",
                              `none-selected-text` = "Select Subjects"
                            )
                          )
          )
          ,
          column(width=6, align = "center",
                 pickerInput(
                   inputId = "sp3",
                   label = "Select the Visit Name",
                   choices = NULL,
                   multiple = TRUE,
                   options = list(
                     `live-search` = TRUE,
                     `actions-box` = TRUE,
                     `deselect-all-text` = "Deselect All",
                     `select-all-text` = "Select All",
                     `none-selected-text` = "Select Visits"
                   )
                 )

          )
          )
          ,


          fluidRow(column(
            width = 6,
            align = "center",

            airDatepickerInput(
              inputId = "smultiple1",
              label = "Select first visit date:",
              placeholder = "Pick the first",
              multiple = 1,
              clearButton = TRUE
            )
          )

          ,
          column(
            width = 6, align = "center",
            airDatepickerInput(
              inputId = "smultiple2",
              label = "Select last visit date:",
              placeholder = "Pick the last",
              multiple = 1,
              clearButton = TRUE
            )

          ))



        ),
        fluidRow(
          # box(
          #   width = 12,
          #   highchartOutput("shc_chart", height = "500px"),
          #   tableOutput("sview")
          # ),
          box(
            width = 12,
            plotlyOutput(
              outputId = 'splotly_chart',
              width = "100%",
              height = "400px",
              inline = FALSE,
              reportTheme = TRUE
            )
          ),
          box(
            width = 12,
            # Simple theme toggle button
            tags$button(onclick = "document.querySelector('.themeable-stbl2').classList.toggle('dark')",
                        "Toggle light/dark"),
            tags$button("Download as CSV", onclick = "Reactable.downloadDataCSV('scars-table2')"),
            div(class = "themeable-stbl2 light", reactableOutput("sreactab2"))

          )
        )


      )
    )
  )
  ,
  controlbar = dashboardControlbar(
    id = "controlbar",
    width = 600,
    collapsed = FALSE,
    overlay = TRUE
    ,
    pinned = FALSE
    # ,controlbarItem(id = "button1", title = "button1"
    #                 ,csvFileUI("datafile1", NULL))
    ,
    fluidRow(column(
      width = 12,
      align = 'center',
      tags$h4("Preview & Upload")
      ,
      csvFileUI("datafile", NULL)
    ))
    ,
    wellPanel(
      style = "background:white;text-align:center",
      uiOutput("selected_row_details")
      ,
      verbatimTextOutput("table_state")
    )
    
    
    
  ),
  title = "updateControlbar"
)



server <- function(input, output, session) {
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)


  observe({
    updatePickerInput(
      session = session,
      inputId = "p2",
      choices = unique(datafile()$`Subject.ID`),
      selected = NULL
      
    )
    # print(input$sp2)
    # print(unique(datafile()[datafile()$`Subject.ID` %in% input$sp2,]$`Visit.Name`))
  })
  
  observeEvent(input$p2,{
    updatePickerInput(
      session = session,
      inputId = "p3",
      choices = unique(datafile()[datafile()$`Subject.ID` %in% input$p2,]$`Visit.Name`),
      selected = NULL
    )
  })
  
  observe({
    updateAirDateInput(session = session,
                       inputId = 'multiple1',
                       value = min(dmy(datafile()$Visit.Dat)))
    
    updateAirDateInput(session = session,
                       inputId = 'multiple2',
                       value = max(dmy(datafile()$Visit.Dat)))
  })
  
  # observe({
  #   print(input$multiple1)
  # })
  
  observe({
    updatePickerInput(
      session = session,
      inputId = "sp2",
      choices = unique(datafile()$`Subject.ID`),
      selected = NULL
      
      )
    # print(input$sp2)
    # print(unique(datafile()[datafile()$`Subject.ID` %in% input$sp2,]$`Visit.Name`))
  })

observeEvent(input$sp2,{

    updatePickerInput(
      session = session,
      inputId = "sp3",
      choices = unique(datafile()[datafile()$`Subject.ID` %in% input$sp2,]$`Visit.Name`),
      selected = NULL
    )
})

# observeEvent(input$sp3,{
#   
#   updatePickerInput(
#     session = session,
#     inputId = "sp2",
#     choices = unique(datafile()[datafile()$`Visit.Name` %in% input$sp3,]$`Subject.ID`),
#     selected = NULL
#   )
# })

    observe({
      
    updateAirDateInput(session = session,
                       inputId = 'smultiple1',
                       value = min(dmy(datafile()$Visit.Dat))
                       )
    
    updateAirDateInput(session = session,
                       inputId = 'smultiple2',
                       value = max(dmy(datafile()$Visit.Dat))
                       )
    
  })
  
  output$vbox1 <- renderValueBox({
    valueBox(
      value = length(unique(datafile()$`Subject.ID`)),
      subtitle = "Subjects",
      color = "primary",
      icon = NULL
      # icon = icon("shopping-cart")
      
      # ,href = "#"
      ,
      footer = div("")
    )
  })
  
  
  output$vbox2 <- renderValueBox({
    valueBox(
      value = length(unique(datafile()$`Site`)),
      subtitle = "Site",
      color = "primary",
      icon = NULL
      # icon = icon("cart-shopping")
      ,
      footer = div("")
    )
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(
      value = length(unique(datafile()$`Country`)),
      subtitle = "Country",
      color = "indigo",
      icon = NULL
      # icon = icon("gears")
      
      ,
      footer = div("")
    )
  })
  
  output$vbox5 <- renderValueBox({
    valueBox(
      value = length(unique(datafile()$`Site`)),
      subtitle = "Site",
      color = "teal",
      icon = NULL
      # icon = icon("sliders")
      ,
      footer = div("")
    )
  })
  
  output$vbox4 <- renderValueBox({
    valueBox(
      value = length(which(
        datafile()$`Sample.Status` == "Shipped"
      )),
      subtitle = "Shipped Count",
      color = "teal",
      icon = NULL
      # icon = icon("sliders")
      ,
      footer = div("")
    )
  })
  
  output$svbox1 <- renderValueBox({
    valueBox(
      value = length(unique(datafile()$`Subject.ID`)),
      subtitle = "Subjects",
      color = "primary",
      icon = NULL
      # icon = icon("shopping-cart")
      
      # ,href = "#"
      ,
      footer = div("")
    )
  })
  
  
  output$svbox2 <- renderValueBox({
    valueBox(
      value = length(unique(datafile()$`Site`)),
      subtitle = "Site",
      color = "primary",
      icon = NULL
      # icon = icon("cart-shopping")
      ,
      footer = div("")
    )
  })
  
  output$svbox3 <- renderValueBox({
    valueBox(
      value = length(unique(datafile()$`Country`)),
      subtitle = "Country",
      color = "indigo",
      icon = NULL
      # icon = icon("gears")
      
      ,
      footer = div("")
    )
  })
  
  output$svbox5 <- renderValueBox({
    valueBox(
      value = length(unique(datafile()$`Site`)),
      subtitle = "Site",
      color = "teal",
      icon = NULL
      # icon = icon("sliders")
      ,
      footer = div("")
    )
  })
  
  output$svbox4 <- renderValueBox({
    valueBox(
      value = length(which(
        datafile()$`Sample.Status` == "Shipped"
      )),
      subtitle = "Shipped Count",
      color = "teal",
      icon = NULL
      # icon = icon("sliders")
      ,
      footer = div("")
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
  
  output$table <- renderDataTable({
    data.table(datafile())
  })
  
  output$reactab <- renderReactable({
    reactable(
      datafile(),
      rownames = F,
      elementId = "cars-table",
      showPageSizeOptions = TRUE,
      # selection = "multiple",
      # onClick = "select",
      theme = theme <- reactableTheme(
        style = list(".dark &" = list(
          color = "#fff", background = "#282a36"
        )),
        cellStyle = list(
          ".dark &" = list(
            borderColor = "grey",
            fontcolor = "white",
            fontSize = "11px"
          )
          ,
          ".light &" = list(
            borderColor = "grey",
            fontcolor = "black",
            fontSize = "11px"
          )
        ),
        headerStyle = list(
          ".dark &" = list(borderColor = "grey", fontSize = "14px")
          ,
          ".light &" = list(borderColor = "grey", fontSize = "14px")
        ),
        paginationStyle = list(
          ".dark &" = list(color = "black", borderColor = "rgba(255, 255, 255, 0.15)")
        ),
        rowHighlightStyle = list(
          ".dark &" = list(background = "grey")
          ,
          ".light &" = list(background = "grey")
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
        header = function(value)
          gsub(".", " ", value, fixed = TRUE),
        cell = function(value)
          format(value, nsmall = 1),
        # style = function(value) list(value,fontWeight = 20),
        align = "center",
        minWidth = 100
        # ,headerStyle = list(background = "#f7f7f8",borderColor = "#555")
      ),
      
      columns = list(
        .rownames = colDef(
          name = "Subject ID",
          sortable = TRUE,
          align = "left"
        ),
        character = colDef(
          # Show species under character names
          cell = function(value) {
            div(div(style = "font-weight: 5", value))
          }
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
  
  
  selected.points <- reactiveValues(x = NULL, y = NULL)
  
  output$view <- renderTable({
    if (is.null(selected.points$x) || is.null(selected.points$y)) {
      return(NULL)
    } else {
      data.table(x = selected.points$x, y = selected.points$y)
    }
  })
  
  observeEvent(input$R_xArr, {
    selected.points$x <-
      sort(unique(c(selected.points$x, input$R_xArr)))
    selected.points$y <- df$y[df$x %in% selected.points$x]
  })
  
  observeEvent(input$plot_hc_unselect, {
    selected.points$x <- NULL
    selected.points$y <- NULL
  })
  
  dfr <- reactive({
    df <- datafile()
    # Remove duplicate rows
    df <- df[!duplicated(df), ]
    df$`Visit.Date`<-dmy(df$`Visit.Date`)
    
    df <-
      df[df$`Subject.ID` %in% input$p2 & df$`Visit.Name` %in% input$p3, ]
    df <-df[df$`Visit.Date` >= input$multiple1 & df$`Visit.Date` <= input$multiple2, ]
    return(df)
  })
  
  ategory <- reactiveVal()
  sategory <- reactiveVal()
  
  # when clicking on a category, 
  observeEvent(event_data("plotly_click", source = "ategory"), {
    ategory(event_data("plotly_click", source = "ategory")$x)
    print(ategory())
    
  })
  
  # when clicking on a category, 
  observeEvent(event_data("plotly_click", source = "sategory"), {
    sategory(event_data("plotly_click", source = "sategory")$x)
    print(sategory())
    
  })
  
  sdfr <- reactive({
    df <- datafile()
    # Remove duplicate rows
    df <- df[!duplicated(df), ]
    df$`Visit.Date`<-dmy(df$`Visit.Date`)
    
    df <-
      df[df$`Subject.ID` %in% input$sp2 & df$`Visit.Name` %in% input$sp3, ]
    df <-df[df$`Visit.Date` >= input$smultiple1 & df$`Visit.Date` <= input$smultiple2, ]
    
    return(df)
  })
  output$plotly_chart <- renderPlotly({
    library(dplyr)
    indata <- dfr() %>% 
      group_by(dfr()$`Subject.ID`)%>%
      mutate(kcount = n())
    
    # print(c(indata$`Subject.ID`,indata$kcount))
    plot_ly(
      indata,
      x = ~`Subject.ID`,
      y = ~`kcount`,
      group =~`Sample.Status`,
      color =~`Sample.Status` ,
      type = 'bar',
      source = "ategory",
      hovertemplate = paste("<b>%{xaxis.title.text}:  %{x}</b><br>",
                            "%{yaxis.title.text}:  %{y}<br><extra></extra>")
    ) %>% layout(
      title = "Patient Status",
      barmode = 'stack',
      xaxis = list(title = "Subject ID's"),
      ## Order & label the x-axis
      yaxis = list(title = "Frequency of Occurences")
    )
  })
  output$splotly_chart <- renderPlotly({
    library(dplyr)
    indata <- sdfr() %>% 
      group_by(sdfr()$`Site`)%>%
      mutate(kcount = n())
    
    # print(c(indata$`Subject.ID`,indata$kcount))
    plot_ly(
      indata,
      x = ~as.character(`Site`),
      y = ~`kcount`,
      group =~`Sample.Status`,
      color =~`Sample.Status` ,
      type = 'bar',
      source = "sategory",
      hovertemplate = paste("<b>%{xaxis.title.text}:  %{x}</b><br>",
                            "%{yaxis.title.text}:  %{y}<br><extra></extra>")
    ) %>% layout(
      title = "Patient Status",
      barmode = 'stack',
      xaxis = list(title = "Subject ID's"),
      ## Order & label the x-axis
      yaxis = list(title = "Frequency of Occurences")
    )
  })
  
  # output$hc_chart <- renderHighchart({
  #   library(dplyr)
  #   indata <- dfr() %>%
  #     group_by(dfr()$`Subject.ID`) %>%
  #     mutate(kcount = n())
  #   
  #   # I've tried to created a function using `JS`:
  #   
  #   highchart() %>%
  #     hc_title(text = "Patient Status",
  #              style = list(fontSize = "15px", fontWeight = "bold")) %>%
  #     hc_subtitle(text = "Frequency of Occurences") %>%
  #     hc_chart(type = "column") %>%
  #     hc_plotOptions(column = list(stacking = "normal"))  %>%
  #     hc_add_series(
  #       data = indata,
  #       animatio = list(duration = 1000,
  #                       easing = easeOutBounce),
  #       type = 'column',
  #       hcaes(
  #         x = unique(Subject.ID),
  #         y = kcount,
  #         group = Sample.Status,
  #         fill = Sample.Status
  #       )
  #     ) %>%
  #     hc_xAxis(
  #       title = list(text = "Subjects"),
  #       categories = indata$Subject.ID,
  #       tickmarkPlacement = "on",
  #       plotLines = list(list(label = list(rotation = 90)))
  #     ) %>%
  #     hc_yAxis(title = list(text = "Frequency"), offset = 10) %>%
  #     # hc_tooltip(pointFormat = "<b> Freq: </b> {point.kcount}",shared = F)%>%
  #     hc_add_theme(hc_theme_google()) %>%
  #     hc_legend(reversed = TRUE) %>% hc_exporting(enabled = TRUE, # always enabled
  #                                                 filename = "Intellia_Patient_Status")
  #   
  # })
  # 
  # output$shc_chart <- renderHighchart({
  #   library(dplyr)
  #   indata <- sdfr() %>%
  #     group_by(sdfr()$`Subject.ID`) %>%
  #     mutate(kcount = n())
  #   
  #   # I've tried to created a function using `JS`:
  #   
  #   highchart() %>%
  #     hc_title(text = "Patient Status",
  #              style = list(fontSize = "15px", fontWeight = "bold")) %>%
  #     hc_subtitle(text = "Frequency of Occurences") %>%
  #     hc_chart(type = "column") %>%
  #     hc_plotOptions(column = list(stacking = "normal"))  %>%
  #     hc_add_series(
  #       data = indata,
  #       animatio = list(duration = 1000,
  #                       easing = easeOutBounce),
  #       type = 'column',
  #       hcaes(
  #         x = unique(Subject.ID),
  #         y = kcount,
  #         group = Sample.Status,
  #         fill = Sample.Status
  #       )
  #     ) %>%
  #     hc_xAxis(
  #       title = list(text = "Subjects"),
  #       categories = indata$Subject.ID,
  #       tickmarkPlacement = "on",
  #       plotLines = list(list(label = list(rotation = 90)))
  #     ) %>%
  #     hc_yAxis(title = list(text = "Frequency"), offset = 10) %>%
  #     # hc_tooltip(pointFormat = "<b> Freq: </b> {point.kcount}",shared = F)%>%
  #     hc_add_theme(hc_theme_google()) %>%
  #     hc_legend(reversed = TRUE) %>% hc_exporting(enabled = TRUE, # always enabled
  #                                                 filename = "Intellia_Patient_Status")
  #   
  # })
  
  output$reactab2 <- renderReactable({
    library(dplyr)
    indata <- dfr() %>%
      filter(
        dfr()$`Subject.ID` %in% ategory()
      )%>% 
      # group_by(dfr()$`Subject.ID`)%>%
      mutate(kcount = n())
    reactable(
      indata,
      rownames = F,
      elementId = "cars-table2",
      showPageSizeOptions = TRUE,
      selection = "multiple",
      onClick = "select",
      theme = theme <- reactableTheme(
        style = list(".dark &" = list(
          color = "#fff", background = "#282a36"
        )),
        cellStyle = list(
          ".dark &" = list(
            borderColor = "grey",
            fontcolor = "white",
            fontSize = "11px"
          )
          ,
          ".light &" = list(
            borderColor = "grey",
            fontcolor = "black",
            fontSize = "11px"
          )
        ),
        headerStyle = list(
          ".dark &" = list(borderColor = "grey", fontSize = "14px")
          ,
          ".light &" = list(borderColor = "grey", fontSize = "14px")
        ),
        paginationStyle = list(
          ".dark &" = list(color = "black", borderColor = "rgba(255, 255, 255, 0.15)")
        ),
        rowHighlightStyle = list(
          ".dark &" = list(background = "grey")
          ,
          ".light &" = list(background = "grey")
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
        header = function(value)
          gsub(".", " ", value, fixed = TRUE),
        cell = function(value)
          format(value, nsmall = 1),
        # style = function(value) list(value,fontWeight = 20),
        align = "center",
        minWidth = 100
        # ,headerStyle = list(background = "#f7f7f8",borderColor = "#555")
      ),
      
      columns = list(
        .rownames = colDef(
          name = "Subject ID",
          sortable = TRUE,
          align = "left"
        ),
        character = colDef(
          # Show species under character names
          cell = function(value) {
            div(div(style = "font-weight: 5", value))
          }
        )
      ),
      bordered = TRUE,
      highlight = TRUE
    )
  })
  
  
  output$sreactab2 <- renderReactable({
    library(dplyr)
    indata <- sdfr() %>% 
      filter(
        sdfr()$`Site` %in% sategory()
      )%>% 
      # group_by(sdfr()$`Subject.ID`)%>%
      mutate(kcount = n())
    reactable(
      indata,
      rownames = F,
      elementId = "scars-table2",
      showPageSizeOptions = TRUE,
      selection = "multiple",
      onClick = "select",
      theme = theme <- reactableTheme(
        style = list(".dark &" = list(
          color = "#fff", background = "#282a36"
        )),
        cellStyle = list(
          ".dark &" = list(
            borderColor = "grey",
            fontcolor = "white",
            fontSize = "11px"
          )
          ,
          ".light &" = list(
            borderColor = "grey",
            fontcolor = "black",
            fontSize = "11px"
          )
        ),
        headerStyle = list(
          ".dark &" = list(borderColor = "grey", fontSize = "14px")
          ,
          ".light &" = list(borderColor = "grey", fontSize = "14px")
        ),
        paginationStyle = list(
          ".dark &" = list(color = "black", borderColor = "rgba(255, 255, 255, 0.15)")
        ),
        rowHighlightStyle = list(
          ".dark &" = list(background = "grey")
          ,
          ".light &" = list(background = "grey")
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
        header = function(value)
          gsub(".", " ", value, fixed = TRUE),
        cell = function(value)
          format(value, nsmall = 1),
        # style = function(value) list(value,fontWeight = 20),
        align = "center",
        minWidth = 100
        # ,headerStyle = list(background = "#f7f7f8",borderColor = "#555")
      ),
      
      columns = list(
        .rownames = colDef(
          name = "Subject ID",
          sortable = TRUE,
          align = "left"
        ),
        character = colDef(
          # Show species under character names
          cell = function(value) {
            div(div(style = "font-weight: 5", value))
          }
        )
      ),
      bordered = TRUE,
      highlight = TRUE
    )
  })
  
  output$selected_row_details <- renderUI({
    selected <- getReactableState("reactab2", "selected")
    req(selected)
    rowdetails <- datafile()[selected, ]
    
    tagList(h2("Selected row details"),
            tags$pre(paste(capture.output(
              print(rowdetails, width = 200)
            ), collapse = "\n")))
  })
}

runApp(shinyApp(ui, server), launch.browser = T)