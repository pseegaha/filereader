#devtools::install_github("jbkunst/highcharter")
library(highcharter) 
library(htmlwidgets)
library(shiny)
library(data.table)

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



shinyApp(
  ui = fluidPage(
    highchartOutput("plot_hc"),
    tableOutput("view") 
  ),
  server = function(input, output) {
    
    df <- data.frame(x = 1:50, y = 1:50, otherInfo = letters[11:15])
    df<-read.csv("indata.csv")
    
    output$plot_hc <- renderHighchart({
      highchart() %>%
        hc_chart(zoomType = 'xy', events = list(selection = s1, click = s2)) %>%
        hc_add_series(df, "scatter") %>%
        hc_add_event_point(event = "unselect")
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
      selected.points$x <- sort(unique(c(selected.points$x, input$R_xArr)))
      selected.points$y <- df$y[df$x %in% selected.points$x]
    })
    
    observeEvent(input$plot_hc_unselect, {
      selected.points$x <- NULL 
      selected.points$y <- NULL
    })
    
  }
  
)