library(shiny)
library(plotly)
library(dplyr)
# https://stackoverflow.com/questions/60137645/how-to-color-a-clicked-bar-from-barchart-with-r-plolty-shiny-when-having-alrea

sales <- diamonds
sales$category = sales$cut
sales$sub_category = sales$color
sales$sales = sales$price
sales$order_date = sample(seq(as.Date('2020-01-01'), as.Date('2020-02-01'), by="day"),nrow(sales), replace = T)



ui <- fluidPage(
  plotlyOutput("category", height = 200),
  plotlyOutput("sub_category", height = 200),
  plotlyOutput("sales", height = 300),
  DT::dataTableOutput("datatable")
)

# avoid repeating this code
axis_titles <- . %>%
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "Sales")
  )

server <- function(input, output, session) {
  
  # for maintaining the state of drill-down variables
  ategory <- reactiveVal()
  sub_category <- reactiveVal()
  order_date <- reactiveVal()
  
  # when clicking on a category, 
  observeEvent(event_data("plotly_click", source = "ategory"), {
    ategory(event_data("plotly_click", source = "ategory")$x)
    sub_category(NULL)
    order_date(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "sub_category"), {
    sub_category(
      event_data("plotly_click", source = "sub_category")$x
    )
    order_date(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "order_date"), {
    order_date(event_data("plotly_click", source = "order_date")$x)
  })
  
  output$category <- renderPlotly({
    print(ategory())
    if (is.null(ategory())) {
      plot_data <- sales %>%
        count(category, wt = sales) %>%
        mutate(current_color = "blue")
    } else {
      plot_data <- sales %>%
        count(category, wt = sales) %>%
        mutate(current_color = if_else(category %in% ategory(), "red", "blue"))
    }
    plot_ly(
      plot_data, x = ~category, y = ~n, source = "ategory", type = "bar",
      marker = list(color = ~current_color)
    ) %>%
      axis_titles() %>% 
      layout(title = "Sales by category")
  })
  
  output$sub_category <- renderPlotly({
    if (is.null(ategory())) return(NULL)
    sales %>%
      filter(category %in% ategory()) %>%
      count(sub_category, wt = sales) %>%
      mutate(current_color = if_else(sub_category %in% sub_category(), "green", "red")) %>%
      plot_ly(
        x = ~sub_category, y = ~n, source = "sub_category", type = "bar",
        marker = list(color = ~current_color)
      ) %>%
      axis_titles() %>%
      layout(title = ategory())
  })
  
  output$sales <- renderPlotly({
    if (is.null(sub_category())) return(NULL)
    sales %>%
      filter(sub_category %in% sub_category()) %>%
      count(order_date, wt = sales) %>%
      plot_ly(x = ~order_date, y = ~n, source = "order_date", line = list(color = "green")) %>%
      add_lines() %>%
      axis_titles() %>%
      layout(title = paste(sub_category(), "sales over time"))
  })
  
  output$datatable <-  DT::renderDataTable({
    if (is.null(order_date())) return(NULL)
    
    sales %>%
      filter(
        sub_category %in% sub_category(),
        as.Date(order_date) %in% as.Date(order_date())
      )
  })
  
}

shinyApp(ui, server)