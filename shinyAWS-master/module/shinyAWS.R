awsUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "AWS Control Panel",width = 6,status = "primary",
    fluidRow(actionBttn("upload_aws" %>% ns,"Upload Button",icon = icon("aws"))),
    DTOutput("aws_bucket" %>% ns()),
    DTOutput("aws_files" %>% ns())
    ))
}

awsServer <- function(id,rv) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      observeEvent(input$upload_aws,{
        
        temp_df <- aws.s3::bucket_list_df()
        
        showModal(modalDialog(size = "l",
                              title = "Upload a CSV file to put into an S3 Bucket",
                              textInput("csv_name" %>% ns(),"Name the CSV you want in the AWS S3 Bucket"),
                              selectInput("bucket_name" %>% ns(),"Which AWS S3 Bucket do you want to put this is?",choices = temp_df$Bucket),
                              fileInput("file1", "Choose CSV File",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              
                              output$upload_csv <- renderDT({
                                req(input$file1)
                                rv$upload <- read.csv(input$file1$datapath)
                                DT::datatable(rv$upload,options = list(scrollX = TRUE),rownames = FALSE)}),
                              actionButton(ns("upload_csv"),"Upload CSV to S3 Bucket",icon = icon("aws"))
        ))
      })
      observeEvent(input$upload_csv,{
        temp_def <- rv$upload
        s3save(temp_def, bucket = input$bucket_name, object = paste0("*UploadFile*",input$csv_name,".csv"))
        shinyalert("Success","We have uploaded your course",timer = 4000)
        removeModal()
      })
  
      observeEvent(input$aws_bucket_rows_selected,{
        rv$selected_bucket <- rv$aws_buckets %>% dplyr::slice(input$aws_bucket_rows_selected) 
        rv$aws_files <- aws.s3::get_bucket_df(bucket = rv$selected_bucket$Bucket)
        output$aws_files <- renderDT({
          datatable(rv$aws_files,
                    rownames = FALSE,
                    options = list(scrollX = TRUE))
      })
  
  })
  
  output$aws_bucket <- renderDT({
    rv$aws_buckets <- aws.s3::bucket_list_df()
    datatable(rv$aws_buckets,
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })
  }
  )
}