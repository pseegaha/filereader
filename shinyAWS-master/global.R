library(dplyr)
library(shiny)
library(DT)
library(shinyWidgets)
library(shinymanager)
library(aws.s3)
library(shinydashboard)

#### Insert the AWS CSV Output in the api_keys folder of directory ####

aws_s3_creds <- read.csv("api_keys/new_user_credentials.csv")
Sys.setenv(
  "AWS_STORAGE_BUCKET_NAME" = 'bucket_name',
  "AWS_ACCESS_KEY_ID" = aws_s3_creds$Access.key.ID,
  "AWS_SECRET_ACCESS_KEY" = aws_s3_creds$Secret.access.key,
  "AWS_S3_CUSTOM_DOMAIN" = "'%s.s3.amazonaws.com' % ipractise",
  "AWS_S3_REGION_NAME" = 'us-east-1'
)


# Making a HexSticker #
# library(hexSticker)
# imgurl <- "https://abacusinsights.com/wp-content/uploads/2019/07/aws-logo-small.png"
# sticker(imgurl, package="shinyAWS", p_size=20, s_x=1, s_y=.75, s_width=.6,
#         filename="images/imgfile.png",h_color = "#ffffff",h_fill="#242F3F")
