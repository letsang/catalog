#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(aweek)
library(readxl)

# DATASET
upcoming <- read_excel("C:\\Users\\jacques.tsang\\Desktop\\RD\\upcoming.xlsx") %>%
  mutate(DELIVERY = as.Date(as.numeric(DELIVERY), origin = "1900-01-01")) %>%
  arrange(DELIVERY)

# Define UI for application that draws a histogram
ui <- navbarPage(
  tags$img(src = "https://i.ibb.co/jTQd6Cc/SL-RD-LOGO.jpg", height = "20px"),
  tabPanel("UPCOMING",
           fluidPage(
             tags$style(HTML('body {font-family:"Helvetica",sans-serif; font-size:6px}')),
             fluidRow(
               lapply(seq_along(upcoming$SKU), function(i){
                 column(2,
                        img(src=upcoming$PATH[i], height="50", width="50"),
                        p(upcoming$SKU[i],
                          br(),
                          upcoming$DESC[i],
                          br(),
                          paste("RP : ", upcoming$EUR[i], "EUR"),
                          br(),
                          paste("ORDER : ", upcoming$QTIES[i], "PCS"),
                          br(),
                          paste("DEL : ", upcoming$DELIVERY[i])
                        )
                      )
               })
             )
          )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)

