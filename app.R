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
library(shinyjs)
library(readxl)

# DATASET
# extract <- read_excel("G:\\SPECIAL PROJECTS\\COLETTE 2018\\MERCHANDISING\\RETAIL\\SALES & STOCKS\\SELL OUT & STOCK\\2020\\10. OCTOBER\\W5\\SALES & STOCK 0211.xlsx", skip = 2)
# extract <- extract %>% mutate(SKU = extract[[2]],
#                               Style = extract[[4]],
#                               Category = extract[[5]],
#                               RP = extract[[7]],
#                               'In store' = extract[[11]],
#                               Warehouse = extract[[12]]) %>%
#         select(SKU, Style, Category, RP, 'In store', Warehouse) %>%
#         arrange(Category)

upcoming <- read_excel("C:\\Users\\jacques.tsang\\Desktop\\RD\\upcoming.xlsx") %>%
    mutate(DELIVERY = as.Date(as.numeric(DELIVERY), origin = "1900-01-01")) %>%
    arrange(DELIVERY)

extract <- read_excel("./catalog.xlsx")

upcoming <- read_excel("C:\\Users\\jacques.tsang\\Desktop\\RD\\upcoming.xlsx") %>%
    mutate(DELIVERY = as.Date(as.numeric(DELIVERY), origin = "1900-01-01")) %>%
    arrange(DELIVERY)

# Define UI for application that draws a histogram
ui <- navbarPage(
        tags$img(src = "https://i.ibb.co/jTQd6Cc/SL-RD-LOGO.jpg", height = "20px"),
        tabPanel("IN STOCK",
            fluidPage(
                tags$style(HTML('body {font-family:"Helvetica",sans-serif; font-size:6px}')),
                fluidRow(
                    column(12, prettyCheckboxGroup("filtre", "FILTER BY :", choices = unique(extract$Category[which(!(extract$Category %in% c("MUSIC","LIBRARY")))]), selected = "HOME", shape = "square", fill = TRUE, inline = TRUE)),
                    lapply(seq_along(extract$SKU), function(i){
                        column(2,
                               uiOutput(paste0("image", i)),
                               textOutput(paste0("sku", i)),
                               textOutput(paste0("desc", i)),
                               textOutput(paste0("price", i)),
                               textOutput(paste0("instore", i)),
                               textOutput(paste0("warehouse", i))
                        )
                    })
                )
            )
        ),
        tabPanel("UPCOMING",
                 fluidPage(
                     tags$style(HTML('body {font-family:"Helvetica",sans-serif; font-size:6px}')),
                     fluidRow(
                         column(1,
                                lapply(seq_along(unique(upcoming$IMPLANTATION)), function(i){
                                    HTML(paste("<a href='#' id='clicked' style='color:black; text-decoration:none; font-size:20px; font-weight:bold' onclick='detect_click(this)'>",
                                               unique(upcoming$IMPLANTATION)[i],
                                               "</a>",
                                               "<br>")
                                    )
                                })
                         ),
                         column(10,
                                includeScript("./detect_click.js"),
                                uiOutput("up"),
                                offset = 1
                         )
                     )
                 )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

# Reactive filtering data
    data <- reactive({
        extract %>% filter(Category %in% input$filtre)
    })
    
# Pictures
    observeEvent(input$filtre,{
        extract <- data()
        lapply(seq_along(extract$SKU), function(i){
            output[[paste0("image", i)]] <- renderUI({
                tags$img(src = paste0('http://fastcache-zur.emea.guccigroup.dom/getimage/?Cod=',
                                      str_sub(extract$SKU[i],1,6),
                                      '-',
                                      str_sub(extract$SKU[i],7,11),
                                      '-',
                                      str_sub(extract$SKU[i],12,15),
                                      '&ds=0&typ=P&dim=XL&fb=1'
                ), height="100")
            })
        }) 
#SKU    
        lapply(seq_along(extract$SKU), function(i){
            output[[paste0("sku", i)]] <- renderText({
                extract$SKU[i]
            })
        })
#DESC    
        lapply(seq_along(extract$SKU), function(i){
            output[[paste0("desc", i)]] <- renderText({
                extract$Style[i]
            })
        })
#PRICE    
        lapply(seq_along(extract$SKU), function(i){
            output[[paste0("price", i)]] <- renderText({
                paste0(extract$RP[i]," EUR")
            })
        })
#IN STORE    
        lapply(seq_along(extract$SKU), function(i){
            output[[paste0("instore", i)]] <- renderText({
                paste0("IN STORE : ",extract$'In store'[i]," PCS")
            })
        })
#WAREHOUSE
        lapply(seq_along(extract$SKU), function(i){
            output[[paste0("warehouse", i)]] <- renderText({
                paste0("BULK : ",extract$Warehouse[i]," PCS")
            })
        })
    })
#UPCOMING CATALOG
    observeEvent(input$clicked,{
        output$up <- renderUI({
            upcoming <- upcoming %>% filter(IMPLANTATION == input$clicked)
            lapply(seq_along(upcoming$SKU), function(j){
                column(3,
                       img(src=upcoming$PATH[j], height="50", width="50"),
                       p(upcoming$SKU[j],
                         br(),
                         upcoming$DESC[j],
                         br(),
                         paste("RP : ", upcoming$EUR[j], "EUR"),
                         br(),
                         paste("ORDER : ", upcoming$QTIES[j], "PCS"),
                         br(),
                         paste("DEL : ", upcoming$DELIVERY[j])
                       )
                )
            })
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
