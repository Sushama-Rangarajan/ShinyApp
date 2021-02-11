#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(reshape)
#Reading the data and processing 

house1 <- read.csv("Housing_2020.csv")
h2 <- subset(house1,house1$PROPERTY.TYPE == "Single Family Residential" | house1$PROPERTY.TYPE == "Townhouse")

#Removing rows with 0 beds since it is practically not possible to have no beds and only baths in a house
h2 <- h2[-which(h2$BEDS == 0), ]

#Removing 2 obs with Year NAs since we can't impute those observations
h2 <- h2[-which(is.na(h2$YEAR.BUILT)),]

#Removing 1 ob where Bath is NAs
h2 <- h2[-which(is.na(h2$BATHS)),]

#Lotsize has 108 NAs. 
#Since lot size distribution is skewed we could use median imputation to fill for the NAs
h2$LOT.SIZE[is.na(h2$LOT.SIZE)] <- median(h2$LOT.SIZE, na.rm = TRUE)
house <- h2


##Shiny Dashboard

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "hist",icon = icon("chart-bar")),
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Boxplot", tabName = "box", icon = icon("square")),
      menuItem("Graph",tabName = "graph",icon = icon("map-marker-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("hist",
              fluidPage(
                titlePanel("House Price distribution (in $M) in Dallas area"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("Zipcode", "Choose one or more Zip codes",house$ZIP.OR.POSTAL.CODE,selected = 75234,multiple = TRUE)), 
                  mainPanel(
                    plotOutput("histoplot"))
                )
              )
      ),
      tabItem("table",
              fluidPage(
                titlePanel("Median House Price (in $M) grouped by Zipcode"),
                mainPanel(
                  tableOutput("med_price")
                )
              )),
      tabItem("box",
              fluidPage(
                titlePanel("House price distribution box plot"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("bed","Minimum number of beds",1,8,value = 3, step =1),
                    sliderInput("bath","Minimum number of bath",1,9,value = 2, step = 0.5),
                    sliderInput("sqft","Minimum squarefeet",528,27075, value = 1000),
                    sliderInput("lot","Minimum lotsize",44,18077,value = 7000),
                    sliderInput("year","Year built (after)",1868,2021,value = 1909,step=1)
                  ),
                  mainPanel(
                    plotOutput("priceplot")
                  )
                )
              )),
      tabItem("graph",
              fluidPage(
                titlePanel("Map showing the distribution of houses. Zoom and Hover for details"),
                leafletOutput("map") 
              ))
    )))
server <- function(input,output) {
  #Histogram
  histdata <- reactive({
    subset(house$PRICE/1000000, house$ZIP.OR.POSTAL.CODE == input$Zipcode)})
  output$histoplot <- renderPlot({ 
    hist(histdata(), xlab = "Prices in $M", ylab = "Number of houses", main = "Price distribution of selected zipcodes")
  })
  #Table
  output$med_price <- renderTable({
    table1 <- aggregate(house$PRICE/1000000, by = list(house$ZIP.OR.POSTAL.CODE,house$PROPERTY.TYPE),FUN = median) 
    colnames(table1) <- c("Zipcode","Property_Type","Median_Price")
    t<-cast(table1,Zipcode ~ Property_Type)
    arrange(t, desc(t$`Single Family Residential`))
  })
  
  #Box plot 
  pricedat <- reactive({
    subset(house$PRICE, house$BEDS >= input$bed & house$BATHS >= input$bath & house$SQUARE.FEET >= input$sqft 
           & house$LOT.SIZE >= input$lot & house$YEAR.BUILT >= input$year)
  })
  
  output$priceplot <- renderPlot({
    boxplot(pricedat()/1000000,main = "Price distribution of house that have the minimum spec as selected",
            ylab = "Price in $M")
  })
  #Graph
  output$map <- renderLeaflet({
    house <- house %>% mutate(lab = paste(house$ADDRESS,",",house$CITY, "City,TX-",house$ZIP.OR.POSTAL.CODE,
                                          ",Features",house$SQUARE.FEET,"sqft area,",house$BEDS,"Beds and",house$BATHS,"Baths and lot size of",house$LOT.SIZE, "sqft,", 
                                          "built in the year ",house$YEAR.BUILT))
    leaflet(house) %>% addTiles() %>% addCircleMarkers(house,lng = ~LONGITUDE, lat = ~LATITUDE, radius = 2,
                                                       color = "#03F",label = ~lab)
    
    
  })}
shinyApp(ui,server)

