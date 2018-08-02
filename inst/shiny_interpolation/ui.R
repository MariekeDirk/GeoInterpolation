library(shiny)
library(leaflet)
library(shinydashboard)
#library(shiny)
library(DT)

ui<-dashboardPage(  
  dashboardHeader(
    title = div(img(src="datalab_logo.jpg",height=50,class="pull-left"),"Temperature")),
   
  
  dashboardSidebar(
    h3("Interpolation"),
    p("Explore the temperature interpolation using 34 ground-based observations and HARMONIE reanalysis from 1995 until 2014. 
      For the kriging interpolation with trend you can choose a date, blocksize and variogram model."),
    h4("(1) Select a date"),
    dateInput("t","Date",min="1995/01/02",max="2014/06/29",value="1999/01/19"),  
    
      h4("(2) Select the block size for kriging"),
        sliderInput("blocksize",
                    "blocksize:",
                    min = 10000,
                    max = 70000,
                    value = 20000),
        h4("(3) Select a variogram model"),
        selectInput("variogram","variogram model",c("Exp","Sph","Gau"))

    
  ),
  
  dashboardBody(
    
    fluidPage(
      fluidRow(
      plotOutput("plotKrige")
      
     
    
  )))
  )


  #
  # # Application title
  # headerPanel("Temperature Interpolation"),
  #
  # # Sidebar with controls to select the variable to plot against mpg
  # # and to specify whether outliers should be included
  # sidebarPanel(
  # sliderInput("blocksize","blocksize",min=as.numeric(10000),max=as.numeric(70000),value=as.numeric(20000),step=1000),
  # 
  #
  #
  #   # dateRangeInput("daterange1","Date range:",
  #   # start = "1995/01/02",
  #   # end ="2014/06/29")
  #   # checkboxInput("outliers", "Show outliers", FALSE)
  # ),
  #
  # # Show the caption and plot of the requested variable against mpg


