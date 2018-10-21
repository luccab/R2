library(shiny)
library(ggplot2)
library(readxl)

# Importing dataset

fluidPage(
  
  #sidebarPanel(
    
    # In case you want to let the graph interactive,
    # you can create a side bar + allows users input
    # and then on server.R you make a reactive variable
    # when you call the graph, so it keeps updating itself
  
    # dateRangeInput("daterange", "Choice the date",
    #                start = min(dataset$approval.date),
    #                end = max(dataset$approval.date),
    #                min = min(dataset$approval.date),
    #                max = max(dataset$approval.date),
    #                separator = " - ", format = "dd/mm/yy",
    #                startview = 'Week', language = 'us', weekstart = 1)
    
  #),
  
  mainPanel(
    # calling the output 'barplot' on server.R
    plotOutput('barplot')
  )
)