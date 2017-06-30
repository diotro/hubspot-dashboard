library(shiny)
library(plyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(magrittr)
library(scales)
library(stringr)
library(tidyr)
library(dplyr)

loadData <- function() {
  inputDir <- "data"
  # Read all the files into a list
  files <- list.files(inputDir, full.names = TRUE)
  lapply(files, load, envir=.GlobalEnv)
}


loadData() 
fluidPage(
  
  titlePanel("Campaign Performance"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("campaignName", "Campaign", 
                  choices=campaignDF %>% filter(sent > 20) %>% use_series(name)),
      dateRangeInput("dateRange", "Date Range", start="2017-01-01")
    ),
    mainPanel(
      plotlyOutput("emailsBar"),
      HTML("<br><br><br><br><br><br>"),
      fillRow(textOutput("sent"),
              textOutput("opens"),
              textOutput("clicks"),
              textOutput("replies")),
      textOutput("data"),
      HTML("<br><br><br>"),
      plotlyOutput("activityGraph")
    )
  )
)
