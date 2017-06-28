library(shiny)
library(plotly)


shinyUI(fluidPage(
  titlePanel("Campaign Performance"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("campaignName", "Campaign",
                  unique(campaignDF$name)),
      dateRangeInput("dateRange", "Date Range", start="2017-01-01")
    ),
    mainPanel(
      textOutput("enrollment"),
      textOutput("opens"),
      textOutput("clicks"),
      textOutput("replies"),
      plotlyOutput("emailsOverTime")
    )
  )
))
