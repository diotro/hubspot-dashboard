library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Campaign Performance"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("campaignName", "Campaign", choices=unique(campaignDF$name)),
      dateRangeInput("dateRange", "Date Range", start="2017-01-01")
    ),
    mainPanel(
      plotlyOutput("emailsOverTime"),
      textOutput("enrollment"),
      textOutput("opens"),
      textOutput("clicks"),
      textOutput("replies")
    )
  )
))
