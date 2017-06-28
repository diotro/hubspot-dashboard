library(shiny)
library(plotly)
library(stringr)


source("data/helper.R")

shinyServer(function(input, output) {
  dat <<- filterData(input, output)
  includeSequencePerformance(input, output)
  includeSequencePerformanceOverTime(input, output)
})  

filterData <- function(input, output) {
  reactive({
    dateStart <- input$dateRange[1]
    dateEnd <- input$dateRange[2]
    eventsDF %>% 
      filter(CampaignID == campaignDF$ID[campaignDF$name %in% input$campaignName],
             Created > dateStart, Created < dateEnd,
             Type %in% c("SENT", "CLICK", "OPEN", "REPLY"))
  })
}

includeSequencePerformance <- function(input, output) {
  output$enrollment <- 
    renderText(paste("Enrollment: ", 
                     nrow(dat() %>% filter(Type == "SENT",
                                           !str_detect(Recipient,"@gouconnect\\.com")))))
  output$opens <- 
    renderText(paste("Opens: ", 
                     nrow(dat() %>% filter(Type == "OPEN",
                                           !str_detect(Recipient,"@gouconnect\\.com")))))
  output$clicks <- 
    renderText(paste("Clicks: ", 
                     nrow(dat() %>% filter(Type == "CLICK",
                                           !str_detect(Recipient,"@gouconnect\\.com")))))
  output$replies <-
    renderText(paste("Replies: ", 
                     nrow(dat() %>% filter(Type == "REPLY"))))
}

includeSequencePerformanceOverTime <- function(input, output) {
  output$emailsOverTime <- renderPlotly({
    summary <- dat() %>%
      group_by(Created, Type) %>%
      summarize(n())
    p <- 
      ggplot(data=summary) +
      geom_point(aes(x=as.POSIXct(as.numeric(Created), origin="1970-01-01"), y=`n()`, fill=Type)) +
      labs(title = str_c(input$campaignName, " Performance")) +
             xlab("Date") + ylab("Number of Events")
    print(ggplotly(p))
    })
}
