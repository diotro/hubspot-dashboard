library(shiny)
library(plotly)
library(ggplot2)
library(scales)
library(stringr)


shinyServer(function(input, output) {
  dat <<- filterData(input, output)
  campaignID <- getCampaignID(input, output)
  contactsInSequence <<- getContactsInSequence(input, output)
  includeSequencePerformance(input, output)
  includeSequencePerformanceOverTime(input, output)
})  

filterData <- function(input, output) {
  reactive({
    dateStart <- input$dateRange[1]
    dateEnd <- input$dateRange[2]
    eventsDF %>% 
      filter(CampaignID == campaignID(),
             Date > dateStart, Date < dateEnd,
             Type %in% c("SENT", "CLICK", "OPEN", "REPLY"))
  })
}

getCampaignID <- function(input, output) {
  reactive({
    campaignDF$ID[campaignDF$name %in% input$campaignName]
  })
}

getContactsInSequence <- function(input, output) {
  reactive({
    campaignDF$ID[campaignDF$name %in% input$campaignName]
  })
}

includeSequencePerformance <- function(input, output) {
  output$sent <- 
    renderText(paste("Sent: ", 
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
  output$emailsBar <- renderPlotly({
    summary <- dat() %>%
      group_by(Date, Type) %>%
      summarize(n())
    
    colnames(summary) <- c("Date", "Type", "Count")
    p <- 
      ggplot(data=summary) +
      geom_col(aes(x=Date, y=Count, fill=Type)) +
      labs(title = str_c(input$campaignName, " Performance")) +
             xlab("Date") + ylab("Number of Events") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            plot.margin = margin(10, 10, 10, 10))
    ggplotly(p)
    })
}

includeSequenceActivity <- function(input, output) {
  engagementDF %>% 
    filter(engagementDF$ContactID %in% contactsInSequence())
  output$sequenceActivity <- renderPlotly({
    summary <- dat() %>%
      group_by(createdAt, type) %>%
      summarize(n())
    
    colnames(summary) <- c("Date", "Type", "Count")
    p <- 
      ggplot(data=summary) +
      geom_col(aes(x=Date, y=Count, fill=Type)) +
      labs(title = str_c(input$campaignName, " Performance")) +
      xlab("Date") + ylab("Number of Events") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            plot.margin = margin(10, 10, 10, 10))
    ggplotly(p)
  })
}
