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


shinyServer(function(input, output) {
  events <<- filterEvents(input, output)
  campaignID <<- getCampaignID(input, output)
  contactsInSequence <<- getContactsInSequence(input, output)
  includeSequencePerformance(input, output)
  includeSequencePerformanceOverTime(input, output)
  includeSequenceActivity(input, output)
})  

filterEvents <- function(input, output) {
  reactive({
    dateStart <<- input$dateRange[1]
    dateEnd <<- input$dateRange[2]
    dat <- eventsDF %>% 
      filter(CampaignID == campaignID(),
             Date > dateStart, Date < dateEnd,
             Type %in% c("SENT", "CLICK", "OPEN", "REPLY"))
    
    dateStart <<- min(dat$Date)
    dat
  })
}

getCampaignID <- function(input, output) {
  reactive({
    campaignDF$ID[campaignDF$name %in% input$campaignName]
  })
}

getContactsInSequence <- function(input, output) {
  reactive({
    contactsDF$VID[which(contactsDF$VID %in% events()$RecipientID)]
  })
}

includeSequencePerformance <- function(input, output) {
  countType <- function(type, atgouconnect=FALSE) {
    events() %>% 
      filter(Type == type) %>%
      nrow
  }
  output$sent <- 
    renderText(paste("Sent: ", countType("SENT")))
  output$opens <- 
    renderText(paste("Opens: ", countType("OPEN")))
  output$clicks <- 
    renderText(paste("Clicks: ", countType("CLICK")))
  output$replies <-
    renderText(paste("Replies: ", countType("REPLY")))
}

includeSequencePerformanceOverTime <- function(input, output) {
  output$emailsBar <- renderPlotly({
    summary <- events() %>%
      mutate(Name = sapply(.$CampaignID, function(x) {
                           campaignDF$name[which(campaignDF$ID == x)[1]]})) %>%
      group_by(Date, Type, Name) %>%
      dplyr::summarize(n())
    
    colnames(summary) <- c("Date", "Type", "Name", "Count")
    p <- 
      ggplot(data=summary) +
      geom_col(position = "stack", aes(x=Date, y=Count, fill=Type)) +
      facet_wrap( ~ Name) +
      labs(x="Date", y="Number of Events", fill="") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
    ggplotly(p)
    })
}

includeSequenceActivity <- function(input, output) {
  output$activityGraph <- renderPlotly({
    
    summary <- (engagementDF %>% 
      filter(Type %in% c("CALL", "MEETING", "TASK"),
             ContactID %in% events()$RecipientID,
             Date > dateStart) %>%
      group_by(Date, Type) %>%
      dplyr::summarize(n()))
    
    colnames(summary) <- c("Date", "Type", "Count")
    p <- 
      ggplot(data=summary) +
      geom_col(aes(x=Date, y=Count, fill=Type)) +
      labs(title = str_c(input$campaignName, " Activity")) +
      labs(x="Date", y="Number of Events", fill="") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    ggplotly(p)
  })
}
