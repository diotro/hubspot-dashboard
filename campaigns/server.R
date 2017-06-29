loadData <- function() {
  inputDir <- "campaigns/data"
  # Read all the files into a list
  files <- list.files(inputDir, full.names = TRUE)
  # Load every file in the list into the global environment
  sapply(files, load, envir=.GlobalEnv)
}

loadData() 

shinyServer(function(input, output) {
  if (!exists("eventsDF")) {
    loadData() 
  }
  events <<- filterEvents(input, output)
  campaignID <<- getCampaignID(input, output)
  contactsInSequence <<- getContactsInSequence(input, output)
  includeSequencePerformance(input, output)
  includeSequencePerformanceOverTime(input, output)
  includeSequenceActivity(input, output)
})  

filterEvents <- function(input, output) {
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
    contactsDF$VID[which(contactsDF$VID %in% events()$RecipientID)]
  })
}

includeSequencePerformance <- function(input, output) {
  countType <- function(type, atgouconnect=FALSE) {
    events() %>% 
      filter(Type == type,
             str_detect(Recipient,"@gouconnect\\.com") == atgouconnect) %>%
      nrow
  }
  output$sent <- 
    renderText(paste("Sent: ", countType("SENT")))
  output$opens <- 
    renderText(paste("Opens: ", countType("OPEN")))
  output$clicks <- 
    renderText(paste("Clicks: ", countType("CLICK")))
  output$replies <-
    renderText(paste("Replies: ", countType("REPLY", atgouconnect=TRUE)))
}

includeSequencePerformanceOverTime <- function(input, output) {
  output$emailsBar <- renderPlotly({
    summary <- events() %>%
      group_by(Date, Type) %>%
      summarize(n())
    
    colnames(summary) <- c("Date", "Type", "Count")
    p <- 
      ggplot(data=summary) +
      geom_col(aes(x=Date, y=Count, fill=Type)) +
      labs(title = str_c(input$campaignName, " Performance")) +
             xlab("Date") + ylab("Number of Events") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_y_continuous(expand = c(0,0))
    ggplotly(p)
    })
}

includeSequenceActivity <- function(input, output) {
  output$activityGraph <- renderPlotly({
    
    summary <- engagementDF %>% 
      filter(Type %in% c("CALL", "MEETING", "TASK"),
             ContactID %in% events()$RecipientID) %>%
      group_by(Date, Type) %>%
      summarize(n())
    
    colnames(summary) <- c("Date", "Type", "Count")
    p <- 
      ggplot(data=summary) +
      geom_col(aes(x=Date, y=Count, color=Type, fill=Type)) +
      labs(title = str_c(input$campaignName, " Activity")) +
      xlab("Date") + ylab("Number of Events") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      scale_y_continuous(expand = c(0,0))
    ggplotly(p)
  })
}
