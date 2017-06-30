shinyServer(function(input, output, session) {
  values <- reactive({reactiveValues(people = input$people, type=input$types)})
  
  filteredData <<- filterData(input, output)
  
  includeActivityOverTime(input, output)
})

filterData <- function(input, output) {
  reactive({
    engagementDF %>% 
      filter(OwnerName %in% input$people,
             Date > input$dateRange[1], Date < input$dateRange[2],
             Type %in% input$types)
  })
}


includeActivityOverTime <- function(input, output) {
  output$activityOverTime <- renderPlotly({
    dat <- filteredData() %>%
      group_by(Date, Type, OwnerName) %>%
      summarize(n())
    colnames(dat) <- c("Date", "Type", "Owner", "Count")
    
    # Only facet graph if multiple people AND multiple types selected
    if (length(input$people) > 1 & length(input$types) > 1) {
      p <- ggplot(data=dat, aes(x = Date, y = Count, fill = Type)) +
        geom_col(position = "stack", stat = "identity") +
        facet_wrap( ~ Owner) +
        theme_bw()
    } else {
      # If either input has length one, fill by group
      if (length(input$people) > 1) {
        fill <- "Owner"
      } else {
        fill <- "Type"
      }
      p <- ggplot(data=dat, aes_string(x = "Date", y = "Count", fill = eval(fill))) +
        geom_col(position = "stack", stat="identity") + 
        theme_bw()
      ggplotly(p)
    }
  })
}