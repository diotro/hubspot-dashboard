loadData <- function() {
  inputDir <- "data"
  # Read all the files into a list
  files <- list.files(inputDir, full.names = TRUE)
  lapply(files, load, envir=.GlobalEnv)
}


loadData() 

fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", label="Date Range", start=Sys.Date() - 7, end=Sys.Date()),
      
      textOutput("people"),
      
      selectizeInput("people", label="People", 
                      unique(ownersDF$firstName), multiple=TRUE),
      selectizeInput("types", label="Type", 
                      unique(engagementDF$Type), multiple=TRUE)
    ),
    mainPanel(
      plotlyOutput("activityOverTime") 
    )
  )
)