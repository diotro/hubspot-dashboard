############################   Packages:   ##########################

library(anytime)
library(stringr)
library(magrittr)
library(httr)
library(plyr)
library(tidyr)
library(dplyr)
library(jsonlite)
library(shiny)
library(plotly)
library(ggplot2)
library(scales)


########################   Additional files:   ######################

# credentials.R loads the hapikey (hapikey <- $YOURHAPIKEY)
setwd("~/Documents/uConnect/hubspot-dashboard")
source("credentials.R")


#######################   Utility functions:   ######################

# hubspotGet : Given a partial hubspot api url and paramaters,
#  makes a request and parses to JSON
# 
# jsonParse : Converts the given curl response to JSON
# 
# parseTimeStamp : converts the given unix timestamp (in microseconds)
# to POSIXct. Works on all hubspot timestamps.




# hubspotGet : character url    : the partial url to query
#            : character params : the params to put in the query 
# 
# returns    : list representing the JSON from the API
hubspotGet <- function(url, params=c()) {
  # TODO: check status code to ensure response
  #       input validation
  #       shortcuts for api calls
  if (length(params)) {
    fullURL <- str_c("https://api.hubapi.com", url,
                     "hapikey=", hapikey, "&", params)
  } else {
    fullURL <- str_c("https://api.hubapi.com/", url,
                     "hapikey=", hapikey)
  }
  GET(fullURL) %>%
    jsonParse
}

# jsonParse : character request : a curl request
# 
# returns    : any JSON from the request's content
jsonParse <- function(request) {
  fromJSON(rawToChar(request$content))
}

# parseTimeStamp : numeric timeStamp : the timestamp to parse
#
# return         : POSIXct : a date object
parseTimeStamp <- function(timeStamp) {
  timeStamp %>% as.numeric %>% divide_by(1000) %>% 
    as.POSIXct(origin="1970-01-01")
}






############################   Emails    ##############################

getCampaigns <- function() {
  # No loop because there's only one page, so it's important to make it
  # break loudly if we ever wrap to >250 email campaigns.... 
  requestResult <- hubspotGet("/email/public/v1/campaigns/by-id?",
                              str_c("&limit=250"))
  if (requestResult$hasMore) {
    warning('We reached 250 email campaigns. Update the "Emails" portion of\
collection.R to scrape all of them.')
  }
  
  campaignDF <- requestResult$campaigns
  colnames(campaignDF) <- c("ID", "AppID", "AppName", "GroupID")
  campaignDF$AppID <- str_replace(campaignDF$AppID, "\\s","")
  
  parseCampaign <- function(campaign) {
    # To avoid getting rate-limited
    Sys.sleep(.05)
    
    campaign <- hubspotGet(str_c("/email/public/v1/campaigns/", campaign[1],
                                 "?appId=", campaign[2], "&"))
    data.frame(c(campaign[-7], campaign$counters))
  }
  
  campaignDF <- campaignDF %>% adply(1, parseCampaign)
  campaignDF$lastProcessingFinishedAt <- 
    parseTimeStamp(campaignDF$lastProcessingFinishedAt)
  
  campaignDF
}

campaignDF <- getCampaigns()

processEmailEvent <- function(event) {
  replaceNullWithNA <- function(x) {
    x[which(is.null(x))] <- NA
    x
  }
  out <- replaceNullWithNA(c(event$id,
                             event$created,              
                             event$appName,
                             event$type,
                             event$recipient,
                             event$emailCampaignId,
                             event$status,
                             event$from))
  length(out) <- 8
  out
}

eventsDF <- matrix(nrow=0, ncol=8)
batch <- hubspotGet("/email/public/v1/events?",
                    "limit=1000")
while (batch$hasMore) {
  batch <- hubspotGet("/email/public/v1/events?",
                      str_c("&limit=1000",
                            "&offset=", batch$offset))
  
  events <- batch$events
  out <- matrix(nrow=nrow(events), ncol=8)
  for (i in seq_len(nrow(events))) {
    events[i, ] %>%
      processEmailEvent() -> out[i,]
  }
  eventsDF <- rbind(eventsDF, out)
  print(str_c(nrow(eventsDF), " EMAILS PROCESSED"))
}

eventsDF <- as.data.frame(eventsDF, stringsAsFactors = FALSE)

colnames(eventsDF) <- c("ID", "Created", "AppName", "Type", 
                        "Recipient", "CampaignID", "Status", "Sender")
eventsDF$Created <- parseTimeStamp(eventsDF$Created)
eventsDF$Date <- as.Date(eventsDF$Created)
eventsDF$Recipient %<>% sapply(function(x) {
  if (str_detect(x, "<.*?>")) {str_extract(x, "(?<=<).*(?=>)")} else {x}})


###########################   Contacts    ##############################

contacts <- unique(eventsDF$Recipient)
n <- length(contacts)
VID = integer(n)
CanonicalVID = integer(n)
Email = character(n)
Company = character(n)
CompanyID = integer(n)
for(i in seq_len(n)) {
  try({
    onFailNA <- function(x) tryCatch(x, error=function(e) NA)
    contact <- hubspotGet(str_c("/contacts/v1/contact/email/", contacts[i],"/profile?"))
    
    onFailNA(VID[i] <-  contact$vid)
    onFailNA(CanonicalVID[i] <-  contact$`canonical-vid`)
    onFailNA(Email[i] <-  contact$properties$email$value)
    onFailNA(Company[i] <-  contact$`associated-company`$properties$name$value)
    onFailNA(CompanyID[i] <-  contact$properties$associatedcompanyid$value)
    
  })
  if (i %% 100 == 0) {
    print(str_c(i, " CONTACTS PROCESSED"))
  }
  Sys.sleep(.03)
}

contactsDF <- data.frame(VID, CanonicalVID, Email, Company, CompanyID)
rm(VID, CanonicalVID, Email, Company, CompanyID)
eventsDF$RecipientID <- sapply(eventsDF$Recipient, function(x) {
  contactsDF$CanonicalVID[which(contactsDF$Email == x)]
})


#########################   Engagements    ###########################

offset <- 1
engagementDFcolnames <-  c("ID", "PortalID", "Active", "Created", 
                           "Updated", "OwnerID", "Type", "Timestamp", "ContactID")
engagementDF <- matrix(nrow=0,ncol=length(engagementDFcolnames))
repeat {
  batch <- hubspotGet("/engagements/v1/engagements/paged?",
                      str_c("&offset=", offset,
                            "&limit=", 250))
  processedBatch <- batch$results$engagement %>% 
    select(matches("^(id|portalId|active|createdAt|lastUpdated|ownerID|type|timestamp)$"))
  processedBatch$ContactID <- sapply(batch$results$associations$contactIds,
                                     function(x)
                                       tryCatch(x[[1]], error=function(e) NA))
  engagementDF %<>% rbind(processedBatch)
  offset <- batch$offset
  if (nrow(engagementDF) %% 1000 == 0) {
    print(str_c(nrow(engagementDF), " ENGAGEMENTS PROCESSED"))
  }
  if (!batch$hasMore) {
    break
  }
}


colnames(engagementDF) <- engagementDFcolnames
engagementDF$Created <- parseTimeStamp(engagementDF$Created)
engagementDF$Date <- as.Date(engagementDF$Created)



save(engagementDF, file="campaigns/data/engagement")
save(eventsDF, file="campaigns/data/events")
save(campaignDF, file="campaigns/data/campaigns")
save(contactsDF, file="campaigns/data/contacts")

