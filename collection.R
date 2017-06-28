############################   Packages:   ##########################
library(anytime)
library(stringr)
library(magrittr)
library(httr)
library(plyr)
library(tidyr)
library(dplyr)
library(jsonlite)
library(RMySQL)

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
  timeStamp %>% divide_by(1000) %>% as.POSIXct(origin="1970-01-01")
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
    Sys.sleep(.08)
    
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

colnames(eventsDF) <- c("ID", "Created", "AppName", "Type", "Recipient", "CampaignID", "Status", "Sender")

# Process types: We care about outbound vs inbound "SENT"
eventsDF$Type <- apply(eventsDF, 1, function(x) {
  if(!(x[4] %in% "SENT")) { x[4] }
  else if (str_detect(x[5], "gouconnect.com")) {
    "REPLY"
  } else { "SENT"}
})

eventsDF$Created <- parseTimeStamp(eventsDF$Created)

save(eventsDF, file="events")
