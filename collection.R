

############################   Packages:   ##########################
library(httr)
library(dplyr)
library(jsonlite)


########################   Additional files:   ######################
# credentials.R loads the hapikey (hapikey <- $YOURHAPIKEY)
source("credentials.R")

#######################   Utility functions:   ######################

# hubspotGet : given a partial hubspot api url and paramaters,
#              makes a request and parses as JSON
#
# jsonParse : converts the given curl response to JSON



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




##########################   Contacts:   ############################

allContacts <- vector(mode="list", length=100000)

listIndex <- 1
vidOffset <- 1
repeat {
  requestResult <- hubspotGet("/contacts/v1/lists/all/contacts/all?",
                              str_c("&showListMemberships=true&count=1000",
                                    "&vidOffset=", vidOffset))
  contacts <- requestResult$contacts
  vidOffset <- requestResult$`vid-offset`
  
  for (i in seq_len(nrow(contacts))) {
    allContacts[[listIndex + i]] <- requestResult$contacts[i, ]
  }
  listIndex <- listIndex + nrow(contacts)
  
  if (listIndex %% 1000 < 10) {
    print(str_c(listIndex - 1, " CONTACTS SCRAPED"))
  }
  
  if (!requestResult$`has-more`) {
    break
  }
}

allContacts <- allContacts[sapply(allContacts, Negate(is.null))]

contactDF <- data.frame(matrix(ncol=5, nrow=length(allContacts)))

parseContact <- function(contact) {
  data.frame(contact$addedAt,
             contact$vid,
             contact$`canonical-vid`,
             contact$`profile-token`,
             contact$`profile-url`)
}

contactDF <- ldply(allContacts, parseContact)
colnames(contactDF) <- c(
  "AddedAt", 
  "Vid", 
  "CanonicalVid",
  "ProfileToken", 
  "ProfileURL"
)


contactPropertiesDF <- data.frame(col.names=c(
  "Vid",
  "PropertyName",
  "PropertyValue"
))

parseContactProperties <- function(contact) {
  properties <- contact$properties
  out <- data.frame(matrix(ncol=3, nrow=length(properties)))
  
  for (i in seq_along(properties)) {
    out[i, ] <- c(contact$vid,
                  names(properties)[i],
                  properties[i], use.names = FALSE)
  }
  out
}

contactPropertiesDF <- ldply(allContacts, parseContactProperties)

colnames(contactPropertiesDF) <- c(
  "Vid",
  "PropertyName",
  "PropertyValue"
)

############################   Emails    ############################

# No loop because there's only one page, so it's important to make it
# break loudly if we ever wrap to >250 email campaigns.... 
requestResult <- hubspotGet("/email/public/v1/campaigns/by-id?",
                            str_c("&limit=250"))
if (requestResult$hasMore) {
  stop('We reached 250 email campaigns. Update the "Emails" portion of\
collection.R to scrape all of them. Or at least to remove this message.')
}

campaignDF <- requestResult$campaigns
colnames(campaignsDF) <- c("ID", "AppID", "AppName", "GroupID")

parseCampaign <- function(campaign) {
  campaign <- hubspotGet(str_c("/email/public/v1/campaigns/", campaign$ID,
                               "?appId=", campaign$AppID, "&"))
  campaignData <- data.frame(campaign[-7], campaign$counters)
}

campaignDF <- adply(campaignDF, 1, parseCampaign)
