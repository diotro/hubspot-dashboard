############################   Packages:   ##########################
library(httr)
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(magrittr)
library(jsonlite)


########################   Additional files:   ######################
# credentials.R loads the hapikey (hapikey <- $YOURHAPIKEY)
source("credentials.R")

#######################   Utility functions:   ######################

# hubspotGet : given a partial hubspot api url and paramaters,
#  makes a request and parses as JSON
# 
# jsonParse : converts the given curl response to JSON
# 
# parseTimeStamp : converts the given unix timestamp to POSIXct




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
  timeStamp %>% as.character %>% as.numeric %>% divide_by(1000) %>% anytime 
}



##########################   Contacts:   ############################

allContacts <- vector(mode="list", length=100000)

listIndex <- 1
vidOffset <- 1
repeat {
  requestResult <- hubspotGet("/contacts/v1/lists/all/contacts/all?",
                              str_c("&showListMemberships=true&count=1000",
                                    "&properties=company",
                                    "&vidOffset=", vidOffset))
  contacts <- requestResult$contacts
  vidOffset <- requestResult$`vid-offset`
  
  for (i in seq_len(nrow(contacts))) {
    allContacts[[listIndex + i]] <- requestResult$contacts[i, ]
  }
  listIndex <- listIndex + nrow(contacts)
  
  if (listIndex %% 1000 < 10) {
    print(str_c(listIndex - 1, " CONTACTS RETRIEVED"))
  }
  
  if (!requestResult$`has-more`) {
    break
  }
}


allContacts <- allContacts[sapply(allContacts, Negate(is.null))]

contactDF <- data.frame(matrix(ncol=6, nrow=length(allContacts)))

parseContact <- function(contact) {
  data.frame(contact$addedAt,
             contact$vid,
             contact$`canonical-vid`,
             contact$`profile-token`,
             contact$`profile-url`,
             contact$properties$company)
}

contactDF <- ldply(allContacts, parseContact)

colnames(contactDF) <- c(
  "AddedAt", 
  "Vid", 
  "CanonicalVid",
  "ProfileToken", 
  "ProfileURL",
  "Company"
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
  warning('We reached 250 email campaigns. Update the "Emails" portion of\
collection.R to scrape all of them.')
}

campaignDF <- requestResult$campaigns
colnames(campaignDF) <- c("ID", "AppID", "AppName", "GroupID")
campaignDF$AppID <- str_replace(campaignDF$AppID, "\\s","")

parseCampaign <- function(campaign) {
  # To avoid getting rate-limited
  Sys.sleep(.1)
  
  campaign <- hubspotGet(str_c("/email/public/v1/campaigns/", campaign[1],
                               "?appId=", campaign[2], "&"))
  data.frame(c(campaign[-7], campaign$counters))
}

campaignDF <- campaignDF %>% adply(1, parseCampaign)


#########################    Companies    ############################


allCompanies <- vector(mode="list", length=0)
offset <- 1
repeat {
  request <- hubspotGet("/companies/v2/companies/paged?",
                        str_c("properties=notes_last_contacted",
                              "&properties=csm_system",
                              "properties=contact_listing_link",
                              "&limit=250&offset=", offset))
  companies <- request$companies
  cs <- sapply(seq_len(nrow(companies)), function(i) {companies[i,]})
  allCompanies <- append(allCompanies, cs)
  
  offset <- request$offset
  
  print(length(allCompanies) / 6)
  
  if (!request$`has-more`) {
    break;
  }
}

allCompanies <- matrix(allCompanies, ncol=6, byrow=TRUE)

allCompanies <- unnest(unnest(as.data.frame(allCompanies[, 1:4])))

colnames(allCompanies) <- c("portalid", "companyid", "isDeleted",
                            "timeSinceContacted", "csmSystem")


allContactProperties <- hubspotGet("/properties/v1/contacts/properties?")
allCompanyProperties <- hubspotGet("/properties/v1/companies/properties?")


# Deals in nurture, CSM system + time since last touched 
# first, last, email of primary contact


###########################    Deals    ##############################

allDeals <- matrix(ncol=7, nrow=0)

offset <- 1
repeat {
  
  request <- hubspotGet("/deals/v1/deal/paged?",
                      str_c("properties=dealstage&limit=250",
                            "&offset=", offset))
  
  deals <- request$deals
  deals <- t(sapply(seq_len(nrow(deals)), function(i) {deals[i,]}))
  allDeals <- rbind(allDeals, deals)
                   

  
  offset <- request$offset
  if (!request$hasMore) {
    break;
  }
}


allDealProperties <- hubspotGet("/properties/v1/deals/properties/?")



allDeals <- unnest(unnest(as.data.frame(allDeals[, c(1:3, 5)])))
colnames(allDeals) <- c("portalID", "dealID", "isDeleted", "dealstage")

assoc <- vector(mode="character", length=nrow(allDeals))
for (i in seq_along(allDeals$dealID)) {
  id <- allDeals$dealID[i]
  request <- hubspotGet(str_c("/deals/v1/deal/", id, "?"))
  a <- list(unlist(request))
  assoc[i] <- a[[1]][which(str_detect(names(a[[1]]), "associatedCompany"))[1]]
}


associatedDeals <- data.frame(cID = vector(mode="character", length=nrow(allCompanies)),
                              dID = vector(mode="character", length=nrow(allCompanies)),
                              stringsAsFactors = FALSE)

for (i in seq_along(allCompanies$companyid)) {
  cid <- allCompanies$companyid[i]
  did <- hubspotGet(str_c("/deals/v1/deal/associated/company/", cid, "/paged?"),
             "limit=1&properties=dealid")$deals$dealId
  associatedDeals[i, ] <- c(cid, did)
}



nurtureDeals <- allDeals %>% filter (dealstage == "nurture") %>% .$dealID

nurtureAssoc <- associatedDeals[which(associatedDeals$dID %in% nurtureDeals),]

allCompanies[sapply(nurtureAssoc$cID, 
                 function(cid) {which(cid == allCompanies$companyid)}), ] %>%
  filter(parseTimeStamp(timeSinceContacted) < as.POSIXct("2017-03-01"))
      
emails <- sapply(allContacts, function(x) {str_extract(str_extract(x$`identity-profiles`, "(?<=\\\").*?@.*?\\..*?(?=\")"), "[A-z0-9]*?@[A-z0-9]*?.[A-z0-9]*?$")}) 
vids <- sapply(allContacts, function(x) {x$vid})       


vids <- vector(mode="character", length=nrow(allCompanies))
for (i in seq_len(nrow(allCompanies))) {
  try(vids[i] <- hubspotGet(str_c("companies/v2/companies/", allCompanies[i, 2], "/vids?"))$vids[1])
}

emails[sapply(vids, function(x) {x %in% goodVids})]
       
       
       
       
