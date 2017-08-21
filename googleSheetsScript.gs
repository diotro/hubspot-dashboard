// finds the size of each deal stage at each of the given dates, using the provided hapikey
function hubspotPipelineAt(dates, hapikey) {
  var stageNameLookup = getDealStageLookup(hapikey)
  var deals = getDeals(hapikey)
  
  var out = [];
  out.push(colNames(hapikey))
  for (var dateIndex in dates) {
    var date = new Date(dates[dateIndex]);
    var dealsAtDate = parseDeals(deals, date, stageNameLookup)
    out.push(dealsAtDate);
  }
  return out;
}

// finds each dealstage name, in the order they're listed in the pipeline
function colNames(hapikey) {
  var stageNameLookup = getDealStageLookup(hapikey);
  var out = [];
  for (var index in Object.keys(stageNameLookup)) {
    var stage = stageNameLookup[Object.keys(stageNameLookup)[index]];
    out.push(stage)
  }
  return out;
}

// Returns a dictionary from deal stage labels to names
function getDealStageLookup(hapikey) {
  var stages = hsGet(hapikey, '/deals/v1/pipelines')[0].stages
  var out = {};
  for (var index in stages) {
    var stage = stages[index]
    out[stage.stageId] = stage.label;
  }
  return out;
}

// Turns the API responses into usable data, pulling out only the important fields
// with their values at the given date
function parseDeals(deals, date, lookup) {
  var dealsAtDate = [];
  for (var index in deals) {
    var deal = deals[index];
    var amount = getAmount(deal, date);
    var stage = getStage(deal, date, lookup);
    dealsAtDate.push({amount:amount, stage:stage})
  }
  var row = [];
  for (var index in Object.keys(lookup)) {
    var stage = lookup[Object.keys(lookup)[index]];
    var value = totalDealValue(dealsAtDate, stage)
    row.push(value)
  }
  return row;
}

// gets the amount the given deal was worth at the given date
function getAmount(deal, date) {
  var amount = deal.properties.amount;
  var out = 0;
  if (amount) {
    out = getLastVersionBefore(amount.versions, date, 0)
  }
  return processNumber(out);
}

// ensures that the given value is a number, and removes "," as in "10,000"
function processNumber(val) {
  var out;
  if (typeof val === "string") {
    out = val.replace(",", "");
  }
  if (out) {
    return +out;
  } else {
    return 0;
  }
}

// gets the stage a deal was in at the given date using the given name lookup table
function getStage(deal, date, lookup) {
  var stages = deal.properties.dealstage;
  if (stages) {
    return lookup[getLastVersionBefore(stages.versions, date, "NA")]
  } else {
    return "NA";
  }
}


// Given a list of versions (as returned by the HubSpot API), returns the value of the
// deal property at the given date.
function getLastVersionBefore(versions, date, base) {
  if (versions.length === 1) {
    return versions[0].value;
  }
  for (var index in versions) {
    var version = versions[index];
    // versions are always returned in reverse order, so the first one below or equal to 
    // the date is the most recent one 
    if (new Date(version.timestamp) <= date) {
      return version.value;
    }
  }
  return base;
}

// returns all deal IDs in the given portal
function getDeals(hapikey) {
  var deals = [];
  var properties = "&propertiesWithHistory=dealstage&propertiesWithHistory=amount";
  
  var hsOut = hsGet(hapikey, "/deals/v1/deal/paged", "&limit=250" + properties)
  for (var index in hsOut.deals) {
    var deal = hsOut.deals[index]
    deals.push(deal)
  }
  while (hsOut.hasMore) {
    hsOut = hsGet(hapikey, "/deals/v1/deal/paged", "&limit=250&offset=" + hsOut.offset + properties)
    for (var index in hsOut.deals) {
      var deal = hsOut.deals[index]
      deals.push(deal)
    }
  }
  return deals;
}

// uses the given hapikey to request the given url from Hubspot's API. Locks to prevent exceeding the rate limit
function hsGet(hapikey, url, params) {
  var lock = LockService.getScriptLock();
  while(!lock.hasLock()) {
    lock.tryLock(150); 
  }
  if (url == undefined) {
    console.error("Cannot get undefined url") 
  } 
  if (hapikey == undefined) {
    console.error("Cannot retrieve data with undefined hapikey")
  }
  
  var out;
  if (params) {
    out = JSON.parse(UrlFetchApp.fetch("api.hubspot.com" + url + "?hapikey=" + hapikey + params).getContentText())
  } else {
    out = JSON.parse(UrlFetchApp.fetch("api.hubspot.com" + url + "?hapikey=" + hapikey).getContentText());
  }
  lock.releaseLock();
  return out;
}

// finds the total deal value of deals in the given dealstage
function totalDealValue(deals, dealstage) {
  var total = 0;
  for (var index in deals) {
    var deal = deals[index];
    if (deal.stage == dealstage) {
      total += +deal.amount;
    }
  }
  return total;
}
