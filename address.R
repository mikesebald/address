library(RCurl)
library(RJSONIO)
library(plyr)
library(data.table)
library(xlsx)

setwd("../../R_data/address_data")

url <- function(address,
                return.call = "json",
                sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <-
    paste(root,
          return.call,
          "?address=",
          address,
          "&sensor=",
          sensor,
          sep = "")
  return(URLencode(u))
}

geoCode <- function(address) {
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc, simplify = FALSE)
  if (x$status == "OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    #    Sys.sleep(0.5)
  }
}

geoCodeX <- function(address) {
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc, simplify = FALSE)
  return(x)
}

validate.address <- function(address.input) {
  df <- data.frame(formated.address = character(),
                   location.type = character(),
                   stringsAsFactors = FALSE)

  for (i in 1:length(address.input)) {
    request.url <- url(address.input[i])
    response.json <- getURL(request.url)
    addr.response <- fromJSON(response.json, simplify = FALSE)
    #Sys.sleep(0.5)
    if (addr.response$status == "OK") {
      df[i, 1] <- addr.response$results[[1]]$formatted_address
      # hno <- addr.response$results[[1]]$address_components[[1]]$long_name
      # str <- addr.response$results[[1]]$address_components[[2]]$long_name
      # city.district <- 
      #   addr.response$results[[1]]$address_components[[3]]$long_name
      # city <- addr.response$results[[1]]$address_components[[4]]$long_name
      # state <- addr.response$results[[1]]$address_components[[7]]$long_name
      # country <- addr.response$results[[1]]$address_components[[8]]$long_name
      # country.iso <- addr.response$results[[1]]$address_components[[8]]$short_name
      # zip <- addr.response$results[[1]]$address_components[[9]]$long_name
      df[i, 2] <- addr.response$results[[1]]$geometry$location_type
      # address.row <- c(str = str,
      #                  hno = hno,
      #                  zip = zip,
      #                  city = city,
      #                  city.district = city.district,
      #                  state = state, 
      #                  country.iso = country.iso, 
      #                  quality = quality)
    # } else {
    #   formatted.address[i] = NA
    #   quality
    }
  }
  return(df)
}

file.columns <- 22
file.input <- fread("address_input.csv", 
                    colClasses = rep("character", file.columns))
#View(file.input)
#View(data.frame(colnames(file.input)))
dt.address.input <- file.input[, 8:18, with = FALSE] 
#View(dt.address.input)

dt.sample <- dt.address.input[runif(500, 1, nrow(dt.address.input)), ]
#View(dt.sample)

dt.sample.gb <- dt.sample[new_country_code == "GB"]
dt.sample.usca <- dt.sample[new_country_code == "US" |
                            new_country_code == "CA"]
dt.sample.rest <- dt.sample[new_country_code != "GB" & 
                            new_country_code != "US" &
                            new_country_code != "CA"]

address.gb <- paste(dt.sample.gb$company_2,
                    dt.sample.gb$department,
                    dt.sample.gb$address1,
                    dt.sample.gb$address2,                      
                    dt.sample.gb$address3,                      
                    dt.sample.gb$zip,                      
                    dt.sample.gb$city,                      
                    dt.sample.gb$new_country_code,
                    sep = ",")
address.usca <- paste(dt.sample.usca$address1,
                      dt.sample.usca$address2,                      
                      dt.sample.usca$address3,                      
                      dt.sample.usca$zip,                      
                      dt.sample.usca$city,                      
                      dt.sample.usca$state,                      
                      dt.sample.usca$new_country_code,
                      sep = ",")
address.rest <- paste(dt.sample.rest$address1,
                      dt.sample.rest$address2,                      
                      dt.sample.rest$address3,                      
                      dt.sample.rest$zip,                      
                      dt.sample.rest$city,                      
                      dt.sample.rest$new_country_code, sep = ",")

address.gb <- gsub(",,+", ",", address.gb)
address.usca <- gsub(",,+", ",", address.usca)
address.rest <- gsub(",,+", ",", address.rest)

system.time({
  address.out <- validate.address(address.gb)
  address.gb <- cbind(address.gb, address.out)
  address.out <- validate.address(address.usca)
  address.usca <- cbind(address.usca, address.out)
  address.out <- validate.address(address.rest)
  address.rest <- cbind(address.rest, address.out)
})

write.xlsx(address.gb, "address.result.xlsx", sheetName = "GB")
write.xlsx(address.usca, "address.result.xlsx", sheetName = "US and CA", append = TRUE)
write.xlsx(address.rest, "address.result.xlsx", sheetName = "Misc.", append = TRUE)
