library(RCurl)
library(RJSONIO)
library(plyr)
library(data.table)

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

validate.address <- function(address.input) {
  request.url <- url(address.input)
  response.json <- getURL(request.url)
  addr.response <- fromJSON(response.json, simplify = FALSE)
  if (addr.response$status == "OK") {
    hno <- addr.response$results[[1]]$address_components[[1]]$long_name
    str <- addr.response$results[[1]]$address_components[[2]]$long_name
    city.district <- 
      addr.response$results[[1]]$address_components[[3]]$long_name
    city <- addr.response$results[[1]]$address_components[[4]]$long_name
    state <- addr.response$results[[1]]$address_components[[7]]$long_name
    country <- addr.response$results[[1]]$address_components[[8]]$long_name
    country.iso <- addr.response$results[[1]]$address_components[[8]]$short_name
    zip <- addr.response$results[[1]]$address_components[[9]]$long_name
    quality <- addr.response$results[[1]]$geometry$location_type
    address.row <- c(str = str,
                     hno = hno,
                     zip = zip,
                     city = city,
                     city.district = city.district,
                     state = state, 
                     country.sio = country.iso, 
                     quality = quality)
    return(address.row)
  } else {
    return(rep(NA, 8))
  }
}

address <- validate.address("Denzenbergweg 16, Breitenholz, DE")
address

file.columns <- 22
file.input <- fread("address_input.csv", 
                    colClasses = rep("character", file.columns))
View(file.input)
View(data.frame(colnames(file.input)))
address.input <- file.input[, 8:18, with = FALSE] 
View(address.input)

sample <- address.input[runif(50, 1, nrow(address.input)), ]
View(sample)

