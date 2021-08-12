#### Script to get crime data from hoyo de crimen api 
# Created 2/17/2021
# last edited 2/28/21
######
library(sf)
library(tidyverse)
library(httr)
library(jsonlite)
library(geojsonsf)
library(rgdal)
library("DCluster")
library("jsonlite")
library("RCurl")
library("jsonlite")
library("rgdal")
library("rgeos")
library("ggplot2")
library("ggmap")
library("RColorBrewer")
library("stringr")
library("scales")
library("geojsonio")
library("downloader")
library("spdep")
library("viridis")
library("maptools")
library("rvest")
library("dplyr")
library("stringr")
library("stringi")
library("geojsonio")
library("knitr")
library(lubridate)

# set the api endpoint as the path. Start with the types of crimes
path <- "https://hoyodecrimen.com/api/v1/crimes"
request <- GET(url = path, 
               query = list()
)
# use the GET function from the httr package 
# first, supply the path to the api endpoint and provide search parameters in the form of a list to the query argument 
# if the request fails the api will return a non-200 status code 
request$status_code

# Next we parse the content returned from the server as text using the content function.
crimes_response <- content(request, as = "text", encoding = "UTF-8")

# Then we’ll parse the JSON content and and convert it to a data frame.
crimes <- fromJSON(crimes_response, flatten = TRUE) %>% 
  data.frame()

# next do the same for cuadrantes
 path <- "https://hoyodecrimen.com/api/v1/cuadrantes"
 request <- GET(url = path, 
               query = list()
)
request$status_code
cuadrantes_response <- content(request, as = "text", encoding = "UTF-8")
cuadrantes <- fromJSON(cuadrantes_response, flatten = TRUE) %>% 
  data.frame()


 #now get time series of crimes.
 #loop through cuadrantes to get all the data 
ts_all <- c()
for(i in 1:length(cuadrantes$rows.cuadrante)){
path <- paste("https://hoyodecrimen.com/api/v1/cuadrantes/", cuadrantes$rows.cuadrante[i], "/crimes/all/series", sep = "")
request <- GET(url = path, 
               query = list()
)
request$status_code
ts_response <- content(request, as = "text", encoding = "UTF-8")
ts <- fromJSON(ts_response, flatten = TRUE) %>% 
  data.frame()
ts_all <- bind_rows(ts_all, ts)
}
save(ts_all, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/crime/time_series.Rdata")

# Get shapefile of cuadrantes and sectores
#tmp_cuadrantes = tempfile("cuads", fileext = ".json")
#download("https://hoyodecrimen.com/api/v1/cuadrantes/geojson", tmp_cuadrantes)
# read the geojson into a spatial object
cuadrantes = readOGR(tmp_cuadrantes, "OGRGeoJSON", verbose = FALSE)
cuadrantes <- st_as_sf(cuadrantes)
save(cuadrantes, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/2_CollectiveActionInfrastructure/2_Data/crime/cuadrantes.Rdata")


tmp_sectores = tempfile("secs", fileext = ".json")
download("https://hoyodecrimen.com/api/v1/sectores/geojson", tmp_sectores)
sectores = readOGR(tmp_sectores, "OGRGeoJSON", verbose = FALSE)
sectores <- st_as_sf(sectores)

