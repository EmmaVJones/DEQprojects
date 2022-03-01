

# this script makes a new method for delineating sites and watersheds and organizing them into a single shape files.
# the script will replace the arduous task of manually delineating watersheds and adding a field to each object 
# containing the site name the file references for each site and polygon file. This script was built with example code
# from Ryan King https://ryan-hill.github.io/sfs-r-gis-2018/modules/rasters/extract-raster-data-r/ exercise 3
# and modified by Emma Jones to fit DEQ's needs.

library(tidyverse)
library(sf)
library(jsonlite)
library(geojsonio)

# Single watershed function
streamStats_Delineation_single <- function(state, # StreamStats state info e.g. 'VA'
                                           longitude, # longitude value, numeric
                                           latitude, # latitude value, numeric
                                           UID # Unique station identifier to append to dataset
){ # function based off code by Ryan King https://ryan-hill.github.io/sfs-r-gis-2018/modules/rasters/extract-raster-data-r/ exercise 3
  outData <- list(polygon = list(), point = list())
  
  query <-  paste0('https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=',
                   state, '&xlocation=', toString(longitude), 
                   '&ylocation=', toString(latitude), 
                   '&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=true') 
  # data pulled
  mydata <-  tryCatch({
    fromJSON(query, simplifyVector = FALSE, simplifyDataFrame = FALSE)},
    error = function(cond){
      message(paste('StreamStats Error:', cond))
      return(NULL)},
    warning = function(cond){
      message(paste('StreamStats Error:', cond))
      return(NULL)})
  
  # catch if server bomb out for any reason
  if(!is.null(mydata)){
    # organize watershed
    poly_geojsonsting <- toJSON(mydata$featurecollection[[2]]$feature, auto_unbox = TRUE)
    outData$polygon <- geojson_sf(poly_geojsonsting) %>%
      mutate(UID = UID) %>%
      dplyr::select(UID)
    
    # organize point
    point_geojsonsting <- toJSON(mydata$featurecollection[[1]]$feature, auto_unbox = TRUE)
    outData$point <- geojson_sf(point_geojsonsting) %>%
      mutate(UID = UID)%>%
      dplyr::select(UID)
  } else {
    outData$polygon <- st_sf(UID = NA, geometry=  st_sfc(st_polygon()), crs = 4326)#data.frame(UID = NA, geometry= NA)
    outData$point <- st_sf(UID = NA, geometry=  st_sfc(st_point()), crs = 4326)#data.frame(UID = NA, geometry= NA)
  }
  
  return(outData)
}




# Multiple Watershed delineation
streamStats_Delineation <- function(# accepts multiple
  state, # StreamStats state info e.g. 'VA'
  longitude, # longitude value, numeric
  latitude, # latitude value, numeric
  UID # Unique station identifier to append to dataset
){ # function based off code by Ryan King https://ryan-hill.github.io/sfs-r-gis-2018/modules/rasters/extract-raster-data-r/ exercise 3
  
  outDataAll <- list(polygon = list(), point = list()) # holder in case server bomb out
  #outDataAllFinal <- list(polygon = list(), point = list())
  
  
  for(i in 1:length(longitude)){
    print(paste('Delineating Site:',i, 'of', length(longitude)))
    
    dat <- streamStats_Delineation_single(state= 'VA', 
                                          longitude = longitude[i], 
                                          latitude = latitude[i], 
                                          UID = UID[i])
    
    #catch in case streamstats bombs out
    #if(is.na(dat[['polygon']]$UID) | is.na(dat[['point']]$UID) ){dat <- list(polygon = data.frame(UID = NA), point = data.frame(UID = NA))}
    outDataAll$polygon[[i]] <- dat[['polygon']] 
    outDataAll$point[[i]] <- dat[['point']] 
  }
  
  return(outDataAll)
}


#x2018_sf <- streamStats_Delineation(state= 'VA', 
#                                    longitude = x2018$LONG_DD, 
#                                    latitude = x2018$LAT_DD, 
#                                    UID = x2018$DEQSITEID)
