

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
#' Delineate a single watershed using the new SS-Delineate API (feature-only).
#' @param region Two-letter StreamStats region/state code, e.g., "VA"
#' @param longitude Numeric longitude (WGS84)
#' @param latitude  Numeric latitude (WGS84)
#' @param UID       Unique station identifier to append to outputs
#' @return list with sf objects: $polygon (unioned basin) and $point (pourpoint)
streamStats_Delineation_single_new <- function(region, longitude, latitude, UID) {
  # deps: jsonlite, geojsonsf, dplyr, sf
  outData <- list(polygon = list(), point = list())
  
  base <- "https://streamstats.usgs.gov/ss-delineate/v1/delineate/features/"
  url  <- paste0(base, region,
                 "?lon=", as.character(longitude),
                 "&lat=", as.character(latitude))  # lon/lat per SS-Delineate docs
  
  # Fetch FeatureCollection (GeoJSON) safely
  mydata <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE, simplifyDataFrame = FALSE),
    error = function(e) { message(sprintf("StreamStats Error: %s", e)); return(NULL) },
    warning = function(w) { message(sprintf("StreamStats Warning: %s", w)); return(NULL) }
  )
  
  if (!is.null(mydata)) {
    # Convert entire FeatureCollection to sf
    fc_json <- jsonlite::toJSON(mydata, auto_unbox = TRUE)
    fc_sf   <- geojsonsf::geojson_sf(fc_json)  # should carry EPSG:4326
    
    # Extract pourpoint
    point_sf <- dplyr::filter(fc_sf, .data$scope == "pourpoint")
    if (nrow(point_sf) > 0) {
      outData$point <- point_sf |>
        dplyr::mutate(UID = UID) |>
        dplyr::select(UID, geometry)
    } else {
      outData$point <- sf::st_sf(UID = NA, geometry = sf::st_sfc(sf::st_point(), crs = 4326))
    }
    
    # Union polygon pieces (split/adjoint/upstream) to mimic old single polygon
    poly_sf <- dplyr::filter(fc_sf, .data$scope %in% c("split_catchment", "adjoint_catchment", "upstream_basin"))
    if (nrow(poly_sf) > 0) {
      # Make valid before union to avoid topology errors
      poly_valid <- sf::st_make_valid(poly_sf)
      poly_union <- sf::st_union(poly_valid)
      outData$polygon <- sf::st_sf(
        UID = UID,
        geometry = sf::st_sfc(poly_union, crs = sf::st_crs(poly_sf))
      )
    } else {
      outData$polygon <- sf::st_sf(UID = NA, geometry = sf::st_sfc(sf::st_polygon(), crs = 4326))
    }
  } else {
    # Fallback when the service fails
    outData$polygon <- sf::st_sf(UID = NA, geometry = sf::st_sfc(sf::st_polygon(), crs = 4326))
    outData$point   <- sf::st_sf(UID = NA, geometry = sf::st_sfc(sf::st_point(),   crs = 4326))
  }
  
  return(outData)
}

# example use
# res <- streamStats_Delineation_single_new(
#   region    = "VA",
#   longitude = -80.05,
#   latitude  = 37.29,
#   UID       = "VA00123"
# )
# 
# 
# # Inspect results
# res$polygon  # sf polygon (unioned basin)
# res$point    # sf point (pourpoint)



# Multiple Watershed delineation
streamStats_Delineation <- function(# accepts multiple
  region, # StreamStats state info e.g. 'VA'
  longitude, # longitude value, numeric
  latitude, # latitude value, numeric
  UID # Unique station identifier to append to dataset
){ # function based off code by Ryan King https://ryan-hill.github.io/sfs-r-gis-2018/modules/rasters/extract-raster-data-r/ exercise 3
  
  outDataAll <- list(polygon = list(), point = list()) # holder in case server bomb out
  #outDataAllFinal <- list(polygon = list(), point = list())
  
  
  for(i in 1:length(longitude)){
    print(paste('Delineating Site:',i, 'of', length(longitude)))
    
    dat <- streamStats_Delineation_single_new(region= region, 
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
