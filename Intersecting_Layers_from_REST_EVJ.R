library(sf)
library(leaflet)
library(geojsonsf)
library(dplyr)
library(urltools)
library(rgdal)
library(httr)

#commented example of ugly but simple full query url below.
#tmdl <- geojson_sf("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/72/query?where=TMDL_EQ_ID+%3D+%27POL0119%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")

#more elegant query / string crafting
url <- parse_url("https://gis.deq.virginia.gov/arcgis/rest/services")
url$path <- paste(url$path, "staff/DEQInternalDataViewer/MapServer/72/query", sep = "/")
url$query <- list(where = "TMDL_EQ_ID = 'POL0119'",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)

tmdl <- st_read(request)

bboxPoly <- st_bbox(tmdl) %>%
  st_as_sfc()


#baseURL for  the WQM Stations layer
baseURL <- 'https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?'
#bounding box of the above selected TMDL Watrshed
bbox <- st_bbox(tmdl)
#convert bounding box to character
bbox <- toString(bbox)
#encode for use within URL
bbox <- urltools::url_encode(bbox)
#EPSG code for coordinate reference system used by the TMDL polygon sf object
epsg <- st_crs(tmdl)$epsg

#set parameters for query
query <- urltools::param_set(baseURL,key="geometry", value=bbox) %>%
  param_set(key="inSR", value=epsg) %>%
  param_set(key="resultRecordCount", value=500) %>%
  param_set(key="f", value="geojson") %>%
  param_set(key="outFields", value="*")

stations <- readOGR(dsn = query)
wqmstations_inbox <- geojson_sf(query)
#wqmstations_sf <- st_as_sf(wqmstations_inbox)
#wqmstations_sf
stations


wqmstations_inbox2 <- wqmstations_inbox %>%
                                  dplyr::mutate(Longitude = sf::st_coordinates(.)[,1],
                                                Latitude = sf::st_coordinates(.)[,2]) %>% 
  dplyr::select(STATION_ID, Latitude, Longitude)



#spatially join stations and tmdl...ive tried using stations here as well...no avail
#insidewatershed <- st_join(wqmstations_inbox,tmdl)
#remove na values (i.e. values where there is no spatial overlap)
#insidewatershed <- na.omit(insidewatershed)

# EVJ method
insidewatershed <- st_intersection(wqmstations_inbox,tmdl) %>%
  dplyr::mutate(Longitude = sf::st_coordinates(.)[,1],
                Latitude = sf::st_coordinates(.)[,2])

leaflet() %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addPolygons(data=tmdl, weight=2, color="blue") %>%
  addPolygons(data=bboxPoly, weight=2, fill=FALSE, color="blue") %>%
  addCircleMarkers(data = wqmstations_inbox2, lng = ~Longitude, lat = ~Latitude, 
             label = ~as.character(STATION_ID)) %>% 
  addCircleMarkers(data = insidewatershed, lng = ~Longitude, lat = ~Latitude, 
                   color = 'red',
                   label = ~as.character(STATION_ID) )
  


