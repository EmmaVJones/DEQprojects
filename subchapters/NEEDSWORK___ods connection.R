library(tidyverse)
library(pool)
library(dbplyr)


## For testing: connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

masterTaxaGenus <- pool %>% tbl(in_schema("wqm",  "Edas_Benthic_Master_Taxa_View")) %>%
  as_tibble()



library(odbc)
library(DBI)
con <- dbConnect(odbc::odbc(), .connection_string = "driver={ODBC Driver 11 for SQL Server};server={DEQ-SQLODS-PROD,50000};database={ODS};trusted_connection=yes")

test <- dbGetQuery(con,  "SELECT * FROM wqm.Edas_Benthic_Master_Taxa_View")
test <- dbGetQuery(con,  "SELECT TOP 10 * FROM wqm.WQM_Stations_View")

## Pull one station- this brings everything back based on these parameters and futher refining is allowed in the app
station <- '2-JKS013.29'#'2-ROT003.15'#'4AGSF002.16'#'2-JKS013.29'#'2-JKS023.61'#'4AROA216.75'#'4AROA175.63'#'1BDUR000.11'#'2-JKS023.61'#'2-SKC001.17'#'2-JKS023.61'#'4AROA175.63'##'4ASRE043.54'#'2-JKS028.69'#'4AROA202.20'#'4ATKR000.08'#'4ADEE000.06'##'4ATKR003.03'#'2-JKS023.61'#'4ADEE000.06'##'2-JKS018.68'#'1BNFS011.81'#'2-PWT003.98'#'2-JKS023.61'#'2-JKS067.00'#'2-JKS023.61'#'1AOCC002.47'##'2-JKS006.67'#'2-JKS023.61'#'4AROA217.38'# not in WQM_full on REST service '2-JKS023.61'#
dateRange <- c(as.Date('1970-01-01'), as.Date('2021-10-26'))# as.Date(Sys.Date())) #as.Date('1985-01-01'))#

stationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
  filter(Fdt_Sta_Id %in% !! station &
           between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>% # & # x >= left & x <= right
  #Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>%  # don't drop QA failure on SQL part bc also drops any is.na(Ssc_Description)
  as_tibble() %>% 
  filter(! Ssc_Description %in% "INVALID DATA SET QUALITY ASSURANCE FAILURE")
