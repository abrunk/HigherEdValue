## For my first model, let's see if I can just download
## the data set and build a super-simple model with
## a couple of inputs to see if you can predict
## graduating salary data using entering SAT or GPA

library(tidyverse)
library(janitor)
library(RODBC)

conn1 <- odbcConnectAccess("IPEDS202021.accdb")

## Import RODBC package
library(RODBC)

## Set up driver info and database path
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "C:\Users\alexb\Documents\Github Projects\HigherEdValue\IPEDS202021.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

## Establish connection
channel <- odbcDriverConnect(PATH)

## Load data into R dataframe
df <- sqlQuery(channel,
               "SELECT [student_id], [first_name], [last_name],
FROM [tbl-students]
ORDER BY [first_name];",
               stringsAsFactors = FALSE)

## Close and remove channel
close(channel)
rm(channel)