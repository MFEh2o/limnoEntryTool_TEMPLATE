# script to update in-season database files from limno datasheets
# 2019-04-16
# BLB
# Edited by Kaija Gahm, October 2020

# Libraries
library(here)

# Load the updateLimno function from the limnoEntry.R script 
source(here("code", "limnoEntry.R"))

# Set variables
dbdir <- here() # database is in the root project directory
db <- "MFEdb_20200530.db" # name of the database file
funcdir <- here("code") # folder where the functions are stored
sampleSheetsDir <- here("sampleSheets2020")
logFilesDir <- here("logFiles2020")
options(warning.length = 3000L, error.length = 3000L)

# Update 
updateLimno(dbdir = dbdir, db = db, sampleSheetsDir = sampleSheetsDir, 
            logFilesDir = logFilesDir, funcdir = funcdir, force_siteID = T, 
            force_lakeID = T, force_newProjectID = T, force_retiredProjectID = T, 
            force_depth = T, force_profileORP = T)

# Steps for updating the Github of the in-season database
# Go to the Terminal tab in the console below
# Execute the following commands:

## bash
## git add -A
## git commit -m "added samples from 2019-MM-DD"
## git push origin master

