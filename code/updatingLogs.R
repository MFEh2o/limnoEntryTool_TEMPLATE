# Script to update in-season database files from limno datasheets
# Last updated by Kaija Gahm, May 2021
# Make sure to read the README document in the limnoEntryTool_TEMPLATE repository for full setup instructions, and instructions on how to run this tool.

# Packages and setup ------------------------------------------------------
library(here)
options(warning.length = 3000L, error.length = 3000L) # so full errors/warnings will print
# Load the updateLimno function from the limnoEntry.R script 
source(here("code", "limnoEntry.R"))

# Set variables -----------------------------------------------------------
dbdir <- here() # database is in the root project directory
db <- "CHANGE THIS" # name of the database file you're using. Try to use one with a specific date to make the workflow clear. For example, "MFEdb_20200530.db"
funcdir <- here("code") # folder where the functions are stored
sampleSheetsDir <- here("sampleSheets")
logFilesDir <- here("logFiles")

# Run the tool ------------------------------------------------------------
updateLimno(dbdir = dbdir, 
            db = db, 
            sampleSheetsDir = sampleSheetsDir, 
            logFilesDir = logFilesDir, 
            funcdir = funcdir)
