# Code for checks performed in the limnoEntry tool
# Created by Kaija Gahm on 23 October 2020
# Functions in here are called by limnoEntry.R
options(warning.length = 3000L, error.length = 3000L)

# repeatSampleIDsCheck ----------------------------------------------------
repeatSampleIDsCheck <- function(tc = toCompile, db = samplesDB, is = samplesIS){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(db)
  assertDataFrame(is)
  assertSubset(c("entryFile", "sampleID"), names(is))
  assertChoice("sampleID", names(db))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc)
  old <- is %>%
    filter(!entryFile %in% tc)
  
  # Get just the duplicates and the files they come from
  problemRows <- new %>%
    filter(sampleID %in% c(db$sampleID, old$sampleID)) %>%
    select(sampleID, entryFile) %>%
    distinct()
  
  # If there are duplicates, throw error and print the duplicates
  if(nrow(problemRows) > 0){
    stop(paste0("You are attempting to add sampleIDs to the in-season database that already exist in the database or in-season database. Here are the sampleID's, and the files they come from: \n\n",
                paste0(capture.output(problemRows), collapse = "\n")
    )
    )
  }
}

# newSiteIDsCheck ---------------------------------------------------------
newSiteIDsCheck <- function(tc = toCompile, db = samplesDB, is = samplesIS, 
                            f = force_siteID){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(db)
  assertDataFrame(is)
  assertLogical(f, len = 1)
  assertSubset(c("entryFile", "siteID"), names(is))
  assertChoice("siteID", names(db))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc)
  old <- is %>%
    filter(!entryFile %in% tc)
  
  # Get just the new siteID's and the files they come from
  problemRows <- new %>%
    filter(!siteID %in% c(db$siteID, old$siteID)) %>%
    select(siteID, entryFile) %>%
    distinct()
  
  # If there are new siteIDs, throw error and print the new siteIDs
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("You are attempting to add siteIDs to the in-season database that do not exist in either the MFE databse or the in-season database. Here are the siteID's, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"), 
                  "\n\n If you are sure these siteID's are valid, use the force_siteID argument."))
    }
  }
}

# newLakeIDsCheck ---------------------------------------------------------
newLakeIDsCheck <- function(tc = toCompile, db = samplesDB, is = samplesIS, 
                            f = force_lakeID){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(db)
  assertDataFrame(is)
  assertLogical(f, len = 1)
  assertChoice("entryFile", names(is))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc) %>%
    {if(!"lakeID" %in% names(.)) mutate(., lakeID = stringr::word(sampleID, 1, 1, sep = "_")) else .}
  old <- is %>%
    filter(!entryFile %in% tc) %>%
    {if(!"lakeID" %in% names(.)) mutate(., lakeID = stringr::word(sampleID, 1, 1, sep = "_")) else .}
  db <- db %>%
    {if(!"lakeID" %in% names(.)) mutate(., lakeID = stringr::word(sampleID, 1, 1, sep = "_")) else .}
  
  # Get just the new lakeID's and the files they come from
  problemRows <- new %>%
    filter(!lakeID %in% 
             c(db$lakeID, old$lakeID)) %>%
    select(lakeID, entryFile) %>%
    distinct()
  
  # If there are new lakeIDs, throw error and print the new lakeIDs
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("You are attempting to add lakeID's to the in-season database that do not exist in either the MFE databse or the in-season database. Here are the lakeID's, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n If you are sure these lakeID's are valid, use the force_lakeID argument."))
    }
  }
}

# newProjectIDsCheck ---------------------------------------------------------
newProjectIDsCheck <- function(tc = toCompile, db = samplesDB, is = samplesIS, 
                               f = force_newProjectID){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(db)
  assertDataFrame(is)
  assertLogical(f, len = 1)
  assertSubset(c("entryFile", "projectID"), names(is))
  assertChoice("projectID", names(db))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc)
  old <- is %>%
    filter(!entryFile %in% tc)
  
  # Get just the new projectID's and the files they come from
  problemRows <- new %>%
    filter(!as.character(projectID) %in% c(as.character(db$projectID), as.character(old$projectID))) %>%
    select(projectID, entryFile) %>%
    distinct()
  
  # If there are new projectIDs, throw error and print the new projectIDs
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("You are attempting to add projectID's to the in-season database that do not exist in either the database SAMPLES or the in-season database. Here are the projectID's, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n If you are sure these projectID's are valid, use the force_newProjectID argument."))
    }
  }
}

# retiredProjectIDsCheck --------------------------------------------------
retiredProjectIDsCheck <- function(tc = toCompile, is = samplesIS, 
                                   f = force_retiredProjectID){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(is)
  assertLogical(f, len = 1)
  assertSubset(c("entryFile", "projectID"), names(is))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc)
  
  # Check whether any of the newly-added projectIDs should have been retired
  problemRows <- new %>%
    filter(projectID %in% retiredProjectIDs) %>%
    select(projectID, entryFile) %>%
    distinct()
  
  # If there are retired projectID's, throw error and print the retired projectIDs
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("Some of the projectID's you're trying to enter are from old projects and should not be associated with incoming data. Here are the projectID's, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n If you are sure that you want to use these projectID's, use force_retiredProjectIDs."))
    }
  }
}

# sampleTimesCheck --------------------------------------------------
sampleTimesCheck <- function(tc = toCompile, is = samplesIS, 
                             f = force_timeSample){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(is)
  assertLogical(f, len = 1)
  assertSubset(c("entryFile", "sampleID"), names(is))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc)
  
  # Check whether any of the newly-added projectIDs should have been retired
  problemRows <- new %>%
    mutate(time = word(sampleID, 4, 4, sep = "_"),
           time = strptime(time, format = "%H%M")) %>%
    filter(time < strptime("0700", format = "%H%M")|
             time > strptime("1800", format = "%H%M")) %>%
    select(time, entryFile) %>%
    distinct()
  
  # If there are early/late sample times, throw error and print the suspect times
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("Some of the sample times you're trying to enter seem unusually early or late. Here are the sample times, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n If you are sure that these sample times are correct, use force_timeSample"))
    }
  }
}

# newMetadataIDsCheck ---------------------------------------------------------
newMetadataIDsCheck <- function(tc = toCompile, db = samplesDB, is = samplesIS, 
                                f = force_metadataID){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(db)
  assertDataFrame(is)
  assertLogical(f, len = 1)
  assertSubset(c("entryFile", "metadataID"), names(is))
  assertChoice("metadataID", names(db))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc)
  old <- is %>%
    filter(!entryFile %in% tc)
  
  # Get just the new metadataID's and the files they come from
  problemRows <- new %>%
    filter(!metadataID %in% c(db$metadataID, old$metadataID)) %>%
    select(metadataID, entryFile) %>%
    distinct()
  
  # If there are new projectIDs, throw error and print the new projectIDs
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("You are attempting to add metadataID's to the in-season database that do not exist in either the MFE databse or the in-season database. Here are the metadataID's, and the files they come from: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"), 
                  "\n\n If you are sure these metadataID's are valid, use the force_metadataID argument."))
    }
  }
}

# depthsCheck -------------------------------------------------------------
depthsCheck <- function(tc = toCompile, db = profilesDB, is = profilesIS, f = force_depth){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(db)
  assertDataFrame(is)
  assertLogical(f, len = 1)
  assertSubset(c("entryFile", "lakeID", "depthBottom"), names(is))
  assertSubset(c("lakeID", "depthBottom"), names(db))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc)
  old <- is %>%
    filter(!entryFile %in% tc)
  
  # Compute previous max depths by combining the `old` portion of profilesIS with profilesDB
  previousMaxDepths <- old %>%
    select(lakeID, depthBottom) %>%
    mutate(depthBottom = as.numeric(depthBottom)) %>%
    bind_rows(db %>% 
                select(lakeID, depthBottom)) %>%
    distinct() %>%
    group_by(lakeID) %>%
    summarize(previousMaxDepth = max(depthBottom))
  
  # Now compute new max depths
  newMaxDepths <- new %>%
    select(lakeID, depthBottom) %>%
    distinct() %>%
    mutate(depthBottom = as.numeric(depthBottom)) %>%
    group_by(lakeID) %>%
    summarize(newMaxDepth = max(depthBottom))
  
  # Join the two together
  allMaxDepths <- left_join(previousMaxDepths, newMaxDepths, by = "lakeID")
  
  # Filter it down to any where the new one is deeper than the old one
  problemRows <- allMaxDepths %>%
    filter(newMaxDepth > previousMaxDepth)
  
  # If there are problem rows, print them
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      # Join the entryFile info
      files <- new %>%
        select(lakeID, depthBottom, entryFile) %>%
        rename("newMaxDepth" = "depthBottom") %>%
        mutate(newMaxDepth = as.numeric(newMaxDepth)) %>%
        distinct()
      
      problemRows <- problemRows %>%
        left_join(files, by = c("lakeID", "newMaxDepth")) %>%
        as.data.frame()
      
      stop(paste0("You are trying to add some depths that are deeper than we have ever collected for the corresponding lakes. The problematic lakes and depths are here: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n Please double-check your depths on the data entry sheet. If you are sure these depths are valid, use the force_depth argument."))
    }
  }
}

# inletOutletCheck --------------------------------------------------------
inletOutletCheck <- function(tc = toCompile, db = samplesDB, is = samplesIS, 
                             fi = force_inlet, fo = force_outlet){
  assertCharacter(tc, pattern = "\\.csv")
  assertDataFrame(db)
  assertDataFrame(is)
  assertLogical(fi, len = 1)
  assertLogical(fo, len = 1)
  assertSubset(c("entryFile", "siteID"), names(is))
  assertChoice("siteID", names(db))
  
  # Separate just-added data from previous data
  new <- is %>%
    filter(entryFile %in% tc)
  old <- is %>%
    filter(!entryFile %in% tc)
  
  # Get a list of all previously-sampled inlets and outlets
  previousInletsOutlets <- old %>%
    select(siteID) %>%
    filter(grepl("Inlet|Outlet", siteID)) %>%
    bind_rows(db %>% 
                select(siteID) %>%
                filter(grepl("Inlet|Outlet", siteID))) %>%
    distinct()
  
  # Get a list of all newly-added inlets and outlets
  newInletsOutlets <- new %>%
    select(siteID) %>%
    filter(grepl("Inlet|Outlet", siteID)) %>%
    distinct()
  
  # Find any newly-added inlets or outlets that haven't been previously sampled
  problemRowsInlets <- newInletsOutlets %>%
    filter(grepl("Inlet", siteID)) %>%
    filter(!siteID %in% previousInletsOutlets$siteID)
  problemRowsOutlets <- newInletsOutlets %>%
    filter(grepl("Outlet", siteID)) %>%
    filter(!siteID %in% previousInletsOutlets$siteID)
  
  # Get entry file info for all sites
  files <- new %>%
    select(siteID, entryFile) %>%
    distinct()
  
  # If there are problem rows, throw an error and print info
  if(nrow(problemRowsInlets) > 0){
    if(fi == F){
      # Join the entryFile info
      problemRowsInlets <- problemRowsInlets %>%
        left_join(files, by = "siteID") %>%
        as.data.frame()
      
      stop(paste0("You indicated that you sampled some inlets that we have never sampled before. The problematic inlets are here: \n\n",
                  paste0(capture.output(problemRowsInlets), collapse = "\n"),
                  "\n\n If you are sure that you sampled these inlets, use the force_inlet argument."))
    }
  }
  
  if(nrow(problemRowsOutlets) > 0){
    if(fo == F){
      # Join the entryFile info
      problemRowsOutlets <- problemRowsOutlets %>%
        left_join(files, by = "siteID") %>%
        as.data.frame()
      
      stop(paste0("You indicated that you sampled some outlets that we have never sampled before. The problematic outlets are here: \n\n",
                  paste0(capture.output(problemRowsOutlets), collapse = "\n"),
                  "\n\n If you are sure that you sampled these outlets, use the force_outlet argument."))
    }
  }
}

# filterVolumesCheck ------------------------------------------------------
filterVolumesCheck <- function(vl = volumesList, f = force_volumeFiltered){
  assertList(vl, names = "named", any.missing = F, null.ok = F)
  assertLogical(f, len = 1)
  
  # Get a full data frame of non-zero volumes from the `volumesList` object
  vols <- vl %>%
    map2(.x = ., 
         .y = names(.), # the names of the list are the entry file filenames
         .f = function(.x, .y){
           .x %>% 
             mutate(parameter = row.names(.)) %>%
             # pivot to long format
             pivot_longer(cols = -parameter, 
                          names_to = "siteOrDepthClass",
                          values_to = "volFiltered") %>%
             filter(volFiltered > 0) %>%
             filter(volFiltered != "",
                    !is.na(volFiltered)) %>%
             # add the entry file
             mutate(entryFile = .y)
         }) %>%
    data.table::rbindlist() %>%
    as.data.frame()
  
  # Now identify any that are outside the normal range (< 60 or > 500, because we've already removed any that were zero.)
  problemRows <- vols %>%
    filter(volFiltered < 60|volFiltered > 500)
  
  # If there are any problems, throw an error and print the rows.
  if(nrow(problemRows) > 0){
    if(f == FALSE){
      stop(paste0("You have indicated filter volumes outside the normal range (i.e. volumes < 60 or > 500). Here are the problematic volumes: \n\n",
                  paste0(capture.output(problemRows), collapse = "\n"),
                  "\n\n If you are sure that these volumes are correct, use the force_volumeFiltered argument."))
    }
  }
}

# profileRangeCheck -------------------------------------------------------
# Define range parameters to be used in the range check
tempMin <- 0
tempMax <- 35
DOmgLMin <- 0
DOmgLMax <- 15
DOsatMin <- -1
DOsatMax <- 150
SpCMin <- 0
SpCMax <- 500
pHMin <- 3
pHMax <- 15
ORPMin <- -300
ORPMax <- 300
PARMin <- -20
PARMax <- 2500
assertNumeric(tempMin, len = 1)
assertNumeric(tempMax, len = 1)
assertNumeric(DOmgLMin, len = 1)
assertNumeric(DOmgLMax, len = 1)
assertNumeric(DOsatMin, len = 1)
assertNumeric(DOsatMax, len = 1)
assertNumeric(SpCMin, len = 1)
assertNumeric(SpCMax, len = 1)
assertNumeric(pHMin, len = 1)
assertNumeric(pHMax, len = 1)
assertNumeric(ORPMin, len = 1)
assertNumeric(ORPMax, len = 1)
assertNumeric(PARMin, len = 1)
assertNumeric(PARMax, len = 1)

profileRangeCheck <- function(p = profDataList, ft = force_profileTemp, 
                              fdm = force_profileDOmgL, fds = force_profileDOsat,
                              fs = force_profileSpC, fph = force_profilePH,
                              fo = force_profileORP, fpa = force_profilePAR){
  assertList(p, any.missing = F, null.ok = F, names = "named")
  
  # Make profDataList into a data frame
  pdf <- map2(.x = p,
              .y = names(p),
              .f = function(.x, .y){
                .x %>%
                  mutate(entryFile = .y)
              }) %>%
    data.table::rbindlist() %>% 
    as.data.frame()
  assertSubset(c("depth", "temp", "DOmgL", "DOsat", "SpC", "pH", 
                 "ORP", "PAR", "entryFile"), names(pdf))
  
  # Check temps
  problemRowsTemp <- pdf %>%
    select(depth, temp, entryFile) %>%
    filter(temp < tempMin | temp > tempMax)
  if(nrow(problemRowsTemp) > 0){
    if(ft == FALSE){
      stop(paste0("You have indicated profile temp values outside the expected range: \n\n",
                  paste0(capture.output(problemRowsTemp), collapse = "\n"),
                  "\n\n If you are sure that these rows are correct, use force_profileTemp."))
    }
  }
  
  # Check DOmgL
  problemRowsDOmgL <- pdf %>%
    select(depth, DOmgL, entryFile) %>%
    filter(DOmgL < DOmgLMin | DOmgL > DOmgLMax)
  if(nrow(problemRowsDOmgL) > 0){
    if(fdm == FALSE){
      stop(paste0("You have indicated profile DOmgL values outside the expected range: \n\n",
                  paste0(capture.output(problemRowsDOmgL), collapse = "\n"),
                  "\n\n If you are sure that these values are correct, use force_profileDOmgL."))
    }
  }
  
  # Check DOsat
  problemRowsDOsat <- pdf %>%
    select(depth, DOsat, entryFile) %>%
    filter(DOsat < DOsatMin | DOsat > DOsatMax)
  if(nrow(problemRowsDOsat) > 0){
    if(fds == FALSE){
      stop(paste0("You have indicated profile DOsat values outside the expected range: \n\n",
                  paste0(capture.output(problemRowsDOsat), collapse = "\n"),
                  "\n\n If you are sure that these values are correct, use force_profileDOsat."))
    }
  }
  
  # Check SpC
  problemRowsSpC <- pdf %>%
    select(depth, SpC, entryFile) %>%
    filter(SpC < SpCMin | SpC > SpCMax)
  if(nrow(problemRowsSpC) > 0){
    if(fs == FALSE){
      stop(paste0("You have indicated profile SpC values outside the expected range: \n\n",
                  paste0(capture.output(problemRowsSpC), collapse = "\n"),
                  "\n\n If you are sure that these values are correct, use force_profileSpC."))
    }
  }
  
  # Check pH
  problemRowsPH <- pdf %>%
    select(depth, pH, entryFile) %>%
    filter(pH < pHMin, pH > pHMax)
  if(nrow(problemRowsPH) > 0){
    if(fph == FALSE){
      stop(paste0("You have indicated profile pH values outside the expected range: \n\n",
                  paste0(capture.output(problemRowsPH), collapse = "\n"),
                  "\n\n If you are sure that these values are correct, use force_profilePH."))
    }
  }
  
  # Check ORP
  problemRowsORP <- pdf %>%
    select(depth, ORP, entryFile) %>%
    filter(ORP < ORPMin | ORP > ORPMax)
  if(nrow(problemRowsORP) > 0){
    if(fo == FALSE){
      stop(paste0("You have indicated profile ORP values outside the expected range: \n\n",
                  paste0(capture.output(problemRowsORP), collapse = "\n"),
                  "\n\n If you are sure that these values are correct, use force_profileORP."))
    }
  }
  
  # Check PAR
  problemRowsPAR <- pdf %>%
    select(depth, PAR, entryFile) %>%
    filter(PAR < PARMin, PAR > PARMax)
  if(nrow(problemRowsPAR) > 0){
    if(fpa == FALSE){
      stop(paste0("You have indicated profile PAR values outside the expected range: \n\n",
                  paste0(capture.output(problemRowsPAR), collapse = "\n"),
                  "\n\n If you are sure that these values are correct, use force_profilePAR."))
    }
  }
}

expectedProfData <- c("depth", "temp", "DOmgL", "DOsat", "SpC", "pH", "ORP", "PAR", "PML", "hypo", "point")
expectedGauges <- c("height", "unit")
expectedMoieties <- c("PML", "hypo", "Outlet", "Inlet1", "Inlet2", "Inlet3", "point")
expectedVolumes <- c("PML", "hypo", "Outlet", "Inlet1", "Inlet2", "Inlet3", "point")
expectedZoopIS <- c("zoopID", "projectID", "lakeID", "site", "dateSample", "timeSample", 
                    "depthClass", "depthTop", "depthBottom", "comments", "sampleID")

# Check the dateSampleString ----------------------------------------------
checkDateSampleString <- function(x){
  if(!grepl("^[0-9]{8}$", x)){
    stop("dateSampleString must be a string of 8 numbers")
  }
}

# Check header$dateSample -------------------------------------------------
checkDateSample <- function(x){
  if(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)){
    stop("header$dateSample failed to parse properly. Must have the format YYYY-MM-DD.")
  }
}

# Check header$timeSample -------------------------------------------------
checkTimeSample <- function(x){
  if(!grepl("^[0-9]{2}:[0-9]{2}$", x)){
    stop("header$timeSample failed to parse properly. Must have the format hh:mm.")
  }
}

# Check header$dateTimeSample ---------------------------------------------
checkDateTimeSample <- function(x){
  if(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}\\s[0-9]{2}:[0-9]{2}:[0-9]{2}$", x))
    stop("header$dateTimeSample failed to parse properly. Must have the format YYYY-MM-DD hh:mm:ss")
}

# Check header function -------------------------------------------------------
# Function to check the header of the current input file for completeness
checkHeader <- function(h = header){
  # if weather is missing or NA, warn
  w <- h[["weather"]]
  if(w == ""|is.na(w)){
    warning("Weather information is missing or NA. Did you forget to include weather for this sample sheet?")
  }
  
  # if any besides weather and comments are missing or NA, error
  if(any(h[names(h)[!names(h) %in% c("weather", "comments", "zoopDepth")]] == "")|
     any(is.na(h[names(h)[!names(h) %in% c("weather", "comments", "zoopDepth")]]))){
    stop(paste0("Required header information is incomplete in ", file, 
                ". You're missing: ", 
                paste(names(h[h == ""|is.na(h)]), 
                      collapse = ", ")))
  }
  if(substr(h$dateSample, 1, 2) != "20"){
    stop(paste0("The date on sample sheet ", file, 
                " does not begin with '20'. Please correct the format of your date."))
  }
  if(is.na(as.integer(h$projectID))){
    stop(paste0("ProjectID for sample sheet ", file, 
                " is not a valid integer. Please correct the projectID."))
  }
}

# checkCurFormat ----------------------------------------------------------
# Currently, this function just checks rows and columns. I'm not sure what else I should have it check. Most of the other pieces get verified at other points along the pipeline...
checkCurMisspellings <- function(x){
  # Check for any commonly-misspelled words, and remind the user to use the updated sample sheet template
  n <- which(x == "Hypo"|x == "moeties"|x == "Dosat")
  
  if(length(n) > 0){
    message("Detected at least one instance of 'Hypo', 'moeties', or 'Dosat'. These will be automatically corrected, but make sure you're using the updated sample sheet template going forward.")
  }
}

# checkMoieties -----------------------------------------------------------
# Checks that the DOC and chloro rows have 0's or 2's, not 1's or any other number. If they find another number, throw error and allow a force.
checkMoieties <- function(m, fd = force_DOCReplicates, fc = force_chloroReplicates){
  # Check inputs
  assertDataFrame(m)
  assertSubset(c("DOC", "chloro"), choices = row.names(m))
  
  # DOC
  if(any(!as.numeric(as.vector(m["DOC",])) %in% c(0, 2))){
    if(fd == FALSE){
      stop(paste0("Found values other than 0 or 2 in the DOC row of moieties for the following columns:\n\n",
                  paste(names(m)[which(!(m["DOC",] 
                                         %in% c(0, 2)))], collapse = ", "),
                  "\n\nIf you're sure you took a different number of replicates, use ",
                  deparse(substitute(fd)), "."))
    }
  }
  
  # Chlorophyll
  if(any(!as.numeric(as.vector(m["chloro",])) %in% c(0, 2))){
    if(fc == FALSE){
      stop(paste0("Found values other than 0 or 2 in the chloro row of moieties for the following columns:\n\n",
                  paste(names(m)[which(!(m["chloro",] 
                                         %in% c(0, 2)))], collapse = ", "),
                  "\n\nIf you're sure you took a different number of replicates, use ",
                  deparse(substitute(fc)), "."))
    }
  }
}