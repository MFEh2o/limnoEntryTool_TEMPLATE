# Function for generating in-season database files from limno datasheets
# 2020-10-15
# KG

options(warning.length = 3000L, error.length = 3000L)

# load libraries ----------------------------------------------------------
library(here)
library(openxlsx)
library(tidyverse)
library(checkmate)
source(here("code", "supportingFuns.R"))
source(here("code", "checks.R"))

# set time zone -----------------------------------------------------------
Sys.setenv(tz = "America/Chicago")

# updateLimno function ----------------------------------------------------
updateLimno <- function(dbdir, db, funcdir, logFilesDir, sampleSheetsDir,
                        force_lakeID = F, force_siteID = F, 
                        force_metadataID = F, force_newProjectID = F,
                        force_retiredProjectID = F,
                        force_timeSample = F, force_depth = F, 
                        force_outlet = F, force_inlet = F,
                        force_volumeFiltered = F, force_profileTemp = F, 
                        force_profileDOmgL = F, force_profileDOsat = F, 
                        force_profileSpC = F, force_profilePH = F,
                        force_profileORP = F, force_profilePAR = F){
  
  # Check the input values
  assertCharacter(dbdir, len = 1)
  assertCharacter(db, pattern = ".*\\.db", len = 1)
  assertCharacter(funcdir, len = 1)
  assertCharacter(logFilesDir, len = 1)
  assertCharacter(sampleSheetsDir, len = 1)
  assertLogical(force_lakeID, len = 1)
  assertLogical(force_siteID, len = 1)
  assertLogical(force_metadataID, len = 1)
  assertLogical(force_newProjectID, len = 1)
  assertLogical(force_retiredProjectID, len = 1)
  assertLogical(force_timeSample, len = 1)
  assertLogical(force_depth, len = 1)
  assertLogical(force_outlet, len = 1)
  assertLogical(force_inlet, len = 1)
  assertLogical(force_volumeFiltered, len = 1)
  assertLogical(force_profileTemp, len = 1)
  assertLogical(force_profileDOmgL, len = 1)
  assertLogical(force_profileDOsat, len = 1)
  assertLogical(force_profileSpC, len = 1)
  assertLogical(force_profilePH, len = 1)
  assertLogical(force_profileORP, len = 1)
  assertLogical(force_profilePAR, len = 1)
  assertNumeric(retiredProjectIDs, any.missing = F, unique = T, lower = 1)
  
  source(file.path(funcdir, "dbUtil.R")) # load the dbUtil functions
  
  # load tables from database
  message("Loading database tables...")
  lakesDB <- suppressWarnings(dbTable("LAKES"))
  profilesDB <- suppressWarnings(dbTable("LIMNO_PROFILES"))
  samplesDB <- suppressWarnings(dbTable("SAMPLES"))
  assertDataFrame(lakesDB)
  assertDataFrame(profilesDB)
  assertDataFrame(samplesDB)
  
  # Check for required input files -----------------------------------------
  logFiles <- list.files(logFilesDir)
  assertAtomic(logFiles, min.len = 1)
  
  ## IN-SEASON DB FILES: if they don't exist, create them
  if("samplesIS.csv" %in% logFiles){
    samplesIS <- customReadCSV(file.path(logFilesDir, "samplesIS.csv"))
    assertDataFrame(samplesIS, 
                    col.names = names(samplesDB %>% 
                                        rename(entryFile = "updateID")))
  }else{
    # creates an empty df w/ same col names as samplesDB
    samplesIS <- samplesDB[FALSE,] %>% rename(entryFile = "updateID") 
  }
  
  if("profilesIS.csv" %in% logFiles){
    profilesIS <- customReadCSV(file.path(logFilesDir, "profilesIS.csv"))
    assertDataFrame(profilesIS, 
                    col.names = names(profilesDB %>% 
                                        rename(entryFile = "updateID")))
  }else{
    # creates an empty df w/ same col names as profilesDB
    profilesIS <- profilesDB[FALSE,] %>% rename(entryFile = "updateID") 
  }
  
  ## LOG FILES: read them in. If they don't exist, then create them.
  # check for bacteria/phytoplankton log file
  if("bpLogFile.csv" %in% logFiles){ 
    bpIS <- customReadCSV(file.path(logFilesDir, "bpLogFile.csv"))
    assertDataFrame(bpIS)
    assertSetEqual(names(bpIS), names(bpInit), ordered = T)
  }else{
    bpIS <- bpInit
  }
  # check for chlorophyll log file
  if("chlLogFile.csv" %in% logFiles){ 
    chlIS <- customReadCSV(file.path(logFilesDir, "chlLogFile.csv"))
    assertDataFrame(chlIS)
    assertSetEqual(names(chlIS), names(chlInit), ordered = T)  
  }else{
    chlIS <- chlInit
  }
  # check for DOC log file
  if("docLogFile.csv" %in% logFiles){ 
    docIS <- customReadCSV(file.path(logFilesDir, "docLogFile.csv"))
    assertDataFrame(docIS)
    assertSetEqual(names(docIS), names(docInit), ordered = T)
  }else{
    docIS <- docInit
  }
  # check for filtered log file
  if("filteredLogFile.csv" %in% logFiles){ 
    fdIS <- customReadCSV(file.path(logFilesDir, "filteredLogFile.csv"))
    assertDataFrame(fdIS)
    assertSetEqual(names(fdIS), names(fdInit), ordered = T)
  }else{
    fdIS <- fdInit
  }
  # check for POC log file
  if("pocLogFile.csv" %in% logFiles){ 
    pocIS <- customReadCSV(file.path(logFilesDir, "pocLogFile.csv"))
    assertDataFrame(pocIS)
    assertSetEqual(names(pocIS), names(pocInit), ordered = T)
  }else{
    pocIS <- pocInit
  }
  # check for unfiltered log file
  if("unfilteredLogFile.csv" %in% logFiles){ 
    ufdIS <- customReadCSV(file.path(logFilesDir, "unfilteredLogFile.csv"))
    assertDataFrame(ufdIS)
    assertSetEqual(names(ufdIS), names(ufdInit), ordered = T)
  }else{
    ufdIS <- ufdInit
  }
  # check for color log file
  if("colorLogFile.csv" %in% logFiles){ 
    colorIS <- customReadCSV(file.path(logFilesDir, "colorLogFile.csv"))
    assertDataFrame(colorIS)
    assertSetEqual(names(colorIS), names(colorInit), ordered = T)
  }else{
    colorIS <- colorInit
  }
  # check for zoop log file
  if("zoopLogFile.csv" %in% logFiles){ 
    zoopIS <- customReadCSV(file.path(logFilesDir, "zoopLogFile.csv"))
    assertDataFrame(zoopIS)
    assertSetEqual(names(zoopIS), c("zoopID", "projectID", "lakeID", "site", "dateSample", "timeSample", 
                                    "depthClass", "depthTop", "depthBottom", "comments", "sampleID"),
                   ordered = T)
  }else{
    stop("No zooplankton log file available!") # throw an error here instead of initializing a new data frame, because zoops count up from the previous year (unlike all the other log files, which count up from 0.)
  }
  
  # Check which files have already been compiled ----------------------------
  beenCompiled <- unique(samplesIS$entryFile)
  
  ## still to compile
  toCompile <- list.files(sampleSheetsDir, pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv")
  toCompile <- toCompile[!(toCompile %in% beenCompiled)] # exclude data sheets already compiled
  
  if(length(toCompile) == 0){ # There are no files left to compile
    message("The in-season database is up to date; no new limno files to compile.")
  }else{ # There are files left to compile
    ## Initialize labels
    message("Compiling...")
    bpLABS <- data.frame()
    chlLABS <- data.frame()
    docLABS <- data.frame()
    fdLABS <- data.frame()
    pocLABS <- data.frame()
    ufdLABS <- data.frame()
    colorLABS <- data.frame()
    zoopLABS <- data.frame()
    
    # Initialize lists to store volumes and profile data, so we can check it at the end.
    volumesList <- vector(mode = "list", length = length(toCompile)) %>%
      setNames(toCompile)
    profDataList <- vector(mode = "list", length = length(toCompile)) %>%
      setNames(toCompile)
    
    # Generate rows to append -------------------------------------------------
    for(i in 1:length(toCompile)){ # for each data sheet... # XXX will eventually want to make this into a function instead of a for-loop, I think.
      file <- toCompile[i] # name of the current entry file
      message(paste0("Processing file ", i, ": ", file))
      
      # Scan the text file
      cur <- read.csv(file.path(sampleSheetsDir, file), 
                      na.strings = c("NA", "", " "),
                      header = F)
      assertDataFrame(cur, ncols = 11)
      #checkCurMisspellings(cur) # this is really verbose--won't run while checking.
      
      # Replace misspelled or non-lowercase words
      cur <- cur %>%
        mutate(across(everything(), function(x) str_replace(x, "Hypo", "hypo"))) %>%
        mutate(across(everything(), function(x) str_replace(x, "moeties", "moieties"))) %>%
        mutate(across(everything(), function(x) str_replace(x, "Dosat", "DOsat")))
      
      # Pull the header info
      header <- getHeader(cur)
      assertList(header, all.missing = F, len = 11)
      checkHeader(header)
      
      # Tabular data ------------------------------------------------------------
      # Data is retrieved using a series of get*Data functions, which are defined in supportingFuns.R
      ## Profiles
      profData <- getProfData(cur)
      assertDataFrame(profData)
      assertSetEqual(names(profData), expectedProfData, ordered = T)
      if(all(is.na(profData$depth))){
        stop("All depths in profData are NA.")
      }
      profDataList[[file]] <- profData # add data to the list so we can check at the end
      
      ## Gauges
      gauges <- getGauges(cur)
      assertDataFrame(gauges)
      assertSetEqual(names(gauges), expectedGauges, ordered = T)
      
      ## Moieties
      moieties <- getMoieties(cur)
      assertDataFrame(moieties)
      assertSetEqual(names(moieties), expectedMoieties, ordered = T)
      
      ## Volumes
      volumes <- getVolumes(cur)
      assertDataFrame(volumes)
      assertSetEqual(names(volumes), expectedVolumes, ordered = T)
      # save volumes to volumesList
      volumesList[[file]] <- volumes
      
      # Date and time -----------------------------------------------------------------
      # dateSample
      dateSampleString <- header$dateSample # Save for creating sampleIDs later.
      checkDateSampleString(dateSampleString)
      header$dateSample <- strptime(header$dateSample, format = "%Y%m%d") %>% 
        strftime(format = "%Y-%m-%d")
      checkDateSample(header$dateSample)
      
      # timeSample
      header$timeSample <- formatTimeSample(header$timeSample) # fix 3-digit times; validate final result
      timeSampleString <- header$timeSample # Save for creating sampleIDs later.
      header$timeSample <- strptime(header$timeSample, format = "%H%M") %>% 
        strftime(format = "%H:%M")
      checkTimeSample(header$timeSample)
      
      # create new header entry: dateTimeSample
      header$dateTimeSample <- paste0(header$dateSample, " ", 
                                      header$timeSample, ":00") 
      checkDateTimeSample(header$dateTimeSample)
      
      # SAMPLES -----------------------------------------------------------------
      # Generate rows for SAMPLES
      samplesNEW <- samplesIS[FALSE,] # initialize 0-row data frame to store new SAMPLES rows in.
      
      ## Profile samples
      # if there is profile data, generate new rows and add them to samplesNEW
      if(any(colSums(is.na(profData[,c("temp", "DOmgL", "DOsat", "SpC", "pH", "ORP", "PAR")])) < 
             nrow(profData))){ 
        ## Generate
        profSamples <- profileSamplesRows(profData)
        assertDataFrame(profSamples, nrow = nrow(profData))
        assertSetEqual(names(profSamples), c("depthClass", "depthTop", "depthBottom"))
        ## Add
        samplesNEW <- bind_rows(tochar(samplesNEW), 
                                tochar(profSamples)) 
        assertDataFrame(samplesNEW, ncols = ncol(samplesIS))
      }
      
      # PML samples
      # if there is PML data, generate new rows and add them to samplesNEW
      if(sum(!is.na(profData$PML)) > 0){
        pml_depthTop <- min(profData$depth, na.rm = T) # top depth
        pml_depthBottom <- max(profData$depth, na.rm = T) # bottom depth
        ## Generate
        pmlSamples <- data.frame(depthClass = "PML",
                                 depthTop = pml_depthTop,
                                 depthBottom = pml_depthBottom)
        ## Add
        samplesNEW <- bind_rows(tochar(samplesNEW), 
                                tochar(pmlSamples))
        assertDataFrame(samplesNEW, ncols = ncol(samplesIS))
      }else{ # if there's no PML data, set depthBottom and depthTop to NA so that future functions that require these will work. 
        pml_depthTop <- NA
        pml_depthBottom <- NA
      }
      assertScalar(pml_depthTop, na.ok = T)
      assertScalar(pml_depthBottom, na.ok = T)
      
      # hypo samples
      # if there is hypo data, generate new rows and add them to samplesNEW
      if(sum(!is.na(profData$hypo)) > 0){
        hypo_depthTop <- min(profData$depth, na.rm = T)
        hypo_depthBottom <- max(profData$depth, na.rm = T)
        ## Generate
        hypoSamples <- data.frame(depthClass = "hypo",
                                  depthTop = hypo_depthTop,
                                  depthBottom = hypo_depthBottom)
        ## Add
        samplesNEW <- bind_rows(tochar(samplesNEW), 
                                tochar(hypoSamples))
        assertDataFrame(samplesNEW, ncols = ncol(samplesIS))
      }else{
        hypo_depthTop <- NA
        hypo_depthBottom <- NA
      }
      assertScalar(hypo_depthTop, na.ok = T)
      assertScalar(hypo_depthBottom, na.ok = T)
      
      # Point samples
      # if there is any other point data, generate new rows and add them to samplesNEW
      if(any(!is.na(profData$point) & (rowSums(is.na(profData[,c("temp", "DOmgL", "DOsat", "SpC", 
                                                                 "pH", "ORP", "PAR")])) == 7))){ 
        ## identify which rows
        whichRows <- which(!is.na(profData$point) & 
                             (rowSums(is.na(profData[, c("temp", "DOmgL", "DOsat", "SpC", 
                                                         "pH", "ORP", "PAR")])) == 7)) 
        ## Generate
        pointSamples <- pointSamplesRows(profData, whichRows)
        assertDataFrame(pointSamples, nrow = length(whichRows))
        assertSetEqual(names(pointSamples), c("depthClass", "depthTop", "depthBottom"))
        ## Add
        samplesNEW <- bind_rows(tochar(samplesNEW), tochar(pointSamples))
        assertDataFrame(samplesNEW, ncols = ncol(samplesIS))
      }
      
      # Outlet samples
      # if there is outlet data, generate new rows and add them to samplesNEW
      if(any(moieties$Outlet > 0)){ # if there is any outlet data
        ## Generate
        outletSamples <- data.frame(depthClass = "surface",
                                    depthTop = 0,
                                    depthBottom = 0)
        ## Add
        samplesNEW <- bind_rows(tochar(samplesNEW), 
                                tochar(outletSamples))
        assertDataFrame(samplesNEW, ncols = ncol(samplesIS))
      } 
      
      # Inlet samples
      # if there is inlet data, generate new rows and add them to samplesNEW
      inletsSampled <- moieties %>% select(contains("Inlet") & 
                                             where(function(x) sum(x) > 0)) %>% 
        names() # which inlets were sampled?
      assertCharacter(inletsSampled, any.missing = FALSE)
      if(length(inletsSampled) > 0){
        ## Generate
        inletSamples <- data.frame(depthClass = rep("surface", 
                                                    length(inletsSampled)),
                                   depthTop = 0,
                                   depthBottom = 0)
        ## Add
        samplesNEW <- bind_rows(tochar(samplesNEW), tochar(inletSamples))
        assertDataFrame(samplesNEW, ncols = ncol(samplesIS))
      }
      
      # Zooplankton samples
      # if there is zoop data, generate new rows and add them to samplesNEW
      if(!is.na(header$zoopDepth)){ 
        ## Generate
        zoopSamples <- data.frame(depthClass = "tow",
                                  depthTop = 0,
                                  depthBottom = header$zoopDepth)
        ## Add
        samplesNEW <- bind_rows(tochar(samplesNEW), tochar(zoopSamples))
        assertDataFrame(samplesNEW, ncols = ncol(samplesIS))
      }
      
      # Staff gauge samples
      # if there is staff gauge data, generate new rows and add them to samplesNEW
      gaugesSampled <- rownames(gauges)[!is.na(gauges$height)] # which gauges?
      if(length(gaugesSampled) > 0){
        # fix the gauge names
        gaugesSampled <- str_remove(gaugesSampled, "_staffGauge") # remove _staffGauge from the gauge names
        gaugesSampled[gaugesSampled == "lake"] <- "WholeLake"
        
        ## Generate
        gaugeSamples <- data.frame(depthClass = "Staff",
                                   depthTop = 0,
                                   depthBottom = 0)
        ## Add
        samplesNEW <- bind_rows(tochar(samplesNEW), tochar(gaugeSamples))
        assertDataFrame(samplesNEW, ncols = ncol(samplesIS))
      }
      
      # Fill in the information in all the other columns in samplesNEW 
      # (sampleID, dateSample, weather, comments, etc.)
      samplesNEW <- samplesNEW %>%
        mutate(crew = header$crew, weather = header$weather,
               comments = header$comments, 
               # assign a different metadataID for staff gauge samples vs. others:
               metadataID = case_when(depthClass == "Staff" ~ staffGaugeMetadataID,
                                      TRUE ~ header$metadataID),
               dateSample = header$dateSample,
               dateTimeSample = header$dateTimeSample,
               entryFile = file,
               siteID = header$siteID,
               # Construct the sampleID
               sampleID = paste(siteID, dateSampleString, timeSampleString,
                                depthClass, depthBottom, header$metadataID, 
                                sep = "_"))
      
      assertSubset(names(samplesNEW), names(samplesIS)) # check that the names match
      
      samplesNEW <- samplesNEW %>%
        select(names(samplesIS)) %>% # put the columns in the right order
        mutate(projectID = header$projectID) %>% 
        relocate(projectID)
      
      # DATA --------------------------------------------------------------------
      # Profile data
      if(any(colSums(is.na(profData[, c("temp", "DOmgL", "DOsat", "SpC", 
                                        "pH", "ORP", "PAR")])) < 
             nrow(profData))){ # if there is profile data
        # which rows have profile data
        whichRows <- which(rowSums(!is.na(profData[, c("temp", "DOmgL", "DOsat", 
                                                       "SpC", "pH", "ORP", 
                                                       "PAR")])) > 0) 
        # how many rows have profile data
        numRows <- length(whichRows) 

        ## Generate rows
        profilesNEW <- profileDataRows(d = profData, h = header, 
                                       dss = dateSampleString, 
                                       tss = timeSampleString) %>%
          # add a temporary entryFile column so the check function at the end will work
          mutate(entryFile = file) 
      }
      
      # Log file rows
      # Bacteria-phytoplankton DNA
      if(sum(moieties["DNA",]) > 0){ 
        # check agreement b/w moieties and volumes tables
        if(any(which(moieties["DNA",] > 0) != which(volumes["DNA",] > 0))){
          stop("DNA entries in the 'moieties' and 'volumes' tables do not agree!")
        }
        assertCharacter(bpIS$bpID, pattern = "BP")
        curID <- str_remove(bpIS$bpID, "BP") %>% 
          as.numeric() %>% max() + 1 # current bpID
        bpNEW <- bpIS[FALSE,] # new empty data frame for new bp rows
        
        ## Non-point rows
        if(rowSums(moieties["DNA",] %>% select(-point)) > 0){
          rows <- dataRows(idName = "bpID", idPrefix = "BP", idStart = curID, rowName = "DNA", 
                           addReplicates = T, addVolumes = T, templateDF = bpIS, 
                           v = volumes, m = moieties, h = header, pt = pml_depthTop,
                           pb = pml_depthBottom, ht = hypo_depthTop, hb = hypo_depthBottom,
                           dss = dateSampleString, tss = timeSampleString)
          ## Add rows
          bpNEW <- bind_rows(tochar(bpNEW), 
                             tochar(rows))
          assertDataFrame(bpNEW, ncols = ncol(bpIS))
          ## Update curID
          curID <- ifelse(nrow(bpNEW) == 0, # if no new rows were added
                          curID, # keep the current ID
                          # else, increment it
                          max(as.numeric(gsub("BP", "", bpNEW$bpID))) + 1) 
          assertNumeric(curID, len = 1)
        }
        
        ## Point rows
        if(moieties["DNA", "point"] > 0){
          ## Generate rows
          rows <- dataRowsPoint(idName = "bpID", idPrefix = "BP", idStart = curID, addReplicates = T,
                                addVolumes = T, volumesRowName = "DNA", templateDF = bpIS, 
                                color = F, dss = dateSampleString, tss = timeSampleString, 
                                h = header, v = volumes, p = profData)
          ## Add rows
          bpNEW <- bind_rows(tochar(bpNEW), tochar(rows))
          assertDataFrame(bpNEW, ncols = ncol(bpIS))
        }
      }
      
      # Chlorophyll data rows
      if(sum(moieties["chloro",]) > 0){
        if(any(which(moieties["chloro",] > 0) != which(volumes["chloro",] > 0))){ # check agreement b/w moieties and volumes tables for chl
          stop("Chlorophyll entries in the moieties and volume filtered tables do not agree!")
        }
        
        curID <- gsub("C", "", chlIS$chlID) %>% as.numeric() %>% max() + 1
        chlNEW <- chlIS[FALSE,] # empty data frame for new chl rows
        
        ## Non-point rows
        if(rowSums(moieties["chloro",] %>% select(-point)) > 0){
          ## Generate rows
          rows <- dataRows(idName = "chlID", idPrefix = "C", idStart = curID, rowName = "chloro", 
                           addReplicates = T, addVolumes = T, templateDF = chlIS, 
                           v = volumes, m = moieties, h = header, pt = pml_depthTop,
                           pb = pml_depthBottom, ht = hypo_depthTop, hb = hypo_depthBottom,
                           dss = dateSampleString, tss = timeSampleString)
          ## Add rows
          chlNEW <- bind_rows(tochar(chlNEW), tochar(rows))
          assertDataFrame(chlNEW, ncols = ncol(chlIS))
          
          ## update curID
          curID <- ifelse(nrow(chlNEW) == 0, curID, 
                          max(as.numeric(gsub("C", "", chlNEW$chlID))) + 1)
          assertNumeric(curID, len = 1)
        }
        
        # Chlorophyll data rows (point samples)
        if(moieties["chloro", "point"] > 0){
          ## Generate rows
          rows <- dataRowsPoint(idName = "chlID", idPrefix = "C", idStart = curID, addReplicates = T,
                                addVolumes = T, volumesRowName = "chloro", templateDF = chlIS, 
                                color = F, dss = dateSampleString, tss = timeSampleString, 
                                h = header, v = volumes, p = profData)
          ## Add rows
          chlNEW <- bind_rows(tochar(chlNEW), tochar(rows))
          assertDataFrame(chlNEW, ncols = ncol(chlIS))
          
        }
      }
      
      # DOC data rows (non-point samples)
      if(sum(moieties["DOC", ]) > 0){
        curID <- gsub("D", "", docIS$docID) %>% as.numeric() %>% max() +1
        docNEW <- docIS[FALSE,] # empty data frame for new doc rows
        
        ## Non-point rows
        if(rowSums(moieties["DOC",] %>% select(-point)) > 0){
          rows <- dataRows(idName = "docID", idPrefix = "D", idStart = curID, rowName = "DOC", 
                           addReplicates = T, addVolumes = F, templateDF = docIS, 
                           v = volumes, m = moieties, h = header, pt = pml_depthTop,
                           pb = pml_depthBottom, ht = hypo_depthTop, hb = hypo_depthBottom,
                           dss = dateSampleString, tss = timeSampleString)
          ## Add rows
          docNEW <- bind_rows(tochar(docNEW), tochar(rows))
          assertDataFrame(docNEW, ncols = ncol(docIS))
          
          ## Update curID
          curID <- ifelse(nrow(docNEW) == 0, curID, 
                          max(as.numeric(gsub("D", "", docNEW$docID))) + 1)
          assertNumeric(curID, len = 1)
        }
        
        # DOC data rows (point samples)
        if(moieties["DOC", "point"] > 0){
          ## Generate rows
          rows <- dataRowsPoint(idName = "docID", idPrefix = "D", idStart = curID, addReplicates = T,
                                addVolumes = F, volumesRowName = NULL, templateDF = docIS, 
                                color = F, dss = dateSampleString, tss = timeSampleString, 
                                h = header, v = volumes, p = profData)
          ## Add rows
          docNEW <- bind_rows(tochar(docNEW), tochar(rows))
          assertDataFrame(docNEW, ncols = ncol(docIS))
        }
      }
      
      # Filtered data rows (non-point samples)
      if(sum(moieties["filtered",]) > 0){
        curID <- gsub("F", "", fdIS$filteredID) %>% as.numeric() %>% max() + 1
        fdNEW <- fdIS[FALSE, ] # empty data frame for new filtered rows 
        
        ## Non-point rows
        if(rowSums(moieties["filtered",] %>% select(-point)) > 0){
          ## Generate rows
          rows <- dataRows(idName = "filteredID", idPrefix = "F", idStart = curID, rowName = "filtered", 
                           addReplicates = F, addVolumes = F, templateDF = fdIS, 
                           v = volumes, m = moieties, h = header, pt = pml_depthTop,
                           pb = pml_depthBottom, ht = hypo_depthTop, hb = hypo_depthBottom,
                           dss = dateSampleString, tss = timeSampleString)
          ## Add rows
          fdNEW <- bind_rows(tochar(fdNEW), tochar(rows))
          assertDataFrame(fdNEW, ncols = ncol(fdIS))
          
          ## Update curID
          curID <- ifelse(nrow(fdNEW) == 0, curID, 
                          max(as.numeric(gsub("F", "", fdNEW$filteredID))) + 1)
          assertNumeric(curID, len = 1)
        }
        
        # Filtered data rows (point samples)
        if(moieties["filtered", "point"] > 0){
          ## Generate rows
          rows <- dataRowsPoint(idName = "filteredID", idPrefix = "F", idStart = curID, addReplicates = F,
                                addVolumes = F, volumesRowName = NULL, templateDF = fdIS, 
                                color = F, dss = dateSampleString, tss = timeSampleString, 
                                h = header, v = volumes, p = profData)
          ## Add rows
          fdNEW <- bind_rows(tochar(fdNEW), tochar(rows))
          assertDataFrame(fdNEW, ncols = ncol(fdIS))
        }
      }
      
      # POC data rows (non-point samples)
      if(sum(moieties["POC",]) > 0){
        if(any(which(moieties["POC",] > 0) != which(volumes["POC",] > 0))){
          stop(paste0("POC entries in the moieties and volume filtered tables do not agree! The problematic data sheet is: ", file))
        }
        curID <- gsub("P", "", pocIS$pocID) %>% as.numeric() %>% max() + 1
        pocNEW <- pocIS[FALSE, ] # empty data frame for new poc rows 
        
        ## Non-point rows
        if(rowSums(moieties["POC",] %>% select(-point)) > 0){
          ## Generate rows
          rows <- dataRows(idName = "pocID", idPrefix = "P", idStart = curID, rowName = "POC", 
                           addReplicates = T, addVolumes = T, templateDF = pocIS, 
                           v = volumes, m = moieties, h = header, pt = pml_depthTop,
                           pb = pml_depthBottom, ht = hypo_depthTop, hb = hypo_depthBottom,
                           dss = dateSampleString, tss = timeSampleString)
          ## Add rows
          pocNEW <- bind_rows(tochar(pocNEW), tochar(rows))
          assertDataFrame(pocNEW, ncols = ncol(pocIS))
          
          ## Update curID
          curID <- ifelse(nrow(pocNEW) == 0, curID, 
                          max(as.numeric(gsub("P", "", pocNEW$pocID))) + 1)
          assertNumeric(curID, len = 1)
        }
        
        # POC data rows (point samples)
        if(moieties["POC", "point"] > 0){
          ## Generate rows
          rows <- dataRowsPoint(idName = "pocID", idPrefix = "P", idStart = curID, addReplicates = T,
                                addVolumes = T, volumesRowName = "POC", templateDF = pocIS, 
                                color = F, dss = dateSampleString, tss = timeSampleString, 
                                h = header, v = volumes, p = profData)
          ## Add rows
          pocNEW <- bind_rows(tochar(pocNEW), tochar(rows))
          assertDataFrame(pocNEW, ncols = ncol(pocIS))
        }
      }
      
      # Unfiltered data rows (non-point samples)
      if(sum(moieties["unfiltered", ]) > 0){
        curID <- gsub("U", "", ufdIS$unfilteredID) %>% as.numeric() %>% max() + 1
        ufdNEW <- ufdIS[FALSE,]
        
        ## Non-point rows
        if(rowSums(moieties["unfiltered",] %>% select(-point)) > 0){
          ## Generate rows
          rows <- dataRows(idName = "unfilteredID", idPrefix = "U", idStart = curID, rowName = "unfiltered", 
                           addReplicates = F, addVolumes = F, templateDF = ufdIS, 
                           v = volumes, m = moieties, h = header, pt = pml_depthTop,
                           pb = pml_depthBottom, ht = hypo_depthTop, hb = hypo_depthBottom,
                           dss = dateSampleString, tss = timeSampleString)
          ## Add rows
          ufdNEW <- bind_rows(tochar(ufdNEW), tochar(rows))
          assertDataFrame(ufdNEW, ncols = ncol(ufdIS))
          
          ## Update curID
          curID <- ifelse(nrow(ufdNEW) == 0, curID, 
                          max(as.numeric(gsub("U", "", ufdNEW$unfilteredID))) + 1)
          assertNumeric(curID, len = 1)
        }
        
        # Unfiltered data rows (point samples)
        if(moieties["unfiltered", "point"] > 0){
          ## Generate rows
          rows <- dataRowsPoint(idName = "unfilteredID", idPrefix = "U", idStart = curID, addReplicates = F,
                                addVolumes = F, volumesRowName = NULL, templateDF = ufdIS, 
                                color = F, dss = dateSampleString, tss = timeSampleString, 
                                h = header, v = volumes, p = profData)
          ## Add rows
          ufdNEW <- bind_rows(tochar(ufdNEW), tochar(rows))
          assertDataFrame(ufdNEW, ncols = ncol(ufdIS))
        }
      }
      
      # Color data rows (non-point samples)
      if(sum(moieties["color",]) > 0){
        curID <- gsub("C", "", colorIS$colorID) %>% as.numeric() %>% max() + 1
        colorNEW <- colorIS[FALSE, ] # empty data frame for new color rows
        
        ## Non-point rows
        if(rowSums(moieties["color",] %>% select(-point)) > 0){
          ## Generate rows
          rows <- dataRows(idName = "colorID", idPrefix = "C", idStart = curID, rowName = "color", 
                           addReplicates = F, addVolumes = F, templateDF = colorIS, 
                           v = volumes, m = moieties, h = header, pt = pml_depthTop,
                           pb = pml_depthBottom, ht = hypo_depthTop, hb = hypo_depthBottom,
                           dss = dateSampleString, tss = timeSampleString)
          ## Add rows
          colorNEW <- bind_rows(tochar(colorNEW), tochar(rows))
          assertDataFrame(colorNEW, ncols = ncol(colorIS))
          
          ## Update curID
          curID <- ifelse(nrow(colorNEW) == 0, curID, 
                          max(as.numeric(gsub("C", "", colorNEW$colorID))) + 1)
          assertNumeric(curID, len = 1)
        }
        
        # Color data rows (point samples)
        if(moieties["color", "point"] > 0){
          ## Generate rows
          rows <- dataRowsPoint(idName = "colorID", idPrefix = "C", idStart = curID, addReplicates = F,
                                addVolumes = F, volumesRowName = NULL, templateDF = colorIS, 
                                color = T, dss = dateSampleString, tss = timeSampleString, 
                                h = header, v = volumes, p = profData)
          ## Add rows
          colorNEW <- bind_rows(tochar(colorNEW), tochar(rows))
          assertDataFrame(colorNEW, ncols = ncol(colorIS))
        }
      }
      
      # Zooplankton data rows (tow samples)
      if(!is.na(header$zoopDepth)){
        curID <- gsub("Z", "", zoopIS$zoopID) %>% as.numeric() %>% max() + 1
        
        ## Generate and add rows
        zoopNEW <- zoopDataRows(c = curID, h = header, dss = dateSampleString, 
                                tss = timeSampleString, df = zoopIS) 
        assertDataFrame(zoopNEW, ncols = ncol(zoopIS))
      }
      
      # APPEND NEW INFO TO LOG FILES --------------------------------------------
      samplesIS <- bind_rows(tochar(samplesIS), 
                             tochar(samplesNEW))
      profilesIS <- bind_rows(tochar(profilesIS), 
                              tochar(profilesNEW))
      
      if(exists("bpNEW")){bpIS <- bind_rows(tochar(bpIS), tochar(bpNEW))}
      if(exists("chlNEW")){chlIS <- bind_rows(tochar(chlIS), tochar(chlNEW))}
      if(exists("docNEW")){docIS <- bind_rows(tochar(docIS), tochar(docNEW))}
      if(exists("fdNEW")){fdIS <- bind_rows(tochar(fdIS), tochar(fdNEW))}
      if(exists("pocNEW")){pocIS <- bind_rows(tochar(pocIS), tochar(pocNEW))}
      if(exists("ufdNEW")){ufdIS <- bind_rows(tochar(ufdIS), tochar(ufdNEW))}
      if(exists("colorNEW")){colorIS <- bind_rows(tochar(colorIS), tochar(colorNEW))}
      if(!is.na(header$zoopDepth)){zoopIS <- bind_rows(tochar(zoopIS), tochar(zoopNEW))}
      
      
      # APPEND NEW INFO TO CURRENT LABELS ---------------------------------------
      ## Append new info
      if(exists("bpNEW")){bpLABS <- bind_rows(bpLABS, bpNEW)}
      if(exists("chlNEW")){chlLABS <- bind_rows(chlLABS, chlNEW)}
      if(exists("docNEW")){docLABS <- bind_rows(docLABS, docNEW)}
      if(exists("fdNEW")){fdLABS <- bind_rows(fdLABS, fdNEW)}
      if(exists("pocNEW")){pocLABS <- bind_rows(pocLABS, pocNEW)}
      if(exists("ufdNEW")){ufdLABS <- bind_rows(ufdLABS, ufdNEW)}
      if(exists("colorNEW")){colorLABS <- bind_rows(colorLABS, colorNEW)}
      if(!is.na(header$zoopDepth)){zoopLABS <- bind_rows(zoopLABS, zoopNEW)}
      
      print(paste0(file, " complete!"))
    } 
    
    # CHECKS ------------------------------------------------------------------
    message("Performing checks...")
    # Check for sampleID's that are already in the database
    repeatSampleIDsCheck(tc = toCompile, db = samplesDB, is = samplesIS)
    # Check for new siteID's
    newSiteIDsCheck(tc = toCompile, db = samplesDB, 
                    is = samplesIS, f = force_siteID)
    # Check for new lakeID's
    newLakeIDsCheck(tc = toCompile, db = samplesDB, 
                    is = samplesIS, f = force_lakeID)
    # Check for new projectID's
    newProjectIDsCheck(tc = toCompile, db = samplesDB, 
                       is = samplesIS, f = force_newProjectID)
    # Check for projectID's that are supposed to be retired
    retiredProjectIDsCheck(tc = toCompile, is = samplesIS, 
                           f = force_retiredProjectID)
    # Check for times outside expected bounds
    sampleTimesCheck(tc = toCompile, is = samplesIS, f = force_timeSample)
    # Check for new metadataID's
    newMetadataIDsCheck(tc = toCompile, db = samplesDB, 
                        is = samplesIS, f = force_metadataID)
    # Check for unprecedented depths
    depthsCheck(tc = toCompile, db = profilesDB, 
                is = profilesIS, f = force_depth)
    # Check for new inlets and outlets
    inletOutletCheck(tc = toCompile, db = samplesDB, is = samplesIS, 
                     fi = force_inlet, fo = force_outlet)
    # Check for filter volumes outside expected bounds
    filterVolumesCheck(vl = volumesList, f = force_volumeFiltered)
    # Check for profile data outside expected bounds
    profileRangeCheck(p = profDataList, ft = force_profileTemp, 
                      fdm = force_profileDOmgL, fds = force_profileDOsat,
                      fs = force_profileSpC, fph = force_profilePH,
                      fo = force_profileORP, fpa = force_profilePAR)
    
    message("Passed checks! Writing files...")
    
    # WRITE INFO TO EXCEL SHEETS FOR PRINTING LABELS --------------------------
    write.xlsx(bpLABS, here("labels", "bpLabels.xlsx"))
    write.xlsx(chlLABS, here("labels", "chlLabels.xlsx"))
    write.xlsx(docLABS, here("labels", "docLabels.xlsx"))
    write.xlsx(fdLABS, here("labels", "filteredLabels.xlsx"))
    write.xlsx(pocLABS, here("labels", "pocLabels.xlsx"))
    write.xlsx(ufdLABS, here("labels", "unfilteredLabels.xlsx"))
    write.xlsx(colorLABS, here("labels", "colorLabels.xlsx"))
    if(!is.na(header$zoopDepth)){write.xlsx(zoopLABS, here("labels", "zoopLabels.xlsx"))}
    
    # WRITE NEW IN-SEASON DATABASE FILES TO TXT -------------------------------
    ## samples and profiles
    write.csv(samplesIS, here(logFilesDir, "samplesIS.csv"), row.names = FALSE)
    write.csv(profilesIS %>% 
                select(-entryFile),  # remove the temporary entryFile column
              here(logFilesDir, "profilesIS.csv"), row.names = FALSE)
    
    ## other log files
    write.csv(bpIS, here(logFilesDir, "bpLogFile.csv"), row.names = FALSE)
    write.csv(chlIS, here(logFilesDir, "chlLogFile.csv"), row.names = FALSE)
    write.csv(docIS, here(logFilesDir, "docLogFile.csv"), row.names = FALSE)
    write.csv(fdIS, here(logFilesDir, "filteredLogFile.csv"), row.names = FALSE)
    write.csv(pocIS, here(logFilesDir, "pocLogFile.csv"), row.names = FALSE)
    write.csv(ufdIS, here(logFilesDir, "unfilteredLogFile.csv"), row.names = FALSE)
    write.csv(colorIS, here(logFilesDir, "colorLogFile.csv"), row.names = FALSE)
    write.csv(zoopIS, here(logFilesDir, "zoopLogFile.csv"), row.names = FALSE)
    message("Done!")
  }
}


# Next to do:
# 7. How to implement an "undo" function? Maybe... put a date stamp on the entries. Then if the user selects the "undo" option, check the date stamp, check whether it was done in the past hour/day/week/whatever, then print an "are you sure" prompt. 
