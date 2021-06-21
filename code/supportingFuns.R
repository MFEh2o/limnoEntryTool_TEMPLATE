# Supporting functions for limnoEntry.R

# MetadataID for staff gauge samples --------------------------------------
staffGaugeMetadataID <- "Staff.Gauge.Sample.20140319"

# List of retired projectIDs ----------------------------------------------
retiredProjectIDs <- c(1:2, 4:5, 7:12, 14, 16, 18:25, 27:33, 39:40)

# count NA's --------------------------------------------------------------
count_na <- function(x) sum(is.na(x))

# initial one-row data frames for each log file ---------------------------
bpInit <- data.frame(bpID = "BP0",
                     projectID = 99999,
                     lakeID = "ZZ",
                     site = "xxxx",
                     dateSample = "2222-22-22",
                     timeSample = "99:99",
                     depthClass = "surface",
                     depthTop = 0,
                     depthBottom = 0,
                     volFiltered = 0,
                     replicate = 1,
                     comments = "aaa",
                     sampleID = "ZZ_xxxx_NA_9999_surface_0_fishScapesLimno.20190319",
                     metadataID = NA)

chlInit <- data.frame(chlID = "C0",
                      projectID = 99999,
                      lakeID = "ZZ",
                      site = "zzz",
                      dateSample = "2222-22-22",
                      timeSample = "99:99",
                      depthClass = "surface",
                      depthTop = 0,
                      depthBottom = 0,
                      volFiltered = 0,
                      replicate = 1,
                      comments = "aaa",
                      sampleID = "ZZ_zzz_NA_9999_surface_0_fishScapesLimno.20190319",
                      metadataID = NA)

colorInit <- data.frame(colorID = "C0",
                        projectID = 99999,
                        lakeID = "ZZ",
                        site = "zzz",
                        dateSample = "2222-22-22",
                        timeSample = "99:99",
                        depthClass = "surface",
                        depthTop = 0,
                        depthBottom = 0,
                        abs440 = 0,
                        g440 = 0,
                        comments = "aaa",
                        sampleID = "ZZ_zzz_NA_9999_surface_0_fishScapesLimno.20190319")

docInit <- data.frame(docID = "D0",
                      projectID = 99999,
                      lakeID = "ZZ",
                      site = "zzz",
                      dateSample = "2222-22-22",
                      timeSample = "99:99",
                      depthClass = "surface",
                      depthTop = 0,
                      depthBottom = 0,
                      replicate = 1,
                      comments = "aaa",
                      sampleID = "ZZ_zzz_NA_9999_surface_0_fishScapesLimno.20190319",
                      metadataID = NA)

fdInit <- data.frame(filteredID = "F0",
                     projectID = 99999,
                     lakeID = "ZZ",
                     site = "zzz",
                     dateSample = "2222-22-22",
                     timeSample = "99:99",
                     depthClass = "surface",
                     depthTop = 0,
                     depthBottom = 0,
                     comments = "aaa",
                     sampleID = "ZZ_zzz_NA_9999_surface_0_fishScapesLimno.20190319",
                     metadataID = NA)

pocInit <- data.frame(pocID = "P0",
                      projectID = 99999,
                      lakeID = "ZZ",
                      site = "zzz",
                      dateSample = "2222-22-22",
                      timeSample = "99:99",
                      depthClass = "surface",
                      depthTop = 0,
                      depthBottom = 0,
                      volFiltered = 0,
                      replicate = "aaaa",
                      comments = "aaa",
                      sampleID = "ZZ_zzz_NA_9999_surface_0_fishScapesLimno.20190319",
                      metadataID = NA)

ufdInit <- data.frame(unfilteredID = "U0",
                      projectID = 99999,
                      lakeID = "ZZ",
                      site = "zzz",
                      dateSample = "2222-22-22",
                      timeSample = "99:99",
                      depthClass = "surface",
                      depthTop = 0,
                      depthBottom = 0,
                      comments = "aaa",
                      sampleID = "ZZ_zzz_NA_9999_surface_0_fishScapesLimno.20190319",
                      metadataID = NA)


# Custom function for reading csv's and removing NA rows ------------------

# ARGUMENTS:
## `filepath`: path to the file. 
## `header`: passed to read.csv. Whether or not the csv has a header row included. Should be T most of the time.
## `stringsAsFactors`: passed to read.csv. Whether to treat strings as factors, or leave them as character.
## `na.strings`: passed to read.csv. Strings to treat as NA on file read-in.

customReadCSV <- function(filepath, header = T, stringsAsFactors = F, 
                          na.strings = c("", " ", NA)){
  assertCharacter(filepath, len = 1)
  assertLogical(header, len = 1)
  assertLogical(stringsAsFactors, len = 1)
  assertCharacter(na.strings, any.missing = T, all.missing = T)
  library(dplyr) # for the pipe
  
  obj <- read.csv(filepath, # read in the file with all the arguments set
                  header = header, 
                  stringsAsFactors = stringsAsFactors, 
                  na.strings = na.strings) %>%
    filter(rowSums(is.na(.)) != ncol(.))
  return(obj) # return the data frame read from the csv, with NA rows removed.
}

# getHeaderInfo -----------------------------------------------------------
getHeader <- function(d = cur){
  assertDataFrame(d, ncols = 11, min.rows = 2)
  
  header <- d[1:10, 2] %>%
    setNames(c("lakeID", "siteName", "dateSample", "timeSample", 
               "projectID", "weather", "crew", "metadataID", 
               "zoopDepth", "comments")) %>%
    as.list()
  
  # Add siteID, since we'll use that a lot
  header$siteID <- paste(header$lakeID, header$siteName, sep = "_")
  
  # Convert zoopDepth to numeric
  header$zoopDepth <- as.numeric(header$zoopDepth)
  
  # Return the header list
  return(header)
}

# Get profile data from the data sheet ------------------------------------
getProfData <- function(d = cur){
  assertDataFrame(d, min.rows = 17)
  
  profData <- d[17:nrow(d),]
  names(profData) <- profData[1,]
  profData <- profData[-1,]
  profData <- profData %>%
    mutate(across(everything(), as.numeric))
  
  # remove any data that's all NA except for the depth
  profData <- profData %>%
    filter(!(rowSums(is.na(select(., -depth))) == 
               ncol(.) - 1))
  
  return(profData)
}

# Get gauge data from the data sheet -------------------------------------
getGauges <- function(d = cur){
  assertDataFrame(d, min.rows = 16, ncols = 11)
  
  gauges <- d[11:16, 1:3]
  names(gauges) <- gauges[1,]
  gauges <- gauges[-1,]
  row.names(gauges) <- gauges[,1]
  names(gauges) <- c("NA", "height", "unit")
  gauges <- gauges[, -1]
  gauges$height <- as.numeric(gauges$height)
  
  return(gauges)
}

# Get moieties data from the data sheet -----------------------------------
getMoieties <- function(d = cur){
  moieties <- d[2:8, 5:11]
  row.names(moieties) <- d[2:8, 4]
  names(moieties) <- d[1, 5:11]
  moieties <- moieties %>%
    mutate(across(everything(), as.numeric))
  
  return(moieties)
}


# Get volumes data from the data sheet ------------------------------------
getVolumes <- function(d = cur){
  assertDataFrame(d, min.rows = 12, ncols = 11)
  
  volumes <- d[10:12, 5:11]
  row.names(volumes) <- d[10:12, 4]
  names(volumes) <- d[9, 5:11]
  volumes <- volumes %>%
    mutate(across(everything(), as.numeric))
  
  return(volumes)
}

# Format timeSample ------------------------------------------------------
formatTimeSample <- function(x){
  assertCharacter(x, len = 1, any.missing = F, min.chars = 3)
  
  if(nchar(x) == 3){
    x <- paste0("0", x)
    message("header$timeSample has 3 characters; appending a leading 0.")
  }
  if(!grepl("^[0-2][0-9][0-5][0-9]$", x)){
    stop("timeSample is not a valid time.")
  }
  return(x)
}

# profileSamplesRows ----------------------------------------------
profileSamplesRows <- function(d = profData){
  assertDataFrame(d)
  assertChoice("depth", names(d))
  
  profSamples <- d %>%
    select(depth) %>%
    mutate(depthClass = "point",
           depthTop = depth,
           depthBottom = depth) %>%
    select(-depth)
  
  return(profSamples)
}

# pointSamplesRows --------------------------------------------------------
pointSamplesRows <- function(d = profData, ind = whichRows){
  assertDataFrame(d)
  assertNumeric(ind, min.len = 1, max.len = nrow(d))
  assertChoice("depth", names(d))
  
  pointSamples <- d[ind,] %>%
    select(depth) %>%
    mutate(depthClass = "point",
           depthTop = depth,
           depthBottom = depth) %>%
    select(-depth)
  
  return(pointSamples)
}

# profileDataRows ---------------------------------------------------------
profileDataRows <- function(d = profData, h = header, dss = dateSampleString, 
                            tss = timeSampleString){
  assertDataFrame(d)
  assertList(h)
  assertSubset(c("PML", "hypo", "point", "depth"), names(d))
  assertSubset(c("projectID", "lakeID", "siteName", "dateSample", "dateTimeSample", "metadataID", "comments"), names(h))
  assertCharacter(dss, len = 1, pattern = "[0-9]{8}")
  assertCharacter(tss, len = 1, pattern = "[0-9]{4}")
  
  profilesNEW <- d %>%
    select(-c("PML", "hypo", "point")) %>%
    rename("depthBottom" = depth) %>% 
    mutate(projectID = h$projectID,
           lakeID = h$lakeID,
           siteName = h$siteName,
           dateSample = h$dateSample,
           dateTimeSample = h$dateTimeSample,
           depthClass = "point",
           depthTop = depthBottom,
           metadataID = h$metadataID,
           comments = h$comments,
           updateID = NA) %>%
    mutate(sampleID = paste(lakeID, siteName, dss, tss, depthClass, depthBottom, metadataID, sep = "_")) %>%
    # put the columns in the right order
    select(c("projectID", "sampleID", "lakeID", "siteName", "dateSample", 
             "dateTimeSample", "depthClass", "depthTop", "depthBottom", "temp", 
             "DOmgL", "DOsat", "SpC", "pH", "ORP", "PAR", "metadataID", "comments", 
             "updateID"))
  
  return(profilesNEW)
}

# General dataRows function -----------------------------------------------
dataRows <- function(idName, idPrefix, idStart = curID, rowName, 
                     addReplicates = F, addVolumes = F, templateDF, 
                     v = volumes, m = moieties, h = header, pt = pml_depthTop,
                     pb = pml_depthBottom, ht = hypo_depthTop, 
                     hb = hypo_depthBottom,
                     dss = dateSampleString, tss = timeSampleString){
  assertCharacter(idName, len = 1)
  assertCharacter(idPrefix, len = 1)
  assertNumeric(idStart, len = 1)
  assertCharacter(rowName, len = 1)
  assertLogical(addReplicates, len = 1)
  assertLogical(addVolumes, len = 1)
  assertDataFrame(templateDF)
  assertDataFrame(v)
  assertDataFrame(m)
  assertList(h)
  assertNumeric(pt, len = 1)
  assertNumeric(pb, len = 1)
  assertNumeric(ht, len = 1)
  assertNumeric(hb, len = 1)
  assertCharacter(dss, len = 1, pattern = "[0-9]{8}")
  assertCharacter(tss, len = 1, pattern = "[0-9]{4}")
  assertChoice("point", names(m))
  assertSubset(c("siteName", "projectID", "lakeID", 
                 "timeSample", "dateSample", "metadataID"), 
               names(h))
  
  # If we'll be joining volumes, prepare the volumes data for the join
  if(addVolumes == TRUE){
    vols <- v[rowName, ] %>%
      pivot_longer(cols = everything(),
                   names_to = "type",
                   values_to = "volFiltered") %>%
      filter(volFiltered > 0)
  }
  
  # First, take the moieties data...
  rows <- m[rowName, ] %>%
    # remove the point samples
    select(-point) %>%
    # pivot to long format so it's easier to work with.
    pivot_longer(cols = everything(),
                 names_to = "type",
                 values_to = "nSamples") %>%
    # Remove any rows where nSamples is 0.
    filter(nSamples > 0) %>%
    # If volumes = T, join volume info (prepared above)
    {if(addVolumes == TRUE) left_join(., vols, by = "type") else .} %>%
    # Expand nSamples to give us the right number of rows 
    uncount(nSamples) %>%
    # The "type" column technically contains both depthClasses and sites. 
    # That's confusing. Let's separate them.
    ## 1. For the actual depth classes, add the site name from `header`. 
    ## Else, use `type` as site. 
    mutate(site = case_when(type %in% c("PML", "hypo") ~ h$siteName,
                            TRUE ~ type),
           ## 2. For the sites, assign depthClass "surface". 
           ## Else, use `type` as depthClass
           depthClass = case_when(!type %in% c("PML", "hypo") ~ "surface",
                                  TRUE ~ type)) %>%
    select(-type) %>% # no longer need this column
    # Define depths conditionally based on depth classes
    mutate(depthTop = case_when(depthClass == "surface" ~ 0,
                                depthClass == "PML" ~ pt,
                                depthClass == "hypo" ~ ht),
           depthBottom = case_when(depthClass == "surface" ~ 0,
                                   depthClass == "PML" ~ pb,
                                   depthClass == "hypo" ~ hb)) %>%
    # Add the rest of the information from the header:
    mutate(!!idName := paste0(idPrefix, 
                              seq(from = idStart,
                                  length.out = nrow(.))),
           projectID = h$projectID,
           lakeID = h$lakeID,
           dateSample = h$dateSample,
           timeSample = h$timeSample,
           metadataID = h$metadataID,
           comments = NA) %>%
    # If these are color rows, add abs440 and g440
    {if(rowName == "color") mutate(., abs440 = NA, g440 = NA) else .} %>%
    # Create sampleID's
    mutate(sampleID = paste(lakeID, site, dss, tss,
                            depthClass, depthBottom, metadataID, sep = "_")) %>%
    # Add a `replicate` column only if replicates = T
    {if(addReplicates == TRUE) {group_by(., site, depthClass) %>%
        mutate(replicate = 1:n()) %>%
        ungroup()} else .} %>%
    # Put the columns in the right order
    select(names(templateDF)) %>%
    as.data.frame()
  
  # Return the finished data
  return(rows)
}

# General dataRowsPoint function ------------------------------------------
dataRowsPoint <- function(idName, idPrefix, idStart = curID, addReplicates = F,
                          addVolumes = F, volumesRowName = NULL, templateDF, 
                          color = F, dss = dateSampleString, 
                          tss = timeSampleString, 
                          h = header, v = volumes, p = profData){
  
  assertCharacter(idName, len = 1)
  assertCharacter(idPrefix, len = 1)
  assertNumeric(idStart, len = 1)
  assertLogical(addReplicates, len = 1)
  assertLogical(addVolumes, len = 1)
  assertLogical(color, len = 1)
  assertCharacter(volumesRowName, len = 1, null.ok = T)
  assertDataFrame(templateDF)
  assertDataFrame(v)
  assertList(h)
  assertCharacter(dss, len = 1, pattern = "[0-9]{8}")
  assertCharacter(tss, len = 1, pattern = "[0-9]{4}")
  assertChoice("point", v)
  assertDataFrame(profData)
  assertSubset(c("point", "depth"), names(profData))
  assertSubset(c("siteName", "projectID", "lakeID", 
                 "timeSample", "dateSample", "metadataID"), 
               names(h))
  
  # If we'll be joining volumes, prepare the volumes data for the join
  if(addVolumes == TRUE){
    vol <- v[volumesRowName, "point"] # this will be a scalar
  }
  
  # First, take the profile data...
  r <- p %>%
    filter(!is.na(point)) %>%
    select("depthTop" = depth) %>%
    # Assign ID's that count up from the idStart
    mutate(!!idName := paste0(idPrefix, seq(from = idStart, 
                                            length.out = nrow(.)))) %>%
    # Add other header info
    mutate(projectID = h$projectID,
           lakeID = h$lakeID,
           site = h$siteName,
           dateSample = h$dateSample,
           timeSample = h$timeSample,
           depthClass = "point", # because these are point samples
           depthBottom = depthTop,
           comments = NA,
           metadataID = h$metadataID) %>%
    mutate(sampleID = paste(lakeID, site, dss, tss, "point", 
                            depthBottom, metadataID, sep = "_")) %>%
    # Add volumes only if addVolumes is TRUE
    {if(addVolumes == TRUE) mutate(., volFiltered = vol) else .} %>%
    # If these are color rows, add abs440 and g440
    {if(color == TRUE) mutate(., abs440 = NA, g440 = NA) else .} %>%
    {if(addReplicates == TRUE) {group_by(., sampleID) %>%
        mutate(replicate = 1:n()) %>%
        ungroup()} else .} %>%
    # Transform to data frame
    as.data.frame()
  
  # Return the finished rows
  return(r)
}

# zoopDataRows ------------------------------------------------------------
zoopDataRows <- function(c = curID, h = header, dss = dateSampleString, tss = timeSampleString, df = zoopIS){
  assertNumeric(c, len = 1)
  assertList(h)
  assertCharacter(dss, len = 1, pattern = "[0-9]{8}")
  assertCharacter(tss, len = 1, pattern = "[0-9]{4}")
  assertDataFrame(df)
  assertSubset(c("projectID", "lakeID", "siteName", "dateSample", "timeSample", "zoopDepth", "metadataID"), 
               names(h))
  
  rows <- data.frame(
    zoopID = paste0("Z", c), # XXX asked Stuart if this is right
    projectID = h$projectID,
    lakeID = h$lakeID,
    site = h$siteName,
    dateSample = h$dateSample,
    timeSample = h$timeSample,
    depthClass = "tow",
    depthTop = 0,
    depthBottom = h$zoopDepth,
    comments = "",
    metadataID = h$metadataID,
    stringsAsFactors = F
  ) %>%
    mutate(sampleID = paste(lakeID, site, dss, tss, 
                            "tow", depthBottom, metadataID, sep = "_")) %>%
    select(names(df)) %>%
    as.data.frame()
  
  return(rows)
}


# tochar ------------------------------------------------------------------
# shortcut for converting all columns in a data frame to character
tochar <- function(df){
  assertDataFrame(df)
  
  df2 <- df %>% 
    mutate(across(everything(), 
                  as.character))
  return(df2)
}


