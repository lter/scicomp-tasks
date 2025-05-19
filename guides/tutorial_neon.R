## -------------------------------------------------------------- ##
                # Neon Tutorial(s)
## -------------------------------------------------------------- ##

# This script is used for the following tutorials:
### 1) Download and Explore NEON Data
# https://www.neonscience.org/resources/learning-hub/tutorials/download-explore-neon-data
### 2) Introduction to Small Mammal Data
# https://www.neonscience.org/resources/learning-hub/tutorials/mammal-data-intro

## -------------------------------------------- ##
# NEON Tutorial No. 1 ----
## -------------------------------------------- ##

### NOTE ON THIS TUTORIAL
# You must download PAR data manually and put the zip file in the "data" folder
# The tutorial depends on doing that AND doing the direct download steps

# Load libraries
# install.packages("librarian")
librarian::shelf(neonUtilities, neonOS, raster, tidyverse)

# Create a folder for local preservation of NEON data
dir.create("data", showWarnings = F)

# Clear environment
rm(list = ls())

# Set global option to *not* convert character variables to factors
options(stringsAsFactors = F)

# Load in data
apchem <- neonUtilities::loadByProduct(dpID = "DP1.20063.001", 
                                     site = c("PRLA", "SUGG", "TOOK"), 
                                     package = "expanded", check.size = T)
## NOTE: You can use `neonUtilities::stackByTable` to load a pre-downloaded .zip file

# `loadByProduct` returns a list
names(apchem)
dplyr::glimpse(apchem$apl_plantExternalLabDataPerSample)

# Can create separate objects for each dataframe in that list
list2env(apchem, .GlobalEnv)

# Export some of these dataframes as CSVs for later use
write.csv(apl_clipHarvest, file.path("data", "apl_clipHarvest.csv"), row.names = F)
write.csv(apl_biomass, file.path("data", "apl_biomass.csv"), row.names = F)
write.csv(apl_plantExternalLabDataPerSample, 
          file.path("data", "apl_plantExternalLabDataPerSample.csv"), row.names = F)
write.csv(variables_20063, file.path("data", "variables_20063.csv"), row.names = F)

# Navigate downloads
par30 <- neonUtilities::readTableNEON(
  dataFile = file.path("data", "NEON_par", "stackedFiles", "PARPAR_30min.csv"), 
  varFile = file.path("data", "NEON_par", "stackedFiles", "variables_00024.csv"))

# Load variables information
parvar <- read.csv(file.path("data", "NEON_par", "stackedFiles", "variables_00024.csv"))
dplyr::glimpse(parvar)

# Create a plot of PAR
plot(PARMean ~ startDateTime, 
     data = subset(par30, verticalPosition == "020"),
     type = "l")

# Plot plant isotope ratios
boxplot(analyteConcentration ~ siteID, 
        data = apl_plantExternalLabDataPerSample, 
        subset = analyte == "d13C",
        xlab = "Site", ylab = "d13C")

# Also NEON has made its own joining function
apct <- neonOS::joinTableNEON(apl_biomass, apl_plantExternalLabDataPerSample)

# Now can plot information across several files
boxplot(analyteConcentration ~ scientificName, 
        data = apct, 
        subset = analyte == "d13C", 
        xlab = NA, ylab = "d13C", 
        las = 2, cex.axis = 0.7)

# Download remote sensing data too
neonUtilities::byTileAOP(dpID = "DP3.30015.001", site = "WREF", year = "2017",
                         check.size = T, easting = 580000, northing = 5075000,
                         savepath = file.path("data"))

# Read in the raster of that remote sensing data
chm <- raster::raster(x = file.path("data", "DP3.30015.001", "neon-aop-products",
                                    "2017", "FullSite", "D16", "2017_WREF_1", "L3", 
                                    "DiscreteLidar", "CanopyHeightModelGtif", 
                                    "NEON_D16_WREF_DP3_580000_5075000_CHM.tif"))

# Check out that remote sensing data
plot(chm, col = topo.colors(6))

# End of tutorial; clear environment
rm(list = ls())

## -------------------------------------------- ##
            # NEON Tutorial No. 2 ----
## -------------------------------------------- ##

# Load libraries
# install.packages("librarian")
librarian::shelf(neonUtilities, neonOS, tidyverse)

# Clear environment
rm(list = ls())

# Link to data on NEON portal that the workshop focused on
## https://data.neonscience.org/data-products/DP1.10072.001

# Download data (as a list)
mamdat <- loadByProduct(dpID = "DP1.10072.001",
                        site = c("SCBI", "KONZ", "UNDE"),
                        package = 'basic', check.size = F,
                        startdate = "2021-01",
                        enddate = "2022-12")

# Look at contents
names(mamdat)

# Make the list into separate objects
list2env(x = mamdat, envir = .GlobalEnv)

# Check for duplicates per plot
mam_plotNight_nodups <- neonOS::removeDups(data = mam_perplotnight,
                                           variables = variables_10072,
                                           table = "mam_perplotnight")

# Check for duplicates per *trapping night*
mam_trapNight_nodups <- neonOS::removeDups(data = mam_pertrapnight,
                                           variables = variables_10072,
                                           table = "mam_pertrapnight")

# Join these two sets
mamjn <- neonOS::joinTableNEON(mam_plotNight_nodups,
                               mam_trapNight_nodups,
                               name1 = "mam_perplotnight",
                               name2 = "mam_pertrapnight")

# Glimpse it
dplyr::glimpse(mamjn)

# Check for bad rows
mam_trapNight_nodups %>%
  # Tag ID is included
  dplyr::filter(!is.na(tagID)) %>%
  # But not listed as a successful capture
  dplyr::filter(!grepl("capture", trapStatus))

# Get a taxon list
mam_list <- neonOS::getTaxonList(taxonType = "SMALL_MAMMAL",
                                 recordReturnLimit = 1000,
                                 verbose = T)

# Check out the taxon list
names(mam_list)

# Identify only target taxa info
## 'small mammal' data product may include other things that happened to get caught in traps but aren't technically "small mammals"
targetTaxa <- mam_list %>%
  dplyr::filter(taxonProtocolCategory == "target") %>%
  dplyr::select(taxonID, scientificName)

# Use that set of target taxa to filter the actual data
captures <- mamjn %>%
  # Filter to only successful captures of target taxa
  dplyr::filter(grepl("capture", trapStatus) &
                  taxonID %in% targetTaxa$taxonID) %>%
  # Pare down to only needed columns
  dplyr::select(nightuid, plotID, collectDate.x, tagID, taxonID, eventID) %>%
  # Remove the ".x" of one of the column names
  dplyr::rename(collectDate = collectDate.x)

# Glimpse it
dplyr::glimpse(captures)

# Generate a column of all of the unique tagIDs included in the dataset
uTags <- captures %>% 
  dplyr::select(tagID) %>% 
  dplyr::filter(!is.na(tagID)) %>% 
  dplyr::distinct()

# Create empty data frame to populate
capsNew <- dplyr::slice(captures, 0)

# For each tagged individual, add a record for each night of trapping done on the plots on which it was captured between the first and last dates of capture
for (i in uTags$tagID){
  # Identify an individual
  indiv <- captures %>% 
    dplyr::filter(tagID == i)
  
  # Get first/last dates of capture for that individual
  firstCap <- as.Date(min(indiv$collectDate), "YYYY-MM-DD", tz = "UTC")
  lastCap <- as.Date(max(indiv$collectDate), "YYYY-MM-DD", tz = "UTC")
  
  # Identify all dates between those caps
  possibleDates <- seq(as.Date(firstCap), as.Date(lastCap), by = "days")
  
  # Filter the data to the possible dates that focal individual can be found
  potentialNights <- mam_plotNight_nodups %>% 
    dplyr::filter(as.character(collectDate) %in% 
             as.character(possibleDates) & plotID %in% indiv$plotID) %>% 
    dplyr::select(nightuid,plotID, collectDate, eventID) %>% 
    dplyr::mutate(tagID = i)
  
  # Attach the potential nights to the subset of the data that relates to that individual
  allnights <- dplyr::left_join(potentialNights, indiv)
  
  # Attach taxon ID
  allnights$taxonID <- unique(indiv$taxonID)[1]
  ## Note that taxonID sometimes changes between recaptures
  ## This uses only the first identification but there are other ways of handlng this
  
  # Bind each of these created dataframes together by row
  capsNew <- dplyr::bind_rows(capsNew, allnights) }

# Check for untagged individuals and add back to the dataset if necessary:
captures %>% 
  dplyr::filter(is.na(tagID))

# Check for the minimum number known alive (MNKA)
## They wrote this function to do that
mnka_per_site <- function(capture_data) {
  mnka_by_plot_bout <- capture_data %>% 
    dplyr::group_by(eventID,plotID) %>% 
    dplyr::summarize(n=n_distinct(tagID))
  mean_mnka_by_site_bout <- mnka_by_plot_bout %>% 
    dplyr::mutate(siteID = substr(plotID, 1, 4)) %>%
    dplyr::group_by(siteID, eventID) %>% 
    dplyr::summarise(meanMNKA = mean(n))
  return(mean_mnka_by_site_bout)
}

# Use the function
(MNKA <- mnka_per_site(capture_data = capsNew))

# Empty dataframe to store things in alter
MNKAbysp <- data.frame()

# Identify unique species
splist <- unique(capsNew$taxonID)

# To estimate the minimum number known alive for each species at each bout and site it is possible to loop through and run the function for each taxonID
for(i in 1:length(splist)){
  taxsub <- capsNew %>% 
    dplyr::filter(taxonID %in% splist[i]) %>% 
    dplyr::mutate(taxonID = splist[i])
  MNKAtax <- mnka_per_site(taxsub) %>% 
    dplyr::mutate(taxonID=splist[i], Year = substr(eventID,6,9))
  MNKAbysp <- rbind(MNKAbysp,MNKAtax)
}

# Next we will visualize the abundance flucutations for Peromyscus leucopus through time:
MNKA_PE <- MNKAbysp %>% 
  dplyr::filter(taxonID %in% "PELE")

# Create a dataframe with the first date of collection for each bout to use as the date variable when plotting
datedf <- mam_plotNight_nodups %>% 
  dplyr::select(eventID, collectDate) %>% 
  dplyr::group_by(eventID) %>%
  dplyr::summarise(Date = min(collectDate)) %>%
  dplyr::mutate(Year = substr(Date, 1, 4), 
                MMDD = substr(Date, 6, 10))

# Join those
MNKA_PE <- dplyr::left_join(MNKA_PE, datedf)

# Make a ggplot!
ggplot(data = MNKA_PE, aes(x = MMDD, y = meanMNKA, color = Year, group = Year)) +
  geom_point() +
  geom_line()+
  facet_wrap(. ~ siteID) +
  theme(axis.text.x = element_text(angle = 90))

# Now make a dataframe of maximum for all taxa
TaxDat <- MNKAbysp %>% 
  dplyr::group_by(taxonID, siteID) %>% 
  dplyr::summarise(max = max(meanMNKA))

# Graph these taxa maximums
ggplot(TaxDat, aes(x = taxonID, y = max, fill = taxonID)) + 
  geom_bar(stat = "identity") +
  facet_wrap(. ~ siteID, scales = 'free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Download the rodent pathogen data
rptdat <- loadByProduct(dpID = "DP1.10064.002", 
                        site = c("SCBI", "KONZ", "UNDE"),
                        package = "basic", check.size = F,
                        startdate = "2021-01",
                        enddate = "2022-12")

# Unlist to dataframes
list2env(rptdat, envir = .GlobalEnv)

# Check for and deal with any duplicates
rpt_pathres_nodups <- neonOS::removeDups(data = rpt2_pathogentesting,
                                         variables = variables_10064,
                                         table = 'rpt2_pathogentesting')

# Pare down both dataframes before joining them
rptdat.merge <- rpt_pathres_nodups %>% 
  dplyr::select(plotID, collectDate, sampleID, testPathogenName, testResult) %>%
  dplyr::mutate(Site = substr(plotID, 1, 4))
mamdat.merge <- mam_trapNight_nodups %>% 
  dplyr::select(taxonID, bloodSampleID, earSampleID)

# Split rodent pathogen data by ear vs. blood
rptear <- rptdat.merge %>% 
  dplyr::filter(grepl('.E', sampleID, fixed = T))
rptblood <- rptdat.merge %>% 
  dplyr::filter(grepl('.B', sampleID, fixed = T))

# Join each sample type with the correct column from the mammal trapping data
rptear.j <- dplyr::left_join(rptear, mamdat.merge, 
                             by = c("sampleID" = "earSampleID"))
rptblood.j <- dplyr::left_join(rptblood, mamdat.merge, 
                               by = c("sampleID" = "bloodSampleID"))

# Now recombine them
rptall <- dplyr::bind_rows(rptear.j[,-8], rptblood.j[,-8])

# Glimpse it
dplyr::glimpse(rptall)

# Prepare a summarized version for plotting
rptprev <- rptall %>%
  dplyr::group_by(Site, testPathogenName, taxonID) %>% 
  dplyr::summarise(tot.test = dplyr::n(),
                   tot.pos = sum(testResult == 'Positive')) %>%
  dplyr::mutate(prevalence = tot.pos / tot.test)

# Barplot of prevalence by site and pathogen name
ggplot(rptprev, aes(x = testPathogenName, y = prevalence, fill = testPathogenName)) + 
  geom_bar(stat = "identity")+
  facet_wrap(. ~ Site) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Check out just one site that seems to higher prevalence of pathogens
rptprev %>% 
  dplyr::filter(Site %in% 'SCBI') %>%
  ggplot(aes(x = testPathogenName, y = prevalence, fill = testPathogenName)) + 
  geom_bar(stat = "identity")+
  facet_wrap(. ~ taxonID) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# End ----
