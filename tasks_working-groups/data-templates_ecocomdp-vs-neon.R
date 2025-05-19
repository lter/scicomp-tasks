## --------------------------------------------- ##
              # Exploring `ecocomDP`
## --------------------------------------------- ##
# Follows this tutorial:
# https://www.neonscience.org/resources/learning-hub/tutorials/neon-biodiversity-ecocomdp-cyverse

# Clear environment
rm(list = ls())

## --------------------------------------------- ##
              # Housekeeping -----
## --------------------------------------------- ##
# Load needed packages
# install.packages("librarian")
librarian::shelf(neonUtilities, tidyverse, ecocomDP, vegan)

# Load NEON token
source(file.path("wg_emergent", "neon_token.R"))
## "neon_token.R" creates "neon_token" object of the NEON token string

# Check token loading
if(!exists("neon_token")){
  message("NEON token *NOT* found! Attach token before continuing")
} else { message("NEON token found! Continue") }

## --------------------------------------------- ##
             # NEON Data Download -----
## --------------------------------------------- ##
# Load some NEON invertebrate data
## Takes ~1 minute
all_tabs_inv <- neonUtilities::loadByProduct(
  dpID = "DP1.20120.001", # NEON aquatic macroinvert data
  site = c("COMO","HOPB"), # NEON sites
  startdate = "2017-01", # start year-month
  enddate = "2019-12", # end year-month
  token = neon_token, # use NEON_TOKEN environmental variable
  check.size = F) # proceed with download regardless of file size

# Check tables within larger data product
names(all_tabs_inv)

# extract items from list and put in R env. 
all_tabs_inv %>%
  list2env(.GlobalEnv)

# "variables" describes metadata
dplyr::glimpse(variables_20120)

# "validation" contains rules for constraining ingestion to NEON
dplyr::glimpse(validation_20120)

# "categoricalCodes" contains controlled lists used in data
dplyr::glimpse(categoricalCodes_20120)

# Download taxon table information as well
## Takes 1-2 minutes
full_taxon_table_from_api <- neonUtilities::getTaxonTable("MACROINVERTEBRATE", token = neon_token)

## --------------------------------------------- ##
            # NEON Data Wrangling -----
## --------------------------------------------- ##
# Remove duplicate IDs from data
de_duped_uids <- inv_fieldData %>% 
  # Remove records where no sample was collected
  filter(!is.na(sampleID)) %>%  
  # Then summarize across duplicate values
  group_by(sampleID) %>%
  summarise(n_recs = length(uid),
            n_unique_uids = length(unique(uid)),
            uid_to_keep = dplyr::first(uid)) 

# Are there any records that have more than one unique uid?
max_dups <- max(de_duped_uids$n_unique_uids %>% unique())

# Filter data using de-duped uids if they exist
if(max_dups > 1){
  inv_fieldData <- inv_fieldData %>%
    dplyr::filter(uid %in% de_duped_uids$uid_to_keep)}

# Extract year from date, add it as a new column
inv_fieldData <- inv_fieldData %>%
  dplyr::mutate(
    year = lubridate::year(lubridate::as_date(collectDate)))

# Extract location data into a separate table
table_location <- inv_fieldData %>%
  # keep only the columns listed below
  dplyr::select(siteID, domainID, namedLocation, decimalLatitude,
                decimalLongitude, elevation) %>%
  # Keep rows with unique combinations of values (no dups)
  dplyr::distinct()

# Create taxon table describing taxonIDs in data
# Start with inv_taxonomyProcessed
table_taxon <- inv_taxonomyProcessed %>%
  # Keep only the coluns listed below
  dplyr::select(acceptedTaxonID, taxonRank, scientificName,
                order, family, genus, 
                identificationQualifier,
                identificationReferences) %>%
  # Remove rows with duplicate information
  dplyr::distinct()

# Check for repeated taxa that need summarizing
inv_taxonomyProcessed_summed <- inv_taxonomyProcessed %>% 
  dplyr::select(sampleID, acceptedTaxonID,
                individualCount, estimatedTotalCount) %>%
  dplyr::group_by(sampleID, acceptedTaxonID) %>%
  dplyr::summarize(
    dplyr::across(c(individualCount, estimatedTotalCount),
                  ~ sum(.x, na.rm = TRUE)))

# Join summed taxon counts back with sample and field data
table_observation <- inv_taxonomyProcessed_summed %>%
  # Join relevant sample info back in by sampleID
  dplyr::left_join(inv_taxonomyProcessed %>% 
                     dplyr::select(sampleID, domainID, siteID,
                                   namedLocation, collectDate,
                                   acceptedTaxonID,
                                   order, family, genus, 
                                   scientificName, taxonRank) %>%
                     dplyr::distinct()) %>%
  # Join the columns selected above with some from inv_fieldData
  dplyr::left_join(inv_fieldData %>% 
                     dplyr::select(sampleID, eventID, year, 
                                   habitatType, samplerType,
                                   benthicArea)) %>%
  # Calculate some new columns
  dplyr::mutate(inv_dens = estimatedTotalCount / benthicArea,
                inv_dens_unit = 'count per square meter')

# Check for duplicate records, should return a table with 0 rows
table_observation %>% 
  dplyr::group_by(sampleID, acceptedTaxonID) %>% 
  dplyr::summarize(n_obs = length(sampleID)) %>%
  dplyr::filter(n_obs > 1)

# Extract sample info
table_sample_info <- table_observation %>%
  dplyr::select(sampleID, domainID, siteID, namedLocation, 
                collectDate, eventID, year, habitatType,
                samplerType, benthicArea, inv_dens_unit) %>%
  dplyr::distinct()

# Create an occurrence summary table
taxa_occurrence_summary <- table_observation %>%
  dplyr::select(sampleID, acceptedTaxonID) %>%
  dplyr::distinct() %>%
  dplyr::group_by(acceptedTaxonID) %>%
  dplyr::summarize(occurrences = n())

# Make some summary data
sampling_effort_summary <- table_sample_info %>%
  # group by siteID, year
  dplyr::group_by(siteID, year, samplerType) %>%
  # count samples and habitat types within each event
  dplyr::summarise(
    event_count = length(unique(eventID)),
    sample_count = length(unique(sampleID)),
    habitat_count = length(unique(habitatType)))

# Check out the summary table
sampling_effort_summary %>%
  as.data.frame() %>% 
  # dplyr::glimpse()
  head()

## --------------------------------------------- ##
          # Visualize Wrangled Data -----
## --------------------------------------------- ##
# Number taxa by rank by site
table_observation %>% 
  dplyr::group_by(domainID, siteID, taxonRank) %>%
  dplyr::summarize(n_taxa = length(unique(acceptedTaxonID))) %>%
  ggplot(aes(n_taxa, taxonRank)) +
    facet_wrap(. ~ domainID + siteID) +
    geom_col()

# Select only site by species density info and remove duplicate records
table_sample_by_taxon_density_long <- table_observation %>%
  dplyr::select(sampleID, acceptedTaxonID, inv_dens) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(inv_dens))

# Pivot to wide format, sum multiple counts per sampleID
table_sample_by_taxon_density_wide <- table_sample_by_taxon_density_long %>%
  tidyr::pivot_wider(id_cols = sampleID, 
                     names_from = acceptedTaxonID,
                     values_from = inv_dens,
                     values_fill = list(inv_dens = 0),
                     values_fn = list(inv_dens = sum)) %>%
  tibble::column_to_rownames(var = "sampleID") 

# check col and row sums -- mins should all be > 0
base::colSums(table_sample_by_taxon_density_wide) %>% min()
base::rowSums(table_sample_by_taxon_density_wide) %>% min()

## --------------------------------------------- ##
        # Downloading `ecocomDP` Data -----
## --------------------------------------------- ##
# Search for data sets with periphyton or algae
search_result <- ecocomDP::search_data(text = "periphyt|algae")
dplyr::glimpse(search_result)

# Search for invertebrate data products
search_result <- ecocomDP::search_data(text = "invertebrate")
dplyr::glimpse(search_result)

# Once identified, can download!
data_neon_inv <- ecocomDP::read_data(
  id = "neon.ecocomdp.20120.001.001",
  site = c("COMO","HOPB"), # NEON sites
  startdate = "2017-01", # start year-month
  enddate = "2019-12", # end year-month
  token = neon_token,
  check.size = F)

# Check structure of returned data
names(data_neon_inv)

# Check data package ID
data_neon_inv$id

# Short list of package summary data
data_neon_inv$metadata$data_package_info

# Validation issues?
data_neon_inv$validation_issues
## If "list()", doesn't have any issues

# Examine the tables
names(data_neon_inv$tables)

# Examine taxon table
dplyr::glimpse(data_neon_inv$tables$taxon)

# Examine observation table
dplyr::glimpse(data_neon_inv$tables$observation)

## --------------------------------------------- ##
          # `ecocomDP` Visual Tools -----
## --------------------------------------------- ##
# Explore spatiotemporal coverage of data
ecocomDP::plot_sample_space_time(data_neon_inv)

# Explore taxonomic resolution of data
ecocomDP::plot_taxa_rank(data_neon_inv)

## --------------------------------------------- ##
        # `ecocomDP` Wrangling Tools -----
## --------------------------------------------- ##
# Combine all data tables into single, "flat" table
flat_neon_inv <- data_neon_inv %>% 
  ecocomDP::flatten_data() %>%
  dplyr::mutate(id = data_neon_inv$id) 
dplyr::glimpse(flat_neon_inv)

# Compare taxa and abundances across sampler types
flat_neon_inv %>% 
  ecocomDP::plot_taxa_abund(trans = "log10",
                            min_relative_abundance = 0.01,
                            color_var = "samplerType")

# Pivot wider, event_id by taxon_id
wide_neon_inv <- data_neon_inv$tables$observation %>%
  tidyr::pivot_wider(id_cols = event_id,
                     names_from = taxon_id,
                     values_from = value,
                     values_fill = 0) %>%
  tibble::column_to_rownames("event_id")

# make sure now rows or columns sum to 0
min(rowSums(wide_neon_inv))
min(colSums(wide_neon_inv))

# Create ordination using metaMDS
my_nmds_result <- wide_neon_inv %>%
  vegan::metaMDS()

# Check stress
my_nmds_result$stress
## Anything under 0.15 is good (soft rule of thumb)

# Plot ordination
vegan::ordiplot(my_nmds_result)

## --------------------------------------------- ##
      # Compare NEON & `ecocomDP` Data ----
## --------------------------------------------- ##
# Download yet more data
## Could take ~10 min
data_neon_inv_allsites <- read_data(
  id = "neon.ecocomdp.20120.001.001",
  token = neon_token,
  release = "RELEASE-2022",
  check.size = FALSE)

# Flatten data
flat_neon_inv_allsites <- data_neon_inv_allsites %>%
  ecocomDP::flatten_data()

# Pull data for NTL aquatic macroinvertebrates to compare
## Takes ~1-2 min
data_ntl_inv <- ecocomDP::read_data(id = "edi.290.2")

# Flatten the dataset
flat_ntl_inv <- data_ntl_inv %>%
  ecocomDP::flatten_data() %>%
  # Briefly wrangle columns
  dplyr::mutate(package_id = data_ntl_inv$id,
                taxon_rank = tolower(taxon_rank))

dplyr::glimpse(flat_ntl_inv)

# Extensive data
names(flat_neon_inv_allsites)

# Check site types
unique(flat_neon_inv_allsites$aquaticSiteType)

# Filter data to only lakes and specific domain
flat_neon_inv_d05 <- flat_neon_inv_allsites %>%
  dplyr::filter(aquaticSiteType == "lake" &
                domainID == "D05")

# Stack two datasets
stacked_inv <- flat_neon_inv_d05 %>%
  dplyr::bind_rows(flat_ntl_inv) %>% 
  as.data.frame()

# Compare taxon ranks used in the two data sets
stacked_inv %>% 
  ecocomDP::plot_taxa_rank(facet_var = "package_id",
                           facet_scales = "free_x")

# Compare spatiotemporal resolution
stacked_inv %>%
  ecocomDP::plot_sample_space_time()

# Some dependencies are needed for map plotting (next step)
librarian::shelf(ggrepel, usmap, maptools, rgdal)

# Plot on US map
stacked_inv %>%
  ecocomDP::plot_sites()

# Explore richness (number of species) through time
stacked_inv %>% 
  ecocomDP::plot_taxa_diversity(time_window_size = "year") 

# Compare taxa & abundances
stacked_inv %>% 
  ecocomDP::plot_taxa_occur_freq(facet_var = "package_id")

# Plot common NEON taxa
flat_neon_inv_d05 %>% 
  ecocomDP::plot_taxa_occur_freq(min_occurrence = 100)

# Plot common NTL (LTER) taxa
flat_ntl_inv %>% 
  ecocomDP::plot_taxa_occur_freq(min_occurrence = 30)

# End ----
