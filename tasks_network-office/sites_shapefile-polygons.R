## -------------------------------------------------- ##
                  # LTER Site Polygons
## -------------------------------------------------- ##
# Script author(s): Nick J Lyon

# PURPOSE:
## Wrangle LTER site polygons into a single shapefile

## ------------------------------ ##
        # Housekeeping ----
## ------------------------------ ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, googledrive, sf, maps, geojsonio, supportR)

# Make needed folder(s)
dir.create(path = file.path("graphs"), showWarnings = F)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "site-polys_2024"), showWarnings = F)

# Turn off S2 Processing
sf::sf_use_s2(FALSE)

# Clear environment
rm(list = ls())

# Load needed utilities
source(file.path("tools", "lter_site-poly-utils.R"))

## ------------------------------ ##
    # Initial Exploration ----
## ------------------------------ ##

# Read in 2017 shapefiles
lter_v1 <- sf::st_read(dsn = file.path("data", "lterDomains_110410.shp"))

# Check contents
dplyr::glimpse(lter_v1)

# How many sites are included / which of them?
sort(unique(lter_v1$SITE)); length(unique(lter_v1$SITE))

# Check CRS
sf::st_crs(lter_v1)

## ------------------------------ ##
          # Site Prep ----
## ------------------------------ ##

# Assemble a dataframe of site-specific information
## Lets subsequent wrangling be a flexible loop
site_faq <- as.data.frame(rbind(
  c("ARC", "Arctic", "ARCLTER_bounday_2024.geojson"),
  c("BLE", "Beaufort Lagoons Ecosystems", "ble_lagoons_polygons.shp"),
  c("CDR", "Cedar Creek", "CDR_Border.shp"),
  c("FCE", "Florida Coastal Everglades", "FCE_study_area_2022.shp"),
  c("HFR", "Harvard Forest", "HFR-bbox.geojson"),
  ## KNZ available here: https://maps-konza.hub.arcgis.com/datasets/53fdc788cabf445e887d719584a3fd43_0/explore?location=39.100986%2C-96.576658%2C12.59
  c("KNZ", "Konza Prairie", "Konza_Prairie_Biological_Station_boundary_from_1977_until_2021_(GIS002).shp"), 
  c("MCM", "McMurdo Dry Valleys", "APAD_ID_75.shp"),
  c("MCR", "Moorea Coral Reef", "MCR-boundary.geojson"),
  ## MSP available here: https://deims.org/dc6949fb-2771-4e31-8279-cdb0489842f0
  c("MSP", "Minneapolis-St.Paul", "msp_deims_sites_boundariesPolygon.shp"),
  c("NES", "Northeast U.S. Shelf", "EPU_extended.shp"),
  c("NGA", "Northern Gulf of Alaska", "nga_bb.geojson")
  # c("", "", ""),
)) %>% 
  # Rename columns more informatively
  dplyr::rename(site_code = V1,
                site_name = V2,
                file_name = V3)

# Check that out
head(site_faq)

## ------------------------------ ##
      # Site Wrangling ----
## ------------------------------ ##

# Make a variant of the Network-wide object to avoid damaging the original version
lter_v2 <- lter_v1

# Loop across sites in the site FAQ object assembled above
for(k in 1:nrow(site_faq)){
  
  # Identify focal row of site FAQ
  focal_info <- site_faq[k, ]
  
  # Processing message
  message("Processing begun for ", focal_info$site_code, " boundary")
  
  # Read in the data (conditional depending on file extension)
  ## Shapefiles
  if(stringr::str_detect(string = focal_info$file_name, 
                         pattern = "\\.shp") == TRUE){
    
    # Read in shapefile
    site_v1 <- sf::st_read(dsn = file.path("data", focal_info$file_name))
    
    ## GeoJSONs
  } else if(stringr::str_detect(string = focal_info$file_name, 
                              pattern = "\\.geojson") == TRUE){
    
    # Read in GeoJSON
    site_v1 <- geojsonio::geojson_read(x = file.path("data", focal_info$file_name),
                            what = "sp") %>%
      # Transform into simple features object
      sf::st_as_sf(x = .)
    
    ## Unrecognized file (need to build into this conditional framework!)
  } else { stop("Error: unrecognized input file type!") }
  
  # Do wrangling
  site_v2 <- poly_tidy(site_sf = site_v1, network_sf = lter_v1,
                      code = focal_info$site_code, name = focal_info$site_name, 
                      plot = F)
  
  # Handle special site-specific wrangling
  ## CDR -- wrong type of polygon
  if(focal_info$site_code == "CDR"){
    site_v2 %<>%
      sf::st_polygonize()
  }
  
  # Wrangle the network-wide polygon
  lter_v2 %<>%
    # Remove the old version of this site
    dplyr::filter(!SITE %in% focal_info$site_code) %>% 
    # Attach the newly wrangled version of this site
    dplyr::bind_rows(site_v2)
  
}

# Check for lost/gained sites
supportR::diff_check(old = unique(lter_v1$SITE), 
                     new = unique(lter_v2$SITE))

# Check structure generally
dplyr::glimpse(lter_v2)

## ------------------------------ ##
      # Final Processing ----
## ------------------------------ ##

# Final (actual) tidying
lter_final <- lter_v2 %>% 
  # Fix some incorrect full site names
  dplyr::mutate(NAME = dplyr::case_when(
    SITE == "BES" ~ "Baltimore Ecosystem Study",
    SITE == "BNZ" ~ "Bonanza Creek",
    SITE == "JRN" ~ "Jornada Basin",
    SITE == "NTL" ~ "North Temperate Lakes",
    # SITE == "" ~ "",
    T ~ NAME)) %>% 
  # Summarize geometry info
  dplyr::group_by(SITE, NAME) %>% 
  dplyr::summarize(geometry = sf::st_union(geometry),
                   .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Order alphabetically
  dplyr::arrange(SITE)

# Check structure
dplyr::glimpse(lter_final)
## View(lter_final)

# Check the final spatial extent
sf::st_bbox(lter_final)

## ------------------------------ ##
            # Export ----
## ------------------------------ ##

# Generate a file name / path
poly_name <- file.path("data", "site-polys_2024", "lter_site-boundaries.shp")

# Export locally
sf::st_write(obj = lter_final, dsn = poly_name, delete_layer = T)

# Generate a CSV name
poly_csv <- gsub(pattern = "boundaries.shp", replacement = "names.csv", x = poly_name)

# Drop the geometry information
lter_csv <- sf::st_drop_geometry(x = lter_final)

# Check structure
dplyr::glimpse(lter_csv)

# Export that too
write.csv(x = lter_csv, file = poly_csv, row.names = F, na = '')

## ------------------------------ ##
    # Per-Site Map Making ----
## ------------------------------ ##

# Clear environment of everything that is not needed
rm(list = setdiff(ls(), c("lter_final")))

# Get state / country borders
borders <- dplyr::bind_rows(sf::st_as_sf(maps::map(database = "world", plot = F, fill = T)),
                            sf::st_as_sf(maps::map(database = "state", plot = F, fill = T)))

# Define a coordinate cutoff (in degrees)
coord_cutoff_lat <- 2.25
coord_cutoff_lon <- 3.5

# Loop across sites
for(one_name in sort(unique(lter_final$SITE))){
  
  # Processing message
  message("Creating map for LTER site: ", one_name)
  
  # Subset to a single site
  one_site <- dplyr::filter(lter_final, SITE == one_name)
  
  # Cast to "POINT" type
  one_pts <- suppressWarnings(sf::st_cast(x = one_site, to = "POINT"))
  
  # Strip as a dataframe
  one_df <- as.data.frame(unique(sf::st_coordinates(x = one_pts)))
  
  # Identify min/max coordinates
  one_box <- data.frame("max_lat" = max(one_df$Y), "min_lat" = min(one_df$Y),
                        "max_lon" = max(one_df$X), "min_lon" = min(one_df$X)) %>% 
    # Calculate range for both
    dplyr::mutate(rng_lat = abs(max_lat - min_lat),
                  rng_lon = abs(max_lon - min_lon)) %>% 
    # Bump up those values if they're beneath a threshold
    dplyr::mutate(rng_lat = ifelse(rng_lat < coord_cutoff_lat, 
                                   yes = coord_cutoff_lat, no = rng_lat),
                  rng_lon = ifelse(rng_lon < coord_cutoff_lon, 
                                   yes = coord_cutoff_lon, no = rng_lon)) %>% 
    # Now use it to identify more reasonable limits
    dplyr::mutate(top = ifelse(max_lat > 0, 
                               yes = max_lat + rng_lat, 
                               no = max_lat - rng_lat),
                  bottom = ifelse(min_lat > 0, 
                                  yes = min_lat - rng_lat, 
                                  no = min_lat + rng_lat),
                  left = ifelse(max_lon > 0, 
                                yes = max_lon + rng_lon, 
                                no = max_lon - rng_lon),
                  right = ifelse(min_lon > 0, 
                                 yes = min_lon - rng_lon, 
                                 no = min_lon + rng_lon))
  
  # Graph this site
  ggplot() +
    # Add country/state borders
    geom_sf(data = borders, fill = "white") +
    # Add site polygons
    geom_sf(data = one_site, aes(fill = SITE), alpha = 0.7) +
    # Define borders
    coord_sf(xlim = c(one_box$left, one_box$right), 
             ylim = c(one_box$top, one_box$bottom)) +
    # Customize legend / axis elements
    labs(x = "Longitude", y = "Latitude",
         title = paste0(one_name, " Boundary")) +
    supportR::theme_lyon() + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 35, hjust = 1))
  
  # Assemble file name / path
  one_file <- file.path("graphs", paste0("lter-site-polygon_", one_name, "_2024.png"))
  
  # Export
  ggsave(filename = one_file, width = 5, height = 5, units = "in")
  
} # Close loop

# Identify exported map file names
lter_maps <- dir(path = file.path("graphs"), pattern = "lter-site-polygon_")

# Send these files to the Google Drive
purrr::walk(.x = lter_maps, 
            .f = ~ googledrive::drive_upload(media = file.path("graphs", .x), overwrite = T,
                                             path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1mqhSuYgun-OA_50ET2vD-u9niCb9f6-R")))

# End ----
