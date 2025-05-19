## -------------------------------------------------- ##
                 # LTER Site FAQ
## -------------------------------------------------- ##
# Script author(s): Nick J Lyon

# PURPOSE:
## Wrangle LTER site information and create frequently-requested products
## LTER = Long Term Ecological Research
## FAQ = Frequently Asked Questions

## ------------------------------ ##
        # Housekeeping ----
## ------------------------------ ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, purrr, supportR, sf)

# Clear environment
rm(list = ls())

# Authorize googledrive
googledrive::drive_auth()

# Create a folder for local export of outputs
dir.create(path = file.path("plots"), showWarnings = F)

## ------------------------------ ##
      # Data Acquisition ----
## ------------------------------ ##

# List all files in relevant Drive folder
site_drive <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1HSB6LWEbXrTCEzHppfmTHWGZSX5qa30B")) %>%
  # Filter to only desired files
  dplyr::filter(name %in% c("site_faq_info.csv"))

# Create a folder for these locally
dir.create(path = file.path("data"), showWarnings = F)

# Download both files
purrr::walk2(.x = site_drive$name, .y = site_drive$id,
             .f = ~ googledrive::drive_download(file = .y, overwrite = T,
                                                path = file.path("data", .x)))

# Read in data
site_df <- read.csv(file = file.path("data", "site_faq_info.csv")) %>%
  # Rename site name column
  dplyr::rename(site_name = name)

# Check structure
dplyr::glimpse(site_df)

## ------------------------------ ##
            # Filter ----
## ------------------------------ ##

# Perform desired filtering
site_actual <- site_df %>%
  dplyr::filter(habitat %in% c("Marine", "Coastal")) # Marine Consumer Nutrient Dynamic request

## ------------------------------ ##
# Site Timeline ----
## ------------------------------ ##

# Flip site data to long format
site_long <- site_actual %>%
  tidyr::pivot_longer(cols = ends_with("_year"),
                      values_to = "year")

# Check structure
dplyr::glimpse(site_long)

# Get vector of habitat colors
habitat_colors <- c("Coastal" = "#34a0a4", 
                    "Freshwater" = "#48cae4", 
                    "Marine" = "#1e6091", 
                    "Forest" = "#007200",
                    "Grassland" = "#9ef01a", 
                    "Mixed Landscape" = "#7f5539",
                    "Tundra" = "#bb9457", 
                    "Urban" = "#9d4edd")

# Make timeline
timeline <- ggplot(site_long, aes(x = year, y = code)) +
  geom_path(aes(group = code, color = habitat), lwd = 1, lineend = 'round') +
  geom_point(aes(fill = habitat), pch = 21, size = 2) +
  # Custom color
  scale_fill_manual(values = habitat_colors) +
  scale_color_manual(values = habitat_colors) +
  # Customize theme elements
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_blank()); timeline

# Export this graph
ggsave(plot = timeline, filename = file.path("plots", "LTER_site_timeline.png"),
       height = 5, width = 6, units = "in")

## ------------------------------ ##
          # Site Map ----
## ------------------------------ ##

# Identify min & max latitude / longitude
(min_lat <- min(site_actual$lat, na.rm = T))
(max_lat <- max(site_actual$lat, na.rm = T))
(min_lon <- min(site_actual$lon, na.rm = T))
(max_lon <- max(site_actual$lon, na.rm = T))

# Define latitude / longitude limits
(lat_lims <- c((min_lat - 0.1 * min_lat), (max_lat + 0.1 * max_lat)))
(lon_lims <- c((min_lon + 0.15 * min_lon), (max_lon - 0.15 * max_lon)))

# Get an object of world / US state borders
borders <- sf::st_as_sf(maps::map(database = "world", plot = F, fill = T)) %>%
  dplyr::bind_rows(sf::st_as_sf(maps::map(database = "state", plot = F, fill = T)))

# Make map
map <- borders %>%
  ggplot() +
  geom_sf(fill = "gray95") +
  # Set map extent
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = F) +
  # Add points & labels for LTER sites
  geom_point(data = site_actual, aes(x = lon, y = lat, fill = habitat), pch = 21, size = 4) +
  geom_label(data = site_actual, aes(x = lon, y = lat),
             label = site_actual$code, nudge_y = 0, nudge_x = 5, size = 3, fontface = "bold", 
             label.padding = unit(x = 0.15, units = "lines")) +
  # Customize color
  scale_fill_manual(values = habitat_colors) +
  # Customize axis labels
  labs(x = "Longitude", y = "Latitude") +
  # Tweak theme / formatting
  theme_bw() + 
  theme(panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_blank()); map
  
# Export the map
ggsave(plot = map, filename = file.path("plots", "LTER_site_map.png"),
       height = 8, width = 8, units = "in")

# End ----
