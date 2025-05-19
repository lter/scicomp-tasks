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
librarian::shelf(tidyverse, googledrive, purrr, supportR)

# Clear environment
rm(list = ls())

# Authorize googledrive
googledrive::drive_auth()

## ------------------------------ ##
      # Data Acquisition ----
## ------------------------------ ##

# List all files in relevant Drive folder
site_drive <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/12rNziwHWrnBp-hjSraAqL-9M-eXi7MTT")) %>%
  # Filter to only desired files
  dplyr::filter(name %in% c("site-start-end-dates-only.csv"))

# Create a folder for these locally
dir.create(path = file.path("data"), showWarnings = F)

# Download both files
purrr::walk2(.x = site_drive$name, .y = site_drive$id,
             .f = ~ googledrive::drive_download(file = .y, overwrite = T,
                                                path = file.path("data", .x)))

## ------------------------------ ##
      # Initial Wrangling ----
## ------------------------------ ##

# Read in the site start/end dates
site_dates_v1 <- read.csv(file = file.path("data", "site-start-end-dates-only.csv")) %>%
  # Drop empty columns
  dplyr::select(-dplyr::starts_with("X"))

# Check structure
dplyr::glimpse(site_dates_v1)

# Load coordinate information (note this is downloaded separately not in R yet)
site_coords_v1 <- read.csv(file = file.path("data", "Site-list.csv")) %>%
  # Drop start/end date (they are only for a particular grant period)
  dplyr::select(-dplyr::ends_with(".date"))

# Check structure
dplyr::glimpse(site_coords_v1)

# Check difference between the two scripts' site abbreviations
supportR::diff_check(old = unique(site_dates_v1$Code), new = unique(site_coords_v1$Code))
## A few dates exist for 'sites' not found in the coordinate set (that's fine)

# Join coordinates to dates to preserve coordinate-lacking rows
sites_v1 <- site_dates_v1 %>%
  dplyr::left_join(y = site_coords_v1, by = c("Code"))

# Check structure
dplyr::glimpse(sites_v1)

## ------------------------------ ##
      # Final Wrangling ----
## ------------------------------ ##

# Do necessary wrangling to make a polished data object
sites_v2 <- sites_v1 %>%
  # Combine full site name
  dplyr::mutate(Name = dplyr::coalesce(Name.x, Name.y), 
                .before = dplyr::everything()) %>%
  # Drop old name columns
  dplyr::select(-Name.x, -Name.y) %>%
  # Remove unwanted 'sites'
  dplyr::filter(!Code %in% c("EDI", "NCO", "LNO")) %>%
  # Rename all columns for clarity + consistent casing
  dplyr::rename(name = Name, code = Code, habitat = Type,
                start_year = Start.date, end_year = End.date,
                lat = Latitude, lon = Longitude, grant = Grant..,
                site_url = Link, status = Status)

# Check a few columns specifically
sort(unique(sites_v2$code))

# Check structure generally
dplyr::glimpse(sites_v2)
## view(sites_v2) # View if needed

## ------------------------------ ##
          # Export ----
## ------------------------------ ##

# Export locally
write.csv(x = sites_v2, row.names = F, na = '',
          file = file.path("data", "site_faq_info.csv"))


# Upload to 'sites-information' folder of LNO Shared Drive
googledrive::drive_upload(media = file.path("data", "site_faq_info.csv"),
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1HSB6LWEbXrTCEzHppfmTHWGZSX5qa30B"), overwrite = T)


# End ----
