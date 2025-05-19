## ------------------------------------------------------- ##
            # LTER Network Office - Zoom Webinars
## ------------------------------------------------------- ##
# Written by: Nick J Lyon

# Purpose
## Summarize synthesis group webinar attendance (total and by job sector)

# Load needed libraries
librarian::shelf(lterpalettefinder, scicomptools, tidyverse, googledrive, 
                 cowplot, scales)

# Create needed sub-folders
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "webinar_info"), showWarnings = F)
dir.create(file.path("plots"), showWarnings = F)

# Clear environment
rm(list = ls()); gc()

# Load needed tool(s)
source(file.path("tools", "zoom-fix.R"))

## ------------------------------------- ##
# Download Raw Data ----
## ------------------------------------- ##

# Identify file(s) of interest
gdrive_webinar <- dplyr::bind_rows(googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/17IzVpVMDOKzE_6lvt7TkFw4Jq8a1mvLz")),
                                   googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/17MZLpCr0OGcZEJwg66vS0FxyujtL-YX9")))

# Check that
gdrive_webinar

# Download everything
purrr::walk2(.x = gdrive_webinar$id, .y = gdrive_webinar$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "webinar_info", .y)))

# Separate attendance and survey info
local_att <- dir(file.path("data", "webinar_info"), pattern = "attendance")
local_qs <- dir(file.path("data", "webinar_info"), pattern = "survey")

## ------------------------------------- ##
# Load & Tidy Attendance ----
## ------------------------------------- ##
# Make an empty list
attendance_list <- list()

# Put each file in its own element of that list
for(file_name in local_att){
  attendance_list[[file_name]] <- zoom_fix(file_name = file.path("data", "webinar_info", file_name), report_type = "attendance")
}

# Wrangle the attendance files
attendance_df <- attendance_list %>%
  # Use `purrr` to apply the custom function to each file
  purrr::map_dfr(.f = select, dplyr::everything()) %>%
  # Identify job sector categories using email domains/suffixes
  dplyr::mutate(job_sector =  case_when(
    # Fix obvious ones
    ## Government
    domain == "gov" ~ "Government",
    ## University
    domain == "edu" & email != "nceas.ucsb" ~ "University",
    ## NGOs (incl. NEON and NCEAS)
    domain == "edu" & email == "nceas.ucsb" ~ "NGO", 
    email == "battelleecology" ~ "NGO", # NEON
    domain == "org" ~ "NGO",
    email == "catf" & domain == "us" ~ "NGO", # Clean Air Task Force
    # Make an international category
    domain == "ar" ~ "International", # Argentina
    domain == "ca" ~ "International", # Canada
    domain == "ch" ~ "International", # Switzerland
    domain == "de" ~ "International", # Germany 
    domain == "it" ~ "International", # Italy
    domain == "nz" ~ "International", # New Zealand
    domain == "se" ~ "International", # Sweden
    domain == "uk" ~ "International", # United Kingdom
    domain == "za" ~ "International", # South Africa
    # And create a grab bag for 'Other' (i.e., sector unidentifiable by email &/or not enough representation to warrant a new category)
    email == "yahoo" ~ "Other",
    email == "aol" ~ "Other",
    email == "gmail" ~ "Other",
    email == "leahwilson" ~ "Other",
    email == "msn" ~ "Other",
    email == "juno" ~ "Other",
    email == "outlook" ~ "Other",
    email == "icloud" ~ "Other",
    email == "live" ~ "Other",
    # And if the above conditions leave any out, warn yourself
    T ~ 'NEW, need to add to `case_when`' ) )

# Check if you need to add any new conditions (to `scicomptools`) to catch new emails
## "<0 rows> ..." means you are up to date
attendance_df %>%
  dplyr::filter(job_sector == 'NEW, need to add to `case_when`') %>%
  dplyr::select(email, domain) %>%
  unique() %>%
  dplyr::arrange(domain) %>%
  as.data.frame()

# Now summarize the data
attendance_ready <- attendance_df %>%
  # Group by needed factors
  dplyr::group_by(webinar_day, job_sector) %>%
  # Summarize
  dplyr::summarise(attendance = max(attendance),
            sector_ct = sum(suffix_ct, na.rm = T),
            .groups = "keep") %>%
  # Pivot wider
  tidyr::pivot_wider(id_cols = webinar_day:attendance,
              names_from = job_sector,
              values_from = sector_ct)

# Take a look
head(attendance_ready)

## ------------------------------------- ##
# Load & Tidy Reports ----
## ------------------------------------- ##
# Make an empty list
survey_list <- list()

# Put each file in its own element of that list
for(file_name in local_qs){
  survey_list[[file_name]] <- zoom_fix(file_name = file.path("data", "webinar_info", file_name), report_type = "survey")
}

# Wrangle the attendance files
survey_df <- survey_list %>%
  # Use `purrr` to apply the bind the columns together
  purrr::map_dfr(.f = select, dplyr::everything()) %>%
  # Split the "heard_where" column to account for multiple mechanisms of hearing
  tidyr::separate(col = heard_where,
                  into = c('heard_sub1', 'heard_sub2', 'heard_sub3'),
           fill = 'left', sep = ';') %>%
  # Now pivot longer to get back to a single "heard" column (effectively duplicating rows with multiple entries)
  tidyr::pivot_longer(cols = starts_with('heard_sub'), names_to = 'heard_temp', values_to = 'heard_where') %>%
  # Drop the temp column
  dplyr::select(-heard_temp) %>%
  # And drop the NAs introduced by the preceding operations
  dplyr::filter(!is.na(heard_where)) %>%
  # Group by webinar & participation
  group_by(webinar_day, participation) %>%
  # Count usefulness/heard_where
  dplyr::add_count(webinar_useful, name = "useful_freq") %>%
  dplyr::add_count(heard_where, name = "heard_freq") %>%
  # Drop unneeded columns
  dplyr::select(-heard_other_text, -feedback) %>%
  # Get only unique rows
  dplyr::distinct() %>%
  # Ungroup
  dplyr::ungroup()

# Examine
head(survey_df)

## ------------------------------------- ##
# Combine Data ----
## ------------------------------------- ##

# Before combination, read in the lookup table connecting day with topic
lookup <- readr::read_csv(file.path("data", "lter-wg_webinar-schedule.csv"), 
                          show_col_types = F)
str(lookup)

# Before combination, define some vectors to use as factor levels
utility_lvls <- c("No", "Kind of", "Yes")
advert_lvls <- c("LTER Newsletter", "LTER website",
                 "Ecolog", "Other listserv",
                 "Social Media", "Other")
sector_lvls <- c("University", "NGO", "Government", "International", "Other")
date_lvls <- c("Sep 15 2021",  "Oct 20 2021", "Nov 17 2021", "Jan 19 2022",
               "Feb 16 2022", "Mar 16 2022", "Apr 20 2022")
gp_lvls <- c("Drought Effects", "Soil Organic Matter", "EMERGENT", "Silica Synthesis", "Ecosystem Transitions",
             "Special Issue: Biosphere", "Plant Reproduction")

# Combine them
combo <- survey_df %>%
  # Attach attendance data to survey data
  dplyr::left_join(attendance_ready, by = "webinar_day") %>%
  # Bring over lookup table too
  dplyr::left_join(lookup, by = "webinar_day") %>%
  # Pivot the attendance information to long format
  tidyr::pivot_longer(cols = c(Government, International, NGO, Other, University),
               names_to = "sector", values_to = "sector_attendance") %>%
  # Re-organize
  dplyr::select(working_group, webinar_day, attendance, participation, webinar_useful, useful_freq, heard_where, heard_freq, sector, sector_attendance) %>%
  # Expand SOM abbreviation
  dplyr::mutate(working_group = case_when(working_group == "SOM" ~ "Soil Organic Matter",
                                   T ~ as.character(working_group) ) ) %>%
  # Calculate/create some needed columns
  dplyr::mutate(
    # Proportion of attendees who responded to the survey
    prop_survey = participation / attendance, .after = participation,
    # Factor and re-level grouping columns
    heard_where = factor(heard_where, levels = advert_lvls),
    sector = factor(sector, levels = sector_lvls),
    webinar_useful = factor(webinar_useful, levels = utility_lvls),
    working_group = factor(working_group, levels = gp_lvls) )
  
# Take a look
head(as.data.frame(combo))

## ------------------------------------- ##
# Graphics Housekeeping ----
## ------------------------------------- ##

# Create subsets of the data for each graphic
## Attendance
(headcount <- combo %>%
  select(working_group, attendance, participation, prop_survey) %>%
  unique())

## Where did you hear about the webinar?
(advert <- combo %>%
  select(working_group, heard_where, heard_freq) %>%
  unique())

## What job sector were attendees from?
(sector <- combo %>%
  select(working_group, sector, sector_attendance) %>%
  unique())

## Did you find the seminar useful?
(utility <- combo %>%
  select(working_group, webinar_useful, useful_freq) %>%
  unique())

# Create a custom theme
lno_theme <- theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size = 15),
        axis.text = element_text(size = 13))

# Identify some useful palettes
hc_palt <- lterpalettefinder::palette_find(name = "lter logo") %>%
  lterpalettefinder::palette_subsample(wanted = nrow(headcount))
ad_palt <- palette_find(name = "salamander") %>%
  palette_subsample(wanted = length(unique(advert$heard_where)))
sec_palt <- palette_find(name = "lotus") %>%
  palette_subsample(wanted = length(unique(sector$sector)))
use_palt <- palette_find(name = "wildflowers") %>%
  palette_subsample(wanted = length(unique(combo$webinar_useful)),
                    random_seed = 10)

# Want to check out other palettes? Demo 'em!
palette_find(name = "wildflowers") %>%
  palette_demo(.)

# Identify number of rows for faceted part of graph
facet_row_num <- ceiling(length(unique(combo$working_group)) / 2)

## ------------------------------------- ##
# Attendance Graphic ----
## ------------------------------------- ##

# Create a plot for total attendance
(hc_total <- ggplot(headcount, aes(x = working_group, y = attendance,
                                  fill = working_group, color = 'x')) +
  geom_bar(stat = 'identity') +
  labs(x = "Webinar", y = "Total Attendance") +
  scale_fill_manual(values = hc_palt) +
  scale_color_manual(values = 'black') +
  lno_theme + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()))

# And another for proportion that responded to the survey
(hc_prop <- ggplot(headcount, aes(x = working_group, y = prop_survey,
                                 fill = working_group, color = 'x')) +
  geom_bar(stat = 'identity') +
  ylim(0, 1) +
  labs(x = "Webinar", y = "Proportion Surveyed") +
  scale_fill_manual(values = hc_palt) +
  scale_color_manual(values = 'black') +
  lno_theme + theme(axis.text.x = element_text(angle = 35, hjust = 1)))

# Combine them
cowplot::plot_grid(hc_total, hc_prop, nrow = 2, ncol = 1)

# Save this plot
ggsave(filename = file.path("plots", "webinar_report_attendance.tiff"),
       plot = last_plot(), width = 4, height = 7, units = 'in')

## ------------------------------------- ##
# Advertising Graphic ----
## ------------------------------------- ##

# Plot it with facets
(ad_facet <- ggplot(advert, aes(x = heard_where, y = heard_freq,
                   fill = heard_where, color = 'x')) +
  geom_bar(stat = 'identity') +
  labs(x = "Heard where?", y = "Number Respondents",
       title = "Where did you hear about the webinar?") +
  facet_wrap(facets = vars(working_group), nrow = facet_row_num, ncol = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = ad_palt) +
  scale_color_manual(values = 'black') +
  lno_theme + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()))

# Plot a composite
(ad_comp <- ggplot(advert, aes(x = heard_where, y = heard_freq,
                               fill = heard_where)) +
  geom_bar(stat = 'identity') +
  labs(x = "Heard where?", y = "Number Respondents") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = ad_palt) +
  lno_theme + theme(axis.text.x = element_text(angle = 35, hjust = 1),
                    axis.title.x = element_blank()))

# Combine them
cowplot::plot_grid(ad_facet, ad_comp, nrow = 2, ncol = 1)

# Save it
ggsave(filename = file.path("plots", "webinar_report_advert.tiff"),
       plot = last_plot(), width = 5, height = 9, units = 'in')

## ------------------------------------- ##
# Sector Graphic ----
## ------------------------------------- ##

# Plot it
(sec_facet <- ggplot(sector, aes(x = sector, y = sector_attendance,
                   fill = sector, color = 'x')) +
  geom_bar(stat = 'identity') +
  labs(x = "Job Sector", y = "Number Attendees") +
  facet_wrap(facets = vars(working_group), nrow = facet_row_num, ncol = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = sec_palt) +
  scale_color_manual(values = 'black') +
  lno_theme + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()))

(sec_comp <- ggplot(sector, aes(x = sector, y = sector_attendance, fill = sector)) +
  geom_bar(stat = 'identity') +
  labs(x = "Job Sector", y = "Number Attendees") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = sec_palt) +
  lno_theme + theme(axis.text.x = element_text(angle = 25, hjust = 1),
                    axis.title.x = element_blank()))

# Combine them
cowplot::plot_grid(sec_facet, sec_comp, nrow = 2, ncol = 1)

# Save it
ggsave(filename = file.path("plots", "webinar_report_sector.tiff"),
       plot = last_plot(), width = 5, height = 9, units = 'in')

## ------------------------------------- ##
# Utility Graphic ----
## ------------------------------------- ##

# Plot
(use_facet <- ggplot(utility, aes(x = webinar_useful, y = useful_freq,
                    fill = webinar_useful, color = 'x')) +
  geom_bar(stat = 'identity') +
  labs(x = "Job Sector", y = "Number Attendees",
       title = "Was the webinar useful?") +
  facet_wrap(facets = vars(working_group), nrow = facet_row_num, ncol = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_color_manual(values = 'black') +
  scale_fill_manual(values = use_palt) +
  lno_theme + theme(axis.text.x = element_blank(),
                    axis.title.x = element_blank()))

(use_comp <- ggplot(utility, aes(x = webinar_useful, y = useful_freq,
                                fill = webinar_useful)) +
  geom_bar(stat = 'identity') +
  labs(x = "Job Sector", y = "Number Attendees") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = use_palt) +
  lno_theme + theme(axis.title.x = element_blank()))

# Combine them
cowplot::plot_grid(use_facet, use_comp, nrow = 2, ncol = 1)

# Save
ggsave(filename = file.path("plots", "webinar_report_useful.tiff"),
       plot = last_plot(), width = 5, height = 9, units = 'in')

# End ----
