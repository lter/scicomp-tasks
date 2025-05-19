## ------------------------------------------------------------- ##
      # LTER Network Office - Synthesis Activities Timeline
## ------------------------------------------------------------- ##
# Written by: Nick J Lyon

# Purpose
## Create visual timeline of different working group (WG) activities separated by WG

# Load needed libraries
librarian::shelf(tidyverse, googledrive, scales, cowplot)

# Make any needed folders
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("plots"), showWarnings = F)

# Clear environment
rm(list = ls())

## --------------------------------- ##
          # Housekeeping ----
## --------------------------------- ##

# Define primary Drive folder
activity_drive <- googledrive::as_id("https://drive.google.com/drive/folders/15TpanlsaMTWYJstbAYjqHzTHoXM3ZGXn")

# Download timeline data
googledrive::drive_ls(path = activity_drive, pattern = "working-group-activities") %>%
  googledrive::drive_download(file = ., path = file.path("data", "working-group-activities.csv"), overwrite = T)

# Read in the data
activities_raw <- read.csv(file = file.path("data", "working-group-activities.csv"))
dplyr::glimpse(activities_raw)

# Make a vector of our preferred order for event types
event_ord <- c("Onboarding", "Meeting", "Training", "Webinar",
               # split by activities (^) and products (v)
               "Award", "Paper", "Dataset", "Code")

# Tidy the data a bit
activities <- activities_raw %>%
  # Keep only needed columns
  dplyr::select(Cohort, Synthesis.Group.Type, Synthesis.Group,
                Position, Date, Event.type) %>%
  # Rename columns more descriptively
  dplyr::rename(group_type = Synthesis.Group.Type,
                synthesis_group = Synthesis.Group,
                group_order = Position,
                date = Date,
                event_type = Event.type,
                cohort = Cohort) %>%
  # Remove any NAs in the synthesis_group or date columns
  dplyr::filter(nchar(synthesis_group) != 0 &
                  nchar(date) != 0 &
                  nchar(event_type) != 0) %>%
  # Split raw date into component parts
  tidyr::separate_wider_delim(cols = date, delim = "/", names = c("month", "day", "year")) %>%
  # Make them all numbers
  dplyr::mutate(dplyr::across(.cols = c(month, day, year), .fns = as.numeric)) %>%
  # If year is two-digit, make it four digit
  dplyr::mutate(year = ifelse(nchar(year) == 2,
                yes = as.numeric(paste0("20", year)), no = year)) %>% 
  # Make date a "real" date
  dplyr::mutate(date = as.Date(paste(month, day, year, sep = "-"), format = "%m-%d-%Y"),
                posix_date = as.POSIXct(x = date),
                .before = event_type) %>%
  # Make event type a factor
  dplyr::mutate(event_type = factor(event_type, levels = event_ord)) %>% 
  # Tweak the group type and cohort columns (to look nicer for the graphs)
  dplyr::mutate(type_nice = paste0(group_type, " Groups"),
                cohort_nice = paste0(cohort, " Cohort"),
                facet_nice = ifelse(group_type == "Full",
                                    yes = paste0(group_type, " Groups"),
                                    no = paste0(group_type, " Groups (", cohort, " Cohort)")))

# Check it out
dplyr::glimpse(activities)

# Check factor columns (contents and levels)
unique(activities$synthesis_group)
unique(activities$event_type)

# We'll use the 'group_order' column so we need to be totally sure that it is 1-to-1 with the synthesis group names (i.e., that there is never a typo where the same order number goes to multiple synthesis_groups)
sort(unique(paste0(activities$group_order, ' - ', activities$synthesis_group)))
## If duplicates are in that list, fix it upstream in the above pipe

# Split full and SPARC groups
full_acts <- dplyr::filter(activities, group_type == "Full")
sparc_acts <- dplyr::filter(activities, group_type == "SPARC")

# Check that this doesn't lose any rows
nrow(full_acts) + nrow(sparc_acts) == nrow(activities)

## --------------------------------- ##
            # Graph Prep ----
## --------------------------------- ##

# Create a vector of shapes to assign to the event types
event_shapes <- c("Award" = 8, "Onboarding" = 4, "Meeting" = 5, 
                  "Training" = 1, "Webinar" = 2,
                  # as with levels, split by activities (^) and products (v)
                  "Paper" = 15, "Dataset" = 18, "Code" = 16, "Analytical Package" = 17)

# Assign colors to each working group for plotting
wg_colors <- c(
  # 2016 cohort (browns)
  "Metacommunities" = "#543005",
  "C2E" = "#8c510a",
  "Streams" = "#bf812d", 
  # 2017 cohort (teals)
  "Synchrony" = "#003c30",
  "Productivity-Biodiversity" = "#01665e",
  "Soil Organic Matter" = "#35978f",
  # 2020 cohort (pinks)
  "Drought Effects" = "#8e0152",
  "EMERGENT" = "#c51b7d",
  "Si Synthesis" = "#de77ae", 
  # 2021 cohort (greens)
  "Plant Reproduction" = "#276419",
  "Ecosystem Transitions" = "#7fbc41",
  # 2022 cohort (red-oranges)
  "Marine Consumer Nutrient Dynamics" = "#800026",
  "Flux Gradient Project" = "#bd0026",
  "Pelagic Community Structure" = "#e31a1c",
  "Selection Across Scales" = "#fc4e2a",
  "Soil P Control of C and N" = "#fd8d3c",
  "Fire and Aridland Streams" = "#feb24c",
  "Producers Consumers and Disturbance" = "#fed976",
  # 2024 cohort (blues)
  "CAGED" = "#0077b6",
  "Resilience Management" = "#00b4d8"
  )

# Make sure none are missing
supportR::diff_check(old = unique(activities$synthesis_group),
                     new = names(wg_colors))

# Define some background theme elements
theme_activities <- theme_classic() +
  theme(legend.title = element_blank(),
        legend.background = element_blank(),
        strip.text = element_text(size = 15), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1) )

## --------------------------------- ##
        # Full Group Graph ----
## --------------------------------- ##

# Make a graph for just the full working groups' activities
full_timeline <- ggplot(data = full_acts, aes(x = posix_date, shape = event_type,
                                              y = reorder(synthesis_group, group_order, 
                                                          decreasing = F))) +
  geom_path(aes(group = synthesis_group, color = synthesis_group), 
            lwd = 1, lineend = "round") +
  geom_point(aes(color = synthesis_group), size = 5, alpha = 0.75) +
  # Handle axes & palette/shape settings
  labs(x = "Date", y = "Synthesis Group") +
  scale_shape_manual(values = event_shapes, breaks = event_ord) +
  scale_color_manual(values = wg_colors) +
  scale_x_datetime(breaks = scales::breaks_width("1 year"), date_labels = "%b.  %Y") +
  # Facet by group type just to get a neat label above the graph
  facet_grid(. ~ type_nice) +
  # Add some information about COVID 19
  annotate("rect", ymin = -1, ymax = Inf, alpha = 0.1,fill = "gray45",
           xmin = as.POSIXct(as.Date(x = "01-31-2020", format = "%m-%d-%Y")),
           xmax = as.POSIXct(as.Date(x = "05-11-2023", format = "%m-%d-%Y"))) +
  geom_text(label = "COVID 19", size = 6, y = 0.05,
            x = as.POSIXct(as.Date(x = "09-15-2021", format = "%m-%d-%Y"))) +
  # Handle theme formatting
  guides(color = 'none') +
  theme_activities +
  theme(legend.position = "inside",
        legend.position.inside = c(0.3, 0.8)); full_timeline

# Export locally
ggsave(filename = file.path("plots", "timeline_full-groups.png"),
       plot = last_plot(), width = 12, height = 9, units = 'in')

## --------------------------------- ##
          # SPARC Graphs ----
## --------------------------------- ##

# Make an empty list
sparc_list <- list()

# For each cohort of SPARC groups
for(focal_cohort in unique(sparc_acts$cohort)){
  
  # Subset to just that cohort
  sparc_cohort <- dplyr::filter(sparc_acts, cohort == focal_cohort)
  
  # Create a timeline graph for this cohort of SPARC groups
  q <- ggplot(data = sparc_cohort, aes(x = posix_date, shape = event_type,
                                       y = reorder(synthesis_group, group_order, decreasing = F))) +
    geom_path(aes(group = synthesis_group, color = synthesis_group), 
              lwd = 1, lineend = "round") +
    geom_point(aes(color = synthesis_group), size = 5, alpha = 0.75) +
    # Handle axes & palette/shape settings
    labs(x = "Date", y = "Synthesis Group") +
    scale_shape_manual(values = event_shapes, breaks = event_ord) +
    scale_color_manual(values = wg_colors) +
    scale_x_datetime(breaks = scales::breaks_width("3 month"), date_labels = "%b.  %Y") +
    # Facet by group type just to get a neat label above the graph
    facet_grid(. ~ facet_nice) +
    # Handle theme formatting
    guides(color = 'none') +
    theme_activities +
    theme(legend.position = "none") # Turning off legend because it's in the full group version
    # theme(legend.position = c(0.9, 0.3))
 
  # Add COVID information to the only SPARC cohort for which it is relevant (2022)
 if(focal_cohort == 2022){
  
   q <- q +
     annotate("rect", ymin = -1, ymax = Inf, alpha = 0.1,fill = "gray45",
              xmin = as.POSIXct(as.Date(x = "01-01-2023", format = "%m-%d-%Y")),
              xmax = as.POSIXct(as.Date(x = "05-11-2023", format = "%m-%d-%Y"))) +
     geom_text(label = "COVID 19", size = 6, y = 0.05,
               x = as.POSIXct(as.Date(x = "03-05-2023", format = "%m-%d-%Y")))
  }
  
  # Add graph to list
  sparc_list[[as.character(focal_cohort)]] <- q }

# Generate a multi-panel figure for this information
sparc_timeline <- cowplot::plot_grid(plotlist = sparc_list, ncol = 1, nrow = 2); sparc_timeline

# Export locally
ggsave(filename = file.path("plots", "timeline_sparc-groups.png"),
       plot = last_plot(), width = 8, height = 5, units = 'in')

## --------------------------------- ##
      # Combination Figure ----
## --------------------------------- ##

# Combine full and SPARC timelines
cowplot::plot_grid(full_timeline, sparc_timeline, nrow = 1, ncol = 2)

# Save this locally as well
ggsave(filename = file.path("plots", "timeline_complete.png"),
       plot = last_plot(), width = 20, height = 9, units = "in")

## --------------------------------- ##
      # Google Drive Export ----
## --------------------------------- ##

# Define necessary Drive link
graph_drive <- googledrive::as_id("https://drive.google.com/drive/folders/1VvXfGlLeJDeaquZifRKIAQeEeusoGedn")

# Identify timeline graphs
(timeline_files <- dir(path = file.path("plots"), pattern = "timeline_"))

# Upload these to the Drive!
for(file in timeline_files){
  googledrive::drive_upload(media = file.path("plots", file), 
                            overwrite = T, path = graph_drive) }

# END ----
