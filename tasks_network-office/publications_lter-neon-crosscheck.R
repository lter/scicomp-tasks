## -------------------------------------------------- ##
        # LTER / NEON Publication Checker
## -------------------------------------------------- ##
# Script author(s): Nick J Lyon

# PURPOSE:
## Compare LTER / NEON publication lists

## ------------------------------ ##
         # Housekeeping ----
## ------------------------------ ##

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Clear environment
rm(list = ls())

## ------------------------------ ##
        # Library Import ----
## ------------------------------ ##

# Make some empty lists
lter_list <- list()
neon_list <- list()

# Get all collections' citations (for LTER)
for(colxn in dir(path = file.path("data", "LTER Collections"))){
  
  # Processing message
  message("Retrieving collection: ", colxn)
  
  # Read in file and add to list
  lter_list[[colxn]] <- read.csv(file = file.path("data", "LTER Collections", colxn)) %>% 
    # Make all columns into characters
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                                .fns = as.character)) }

# Do the same for NEON
for(colxn in dir(path = file.path("data", "NEON Collections"))){
  
  # Processing message
  message("Retrieving collection: ", colxn)
  
  # Read in file and add to list
  neon_list[[colxn]] <- read.csv(file = file.path("data", "NEON Collections", colxn)) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                                .fns = as.character)) }

# Unlist into a dataframe (for both) and drop duplicates (if Zotero made any)
lter_v0 <- dplyr::distinct(purrr::list_rbind(x = lter_list))
neon_v0 <- dplyr::distinct(purrr::list_rbind(x = neon_list))

# Tidy environment
rm(list = setdiff(x = ls(), y = c("lter_v0", "neon_v0")))

## ------------------------------ ##
        # Library Prep ----
## ------------------------------ ##

# Check structure of one (same columns in both)
dplyr::glimpse(lter_v0)

# Wrangle the full information into just what we need
lter_v1 <- lter_v0 %>% 
  # Pare LTER pubs down to just the desired columns
  dplyr::select(Item.Type, dplyr::starts_with("Publication."), 
                Author, Title, ISBN:DOI) %>% 
  # Add a column indicating which library these publications are in
  dplyr::mutate(library = "LTER", .before = dplyr::everything()) %>% 
  # Make empty cells true NAs (across all columns)
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(test = nchar(.x) == 0,
                                              yes = NA, no = .x))) %>% 
  # Make the contents of all columns lowercase
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = tolower)) %>% 
  # Make publication year numeric and drop any pubs missing year info
  dplyr::mutate(Publication.Year = as.numeric(Publication.Year)) %>% 
  dplyr::filter(!is.na(Publication.Year)) %>% 
  # Drop any publications before LTER was founded
  dplyr::filter(Publication.Year >= 1980) %>% 
  # Filter to only journal articles or theses (and drop that column after)
  dplyr::filter(Item.Type %in% c("journalarticle", "thesis")) %>% 
  dplyr::select(-Item.Type) %>% 
  # Drop non-unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(lter_v1)
## view(lter_v1)

# Do the same for NEON
neon_v1 <- neon_v0 %>% 
  dplyr::select(Item.Type, dplyr::starts_with("Publication."), 
                Author, Title, ISBN:DOI) %>% 
  dplyr::mutate(library = "NEON", .before = dplyr::everything()) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = ~ ifelse(test = nchar(.x) == 0,
                                              yes = NA, no = .x))) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
                              .fns = tolower)) %>% 
  dplyr::mutate(Publication.Year = as.numeric(Publication.Year)) %>% 
  dplyr::filter(!is.na(Publication.Year)) %>% 
  dplyr::filter(Publication.Year >= 1980) %>% 
  dplyr::filter(Item.Type %in% c("journalarticle", "thesis")) %>% 
  dplyr::select(-Item.Type) %>% 
  dplyr::distinct()

# Check structure of this too
dplyr::glimpse(neon_v1)

## ------------------------------ ##
    # Shared Pub Counting ----
## ------------------------------ ##
# Identify whether a given LTER pub is in the NEON list too
lter_v2 <- lter_v1 %>% 
  # Do counting conditionally
  dplyr::mutate(shared = dplyr::case_when(
    !is.na(Title) & Title %in% neon_v1$Title ~ 1,
    !is.na(ISBN) & ISBN %in% neon_v1$ISBN ~ 1,
    !is.na(ISSN) & ISSN %in% neon_v1$ISSN ~ 1,
    !is.na(DOI) & DOI %in% neon_v1$DOI ~ 1,
    T ~ 0))

# Check structure
dplyr::glimpse(lter_v2)
## view(lter_v2)

# Do the same counting for NEON
neon_v2 <- neon_v1 %>% 
  # Do counting conditionally
  dplyr::mutate(shared = dplyr::case_when(
    !is.na(Title) & Title %in% lter_v1$Title ~ 1,
    !is.na(ISBN) & ISBN %in% lter_v1$ISBN ~ 1,
    !is.na(ISSN) & ISSN %in% lter_v1$ISSN ~ 1,
    !is.na(DOI) & DOI %in% lter_v1$DOI ~ 1,
    T ~ 0)) %>% 
  # Drop shared pubs (they're already in the LTER object)
  dplyr::filter(shared != 1)

# Check structure
dplyr::glimpse(neon_v2)

## ------------------------------ ##
    # Library Integration ----
## ------------------------------ ##

# Actual integration across datasets
combo_v1 <- dplyr::bind_rows(lter_v2, neon_v2) %>% 
  # Tweak the name of columns with overly long names
  dplyr::rename(pub_year = Publication.Year) %>% 
  # Count instances per year and publication type
  dplyr::group_by(library, pub_year) %>% 
  dplyr::summarize(total_ct = dplyr::n(),
                   shared_ct = sum(shared, na.rm = T)) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(combo_v1)

# Pre-visualization needed wrangling
combo_v2 <- combo_v1 %>% 
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::ends_with("_ct"),
                      values_to = "pub_ct") %>% 
  # Filter out instances where no shared publications were present
  dplyr::filter(!(name == "shared_ct" & pub_ct == 0)) %>% 
  # Generate useful categories
  dplyr::mutate(category = dplyr::case_when(
    library == "lter" & name != "shared_ct" ~ "LTER only",
    library == "lter" & name == "shared_ct" ~ "Shared",
    library == "neon" & name != "shared_ct" ~ "NEON only"),
    .before = dplyr::everything()) %>% 
  # Mess with the factor order
  dplyr::mutate(category = factor(category, levels = c("LTER only", "Shared", "NEON only"))) %>% 
  # Drop unwanted columns
  dplyr::select(-library, -name)

# Check structure
dplyr::glimpse(combo_v2)

# Make a version for shared only (LTER focal)
shared_only <- dplyr::filter(combo_v2, category == "Shared")

# Check structure
dplyr::glimpse(shared_only)

# Make a version where the *proportion* shared is calculation
shared_prop <- combo_v2 %>% 
  # Drop NEON pubs
  dplyr::filter(category != "NEON only") %>% 
  # Pivot wider
  tidyr::pivot_wider(names_from = category, values_from = pub_ct, values_fill = 0) %>% 
  # Calculate proportion shared
  dplyr::mutate(prop = Shared / `LTER only`, .after = pub_year) %>% 
  # Drop unwanted columns (implicitly)
  dplyr::select(pub_year, prop)

# Check structure
dplyr::glimpse(shared_prop)

## ------------------------------ ##
            # Visuals ----
## ------------------------------ ##

# Create folder for exporting graphs locally
dir.create(path = file.path("graphs"), showWarnings = F)

# Graph 1 - Shared paper *count* over time
ggplot(shared_only, aes(x = pub_year, y = pub_ct)) +
  geom_smooth(method = "loess", formula = "y ~ x", se = F, color = 'black') +
  geom_point(aes(fill = pub_ct), shape = 21, size = 3) +
  labs(y = "Shared Publication Count", x = "Publication Year") +
  supportR::theme_lyon() +
  theme(legend.position = "none")

# Export
ggsave(filename = file.path("graphs", "lter-neon-pubs_shared-ct.png"),
       height = 4, width = 6, units = "in")

# Graph 2 - Shared paper *proportion* over time
ggplot(shared_prop, aes(x = pub_year, y = prop)) +
  geom_smooth(method = "loess", formula = "y ~ x", se = F, color = 'black') +
  geom_point(aes(fill = prop), shape = 21, size = 3) +
  labs(y = "Shared Publication Proportion", x = "Publication Year") +
  supportR::theme_lyon() +
  theme(legend.position = "none")

# Export
ggsave(filename = file.path("graphs", "lter-neon-pubs_shared-prop.png"),
       height = 4, width = 6, units = "in")

# Graph 3 - Stacked bar plot of LTER only, NEON only, and shared
ggplot(combo_v2, aes(x = pub_year, y = pub_ct)) +
  geom_bar(aes(fill = category), color = "black", stat = "identity") +
  labs(y = "Publication Count", x = "Publication Year") +
  scale_fill_manual(values = c("LTER only" = "#97AE3F", 
                               "Shared" = "#B89E92", 
                               "NEON only" = "#0262bf")) +
  supportR::theme_lyon()
  
# Export
ggsave(filename = file.path("graphs", "lter-neon-pubs_shared-vs-not.png"),
       height = 4, width = 6, units = "in")

# End ----
