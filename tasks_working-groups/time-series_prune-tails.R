## ---------------------------------------------- ##
              # Time Series Pruning
## ---------------------------------------------- ##
# Written by Nick J Lyon

# Purpose:
## Prune time series with long "tails" of 0/low values at start/end of series
## Developed for Plant Reproduction WG but they decided not to use it
## Saving it here in case another group has use fr something like this

# Housekeeping -------------------------------------

# Clear environment
rm(list = ls())

# Load needed packages
# install.packages("librarian")
librarian::shelf(cowplot,
                 tidyverse)

# Make a folder for saving plots to
dir.create(path = "plots", showWarnings = F)

# Simulate Time Series -----------------------------
## A = 0s at both start and end and one group in middle
( typeA <- data.frame(
  Year = 1989:2018,
  Plant.ID = rep('Plant 1', 30),
  Plot.ID = rep('Plot X', 30),
  seeds.per.trap = c(rep(0, 4), rep(33, 6), rep(0, 5), rep(16, 11), rep(0, 4) ) ) )
## B = 0s only at start
( typeB <- data.frame(
  Year = 1989:2018,
  Plant.ID = rep('Plant B', 30),
  Plot.ID = rep('Plot Y', 30),
  seeds.per.trap = c(rep(0, 15), rep(18, 5), rep(15, 10)) ) )
## C = 0s only at end
( typeC <- data.frame(
  Year = 1989:2018,
  Plant.ID = rep('Plant Zed', 30),
  Plot.ID = rep('Plot Bamboo', 30),
  seeds.per.trap = c(rep(21, 6), rep(10, 9), rep(0, 15)) ) )
## D = very complex (but should only drop three zeros at end)
( typeD <- data.frame(
  Year = 1989:2018,
  Plant.ID = rep('Plant Wilde', 30),
  Plot.ID = rep('Plot Hands', 30),
  seeds.per.trap = c(rep(0, 2), 10, rep(0,2), rep(0,2), 11,
                     rep(0, 12), rep(8, 4), rep(6, 2), rep(0, 4) ) ) )

# Assemble them into a single dataframe
test_df <- dplyr::bind_rows(typeA, typeB, typeC, typeD)

# Take a quick look
head(test_df)

# Identify Runs of Lows/Zeros ----------------------

# Track runs of zeros at start / end
test_df2 <- test_df %>%
  # Group by plant / plot
  dplyr::group_by(Plant.ID, Plot.ID) %>%
  # Arrange by year
  dplyr::arrange(Year, .by_group = TRUE) %>%
  # Get cumulative lows
  dplyr::mutate(is_low = ifelse(test = seeds.per.trap == 0,
                                yes = 1, no = 0),
                cumulative_lows = cumsum(is_low)) %>%
  # Find out whether they're preceded / followed by lows
  dplyr::mutate(low_seq = dplyr::case_when(
    is_low == 1 & lead(is_low, n = 1) == 1 ~ 'run lows',
    is_low == 1 & lag(is_low, n = 1) == 1 ~ 'run lows',
    TRUE ~ 'not')) %>%
  # If a row is NOT part of a run of lows, get the cumulative low number at that row (i.e., the number of low values that precede it)
  dplyr::mutate(diagnostic = ifelse(low_seq == 'not',
                                   yes = (cumulative_lows + 0.5),
                                   no = NA)) %>%
  # Now use that to figure out which lows should be removed
  dplyr::mutate(subset_suggestion = dplyr::case_when(
    # If a run of lows is < 3 long at start or end, we want to retain the two low values closest to non-low data
    is_low == 1 & low_seq == 'run lows' &
      cumulative_lows <= min(diagnostic, na.rm = T) &
      (min(diagnostic, na.rm = T) - cumulative_lows) < 3 ~ 'keep',
    is_low == 1 & low_seq == 'run lows' &
      abs((max(diagnostic, na.rm = T) - cumulative_lows)) < 3 ~ 'keep',
    # If a number is low, is part of a sequence of lows, and its cumulative lows is less than the number of lows for when the run of lows end, it should be dropped (this is low runs at the start of a time series)
    is_low == 1 & low_seq == 'run lows' &
      cumulative_lows < min(diagnostic, na.rm = T) ~ 'drop',
    # Same as above but this time for if a series ends with many lows
    is_low == 1 & low_seq == 'run lows' &
      cumulative_lows > max(diagnostic, na.rm = T) ~ 'drop',
    # Now, if a number is low/part of a low series BUT it has a cumulative low that is greater than that of the end of a low run and less than the max of a low run, keep it (this is zeros in the middle of a time series)
    is_low == 1 & low_seq == 'run lows' &
      length(unique(!is.na(diagnostic))) >= 2 &
      cumulative_lows > min(diagnostic, na.rm = T) &
      cumulative_lows < max(diagnostic, na.rm = T) ~ 'keep',
    # Keep all non-low values
    low_seq == 'not' ~ 'keep'))

# Filter Out Identified Lows -----------------------

# Remove lows flagged in previous chunk
test_df3 <- test_df2 %>%
  dplyr::filter(subset_suggestion == 'keep')

# Diagnostic Plotting ------------------------------

# Plot raw data
(raw_plt <- ggplot(test_df, aes(x = Year, y = seeds.per.trap)) +
  geom_path() +
  geom_point(size = 2) +
  facet_grid(Plant.ID ~ .) +
  labs(x = "Year", y = "", title = "Raw Series") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = 'none'))

# Plot data where 'bad' years are identified
(id_plt <- ggplot(test_df2, aes(x = Year, y = seeds.per.trap)) +
  geom_path() +
  geom_point(aes(color = subset_suggestion), size = 2) +
  facet_grid(Plant.ID ~ .) +
  scale_color_manual(values = c("#BB0000", "#000000")) +
  labs(x = "Year", y = "", title = "Unwanted Years Identified") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = c(0.135, 0.665),
          legend.background = element_rect(color = 'black')))

# Plot again with those values removed
(slim_plt <- ggplot(test_df3, aes(x = Year, y = seeds.per.trap)) +
    geom_path() +
    geom_point(size = 2) +
    facet_grid(Plant.ID ~ .) +
    labs(x = "Year", y = "", title = "Subsetted Series") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = 'none'))

# Plot them together
cowplot::plot_grid(raw_plt, id_plt, slim_plt, ncol = 3, nrow = 1)

# Save it
ggplot2::ggsave(filename = file.path("plots", "series_pruning.tiff"),
                plot = last_plot(),
                width = 12, height = 7, units = 'in')
