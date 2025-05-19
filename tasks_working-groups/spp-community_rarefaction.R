## ----------------------------------------------------- ##
                # Rarefaction Exploration
## ----------------------------------------------------- ##
## ------------------------------------- ##
            # Vegan Package ----
## ------------------------------------- ##

# Load vegan
# install.packages("librarian")
librarian::shelf(vegan)

# Clear environment
rm(list = ls())

# Load data
data(BCI)

# Glimpse it
dplyr::glimpse(BCI)

# Calculate number of species
(S <- vegan::specnumber(BCI))

# Now identify minimum sample size
(sample_min <- min(rowSums(BCI)))

# Do rarefaction
Srare <- vegan::rarefy(x = BCI, sample = sample_min, se = F)

# Exploratory plotting
base::plot(x = S, y = Srare, 
           xlab = "Observed No. of Species", 
           ylab = "Rarefied No. of Species")
graphics::abline(0, 1)

# Now plot rarefaction curves
vegan::rarecurve(x = BCI, sample = raremax, step = 20,
                 col = "blue", cex = 0.6)

## ------------------------------------- ##
      # Rarefy Package - Spatial ----
## ------------------------------------- ##
# See: https://cran.r-project.org/web/packages/Rarefy/vignettes/Rarefy_basics.html

# Load Rarefy packages
# install.packages("librarian")
librarian::shelf(Rarefy, ade4, adiv, ape, vegan, phyloregion, raster)
## Note that "Rarefy" requires (at least) Mac users to have XQuartz installed

# Load data
data("duneFVG") #plot/species matrix
data("duneFVG.xy") #plots geographic coordinates

# Calculates pairwise Euclidean distances among sample units
dist_sp <- stats::dist(x = duneFVG.xy$tot.xy, method = "euclidean")

# Calculate directional and non-directional accumulation curves
ser_rarefaction <- Rarefy::directionalSAC(community = duneFVG$total, gradient = dist_sp)

# Make exploratory plot
base::plot(x = 1:128, y = ser_rarefaction$N_Exact,
           xlab = "M", ylab = "Species richness",
           ylim = c(0, 71), pch = 1)
graphics::points(x = 1:128, y = ser_rarefaction$N_SCR, pch = 2)
graphics::legend(x = "bottomright",
                 legend = c("Classic Rarefaction",
                            "Spatially-explicit Rarefaction"),
                 pch = 1:2)

## ------------------------------------- ##
    # Rarefy Package - Alpha Div. ----
## ------------------------------------- ##
# Define set of community metrics
a <- list(NA,'Shannon')
names(a) <- c('comm','method')

# Perform rarefaction with and without spatial explicit-ness
rare_shannon <- Rarefy::rare_alpha(comm = duneFVG$total,
                                   method = "fun_div",
                                   random = 999,
                                   fun_div = 'speciesdiv',
                                   args = a, mean = T, verbose = F,
                                   spatial = FALSE)
rare_shannon_sp <- Rarefy::rare_alpha(comm = duneFVG$total,
                                      dist_xy = dist_sp,
                                      method = "fun_div",
                                      random = 999,
                                      fun_div = 'speciesdiv',
                                      args = a, mean = T, verbose = F,
                                      spatial = TRUE)

# Exploratory plot
base::plot(rare_shannon[,1], ylab = "Shannon index",
           xlab = "Number of sampling units",
           type = "l", ylim = range(rare_shannon, na.rm=TRUE))
graphics::lines(rare_shannon[,2], lty = 2)
graphics::lines(rare_shannon[,3], lty = 2)
graphics::lines(rare_shannon_sp[,1], col = 4)
graphics::lines(rare_shannon_sp[,2], lty = 2, col = 4)
graphics::lines(rare_shannon_sp[,3], lty = 2, col = 4)
graphics::legend(x = "bottomright",
                 legend = c("Non spatially-explicit Rarefaction",
                            "Spatially-explicit Rarefaction"),
                 lty = 1, col = c(1, 4))

# End ----
