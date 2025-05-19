## -------------------------------------------- ##
      # LTER Site Polygon Custom Functions
## -------------------------------------------- ##

## -------------------------------- ##
# Polygon Tidying ----
## -------------------------------- ##

# This function accepts a basically raw (but still 'sf') boundary for a particular site and readies it for inclusion in the network-wide set of polygons
poly_tidy <- function(site_sf = NULL, network_sf = NULL,
                      code = NULL, name = NULL, plot = FALSE){
  
  # Error out if any arguments are undefined
  if(any(is.null(site_sf) | is.null(network_sf) | is.null(code) | is.null(name)) == TRUE)
    stop("All arguments must be defined")
  
  # Error out for inappropriate spatial class
  if(all(class(site_sf) %in% c("sf", "data.frame")) != TRUE)
    stop("Site-specific boundary must be supplied as a simple features ('sf') object")
  if(all(class(network_sf) %in% c("sf", "data.frame")) != TRUE)
    stop("Network boundaries must be supplied as a simple features ('sf') object")
  
  # Error out for inappropriate code/name class
  if(is.character(code) != TRUE | is.character(name) != TRUE)
    stop("Site code and full name must be supplied as characters")
  
  # Error out for malformed site code
  if(nchar(code) != 3)
    stop("Site code must be the three-letter abbreviation for the site.")
  
  # Warn and fix for malformed 'plot' logical
  if(is.logical(plot) != TRUE){
    warning("'plot' must be a logical, defaulting to FALSE")
    plot <- FALSE }
  
  # Actually do the needed wrangling
  site_actual <- site_sf %>% 
    # Transform CRS (it may be correct already but better safe than sorry)
    sf::st_transform(x = ., crs = sf::st_crs(network_sf)) %>% 
    # Add in desired columns
    dplyr::mutate(SITE = code, NAME = name) %>% 
    # Drop all columns except those new ones
    dplyr::select(SITE, NAME)
  
  # If plotting is desired, make a simple plot
  if(plot == TRUE){
    plot(site_actual["SITE"], main = name, axes = TRUE)
  }
  
  # Return the sf object
  return(site_actual)
  
} # Close function





# End ----

