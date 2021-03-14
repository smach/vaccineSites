# Load libraries
library(tidyverse)
library(tidycensus)
library(rio)
library(sf)
library(leaflet)
library(scales)
library(RColorBrewer)
library(htmltools)
library(gmapsdistance) # if you have a Google Maps API key
# You need a Google Maps API key stored in your R environment if you want to calculate driving distances. There is an alternative using closest points to polygons calculations that doesn't require the Google Maps API, but it does not understand distances shouldn't be calculated over water.

# You need a US Census API key to use tidycensus


# Step 1: Data by city and town ----
# Get estimated 18+ population by city/town
ma_towns <- get_acs(
  geography = "county subdivision",
  state = "MA",
  variables = c("AdultPopulation" = "B09021_001"),
  year = 2018,
  geometry = TRUE
)

# save for future use:
saveRDS(ma_towns, "ma_towns_basic.Rds")
# Load if you've already run once
# ma_towns <- readRDS("ma_towns.Rds")

# Make a new place column with just the city/town name
ma_towns$Place <- gsub(" town,.*?$", "",   ma_towns$NAME)
ma_towns$Place <- gsub(" Town,.*?$", "",   ma_towns$Place)
ma_towns$Place <- gsub(" Town.*?$", "",   ma_towns$Place)
ma_towns$Place <- gsub(" city,.*?$", "",   ma_towns$Place)
ma_towns$Place <- gsub(" City,.*?$", "",   ma_towns$Place)

# Load spreadsheet with 7 mass vaccination sites and their latitude and longitude. Geocoding was done with geocod.io

sites <- rio::import("geocoded_sites.xlsx")

# Project ma_towns to use Google Maps CRS
ma_towns <- st_transform(ma_towns, "WGS84")

# Step 2: Find closest sites to each city/town: ----

# If you have a Google Maps API key in GOOGLE_MAP_GEOCODING_KEY environment variable:

if(Sys.getenv("GOOGLE_MAP_GEOCODING_KEY") != "") {

  # I broke this up first to make sure I wasn't running into problems with the API key before running it all at once on all 351 places, trying it on rows 1:10. You may want to do that also.
  
 myorigin = ma_towns$Place[1:351]
 # The API does not want commas or spaces in place names, so I'm changing them to + signs
 myorigin <- paste0(myorigin, "+MA")
 myorigin <- gsub(" ", "+", myorigin)
 
 # Here I'm using longitude and latitude for the vaccine sites and also adding + to place names
 mydest_long = sites$Longitude
 mydest_lat = sites$Latitude
 my_dest_names = sites$Site 
 mydest <- paste(mydest_lat, mydest_long, sep = "+")
 
 # Make sure you are ready to run this with your Google Maps API key! It will take awhile to run. Replace "GGMAP_GOOGLE_API_KEY" with whatever R environment variable has your API key.
 myresults <- gmapsdistance::gmapsdistance(origin = myorigin, destination = mydest, mode = "driving", shape = "long", key = Sys.getenv("GGMAP_GOOGLE_API_KEY"))
 
 # Distances are in the list item named Distance, I'll save that data frame in a separate variable
 mydistance <- myresults$Distance

 # And then I'll immediately save these so I don't have to run the Google Maps API calls again
 saveRDS(myresults, "myresults.Rds")
 saveRDS(mydistance, "mydistance.Rds")
 
    
    # get Site names, Place names, and distance in miles
    mysite_lookup <- my_dest_names
    names(mysite_lookup) <- mydest
    mydistance$Site <- mysite_lookup[mydistance$de]
    
 # Distance is returned in meters; changing to miles:
    mydistance$Miles <- round((mydistance$Distance / 1000) * 0.621371)
    
    # I combined both Boston sites since it is difficult to calculate which one people are closest to when only using an entire city or town
    mydistance$ClosestSite <- ifelse(mydistance$Site %in% c("Reggie Lewis Center", "Hynes Convention Center"), "Hynes or Reggie Lewis Ctr", mydistance$Site)
    mydistance <- mydistance[!is.na(mydistance$Miles),]
    
    # Saving mydistance edited to a new file:
    saveRDS(mydistance, "mydistance_all.Rds")
    
    
    # Time to calculate which site is closest to each place:
    closest_sites <- mydistance %>%
      group_by(or) %>%
      arrange(Distance) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        or = as.character(or),
        Place = stringr::str_replace_all(or, "\\+MA", ""),
        Place = stringr::str_replace_all(Place, "\\+", " ")
      ) %>%
      select(
        Place, ClosestSite, Miles
      )
    # Saving that also
    saveRDS(closest_sites, "closest_sites.Rds")
    

    # Merge ma_towns with closest sites to use for a map, save that, too
    ma_towns <- merge(ma_towns, closest_sites, by = "Place", all.x = FALSE, all.y = TRUE)
    # Save ma_towns version for map
    saveRDS(ma_towns, "ma_towns_for_map.Rds")
    
  } else {

# This is a way to approximate closest center without access to google maps api using polygon and points calculations in the sf package. However, this method does not know the difference between land and water, so some coastal areas will be inaccurate.


# Create a lookup table for site index and names, which will be needed when using sf::st_nearest_feature() to find closest vaccine site.
sites$index <- row.names(sites)
site_lookup <- sites$Site
names(site_lookup) <- sites$index
sites$index <- NULL

# turn sites into an sf geospatial object:
sites_sf = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4269, agr = "constant")

# Get nearest point for each city/town polygon.
nearest_site_calculation <- st_nearest_feature(ma_towns, sites_sf)

# Add nearest point info to the ma_towns data
ma_towns$ClosestSiteIndex <- as.character(nearest_site_calculation)

# Add column for site name from side index
ma_towns$ClosestSite <- site_lookup[ma_towns$ClosestSiteIndex]
ma_towns$ClosestSiteIndex <- NULL


# Manually fix a couple of glaring problems
change_site <- function(myplace, mynewsite, mydf = ma_towns) {
  mydf$ClosestSite[mydf$Place == myplace] <- mynewsite
  return(mydf)
}

ma_towns <- change_site("Provincetown", "Circuit City Dartmouth")
ma_towns <- change_site("Truro", "Circuit City Dartmouth")


}
## End alternate polygons-to-points calculations


# Step 3: Summarize population for each site's closest towns ----
# Easier to calculate sums without the large geometry file not needed here, so I make a separate object without the geometry before calculating population by group
ma_towns_no_geo <- ma_towns
ma_towns_no_geo$geometry <- NULL
adult_population <- ma_towns_no_geo %>%
  group_by(ClosestSite) %>%
  summarize(
    AdultPopulation = sum(estimate, na.rm = TRUE)
  )


# Alternative by census tract: Data by census tract using polygons not driving distance ----
# Problem again is not understanding you can't calculate distance over water but it's a rough  check of the city/towns analysis
# Get estimated 18+ population by city/town
ma_census_tracts <- get_acs(
  geography = "tract",
  state = "MA",
  variables = c("AdultPopulation" = "B09021_001"),
  year = 2018,
  geometry = TRUE
)

# save for future use:
saveRDS(ma_census_tracts, "ma_census_tracts.Rds")

# As in the alternative above, if you didn't run that code:
# Create a lookup table for site index and names, which will be needed when using sf::st_nearest_feature() to find closest vaccine site.
sites$index <- row.names(sites)
site_lookup <- sites$Site
names(site_lookup) <- sites$index
sites$index <- NULL

# turn sites into an sf geospatial object:
sites_sf = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4269, agr = "constant")

# Get nearest point for each city/town polygon.
tracts_nearest_site_calculation <- st_nearest_feature(ma_census_tracts, sites_sf)

# Add nearest point info to the ma_towns data
ma_census_tracts$ClosestSiteIndex <- as.character(tracts_nearest_site_calculation)

# Add column for site name from side index
ma_census_tracts$ClosestSite <- site_lookup[ma_census_tracts$ClosestSiteIndex]

# Summarize population for each site's closest towns
ma_census_tracts_no_geo <- ma_census_tracts
ma_census_tracts_no_geo$geometry <- NULL
census_tracts_adult_population <- ma_census_tracts_no_geo %>%
  group_by(ClosestSite) %>%
  summarize(
    AdultPopulation = sum(estimate, na.rm = TRUE)
  )

## End data by census tracts

# Step 4 Map by City/Town ----
palette_colors <- RColorBrewer::brewer.pal(6, "Dark2")

pal <- colorFactor(palette_colors, ma_towns$ClosestSite)
ma_towns <- st_transform(ma_towns, "WGS84")

popup_text <- paste0("<strong>",
  ma_towns$Place, ":</strong><br />Est. adult population: ",
  scales::comma(ma_towns$estimate, accuracy = 1), " (margin of error ",
  scales::comma(ma_towns$moe, accuracy = 1), ")<br />Closest site: ",
  ma_towns$ClosestSite, "<br />Est. driving distance: ",
  ma_towns$Miles, " miles"
)  %>%   
  lapply(htmltools::HTML)

site_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = ma_towns,
    fillColor = ~pal(ma_towns$ClosestSite),
    fillOpacity = 0.9,
    weight = 0.2,
    smoothFactor = 0.2,
    label = popup_text
  ) %>%
  addLegend(pal = pal, 
            values = ma_towns$ClosestSite, 
            position = "bottomleft", 
            title = "Closest Mass Vaccine Site") %>%
  addMarkers(
    data = sites,
    ~Longitude,
    ~Latitude,
    label = sites$Site
  )
site_map
saveRDS(site_map, "vaccine_site_map.Rds")

saveRDS(adult_population, "adult_population.Rds")
