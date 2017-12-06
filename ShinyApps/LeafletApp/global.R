library(dplyr)

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )


fileExtension = "csv"
dataFiles <-
    dir(path = "data/", pattern = paste0("*.", "csv")) %>%
    gsub(pattern = paste0(".", fileExtension), replacement = "", x = .)

# loading terrestrial_mammals - do it global to only load it once, otherwise, it could be added to map.2 in server.R to make it slow only for species alert tab
# load this big shapefile only once (if it already as global variable it won't load again)
shapefile_path <- "data/Shapefiles"
# terrestrial mammals shapefiles 
if(!exists("terrestrial_mammals")){
  terrestrial_mammals <- readOGR(shapefile_path, "new_terrestrial_mammals", verbose = FALSE) %>%
    spTransform(CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
}

isPointInBoundaries <- function(sp, spgeom, specie_name){
  # if spgeom is empty (specie not found in shapefile), then it will throw an expection
  tryCatch({
    return(gContains(spgeom,sp, byid = F))  # true, if points is inside boundaries
  },
  error=function(cond){
    print(paste0("Shapefiles for specie ", specie_name, " were not found."))
    return("NOT FOUND")
  })
}