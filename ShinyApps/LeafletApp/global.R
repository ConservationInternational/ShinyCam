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

isPointInBoundaries <- function(sp, spgeom, specie_name){
  tryCatch({
    spgeom= unionSpatialPolygons(spgeom, 1)  # instead of 1, it was spgeom$rowNo
    # check if it is inside or outside the specie boundary
    return(gContains(spgeom,sp, byid = T))  # true, if points is inside boundaries
  },
  error=function(cond){
    print(paste0("Shapefiles for specie ", specie_name, " were not found."))
    # FIX later: if the specie is not in the shapefiles, then don't show alert
    return("NOT FOUND")
  })
}