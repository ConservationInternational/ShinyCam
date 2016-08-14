get_KDE_polygons <- function(dat, bandwidth=0.25, gridsize=51){
  kde <- bkde2D(dat[ , c("Longitude", "Latitude")],
                bandwidth=c(bandwidth, bandwidth),
                gridsize=c(gridsize, gridsize))
  CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
  
  ## EXTRACT CONTOUR LINE LEVELS
  LEVS <- as.factor(sapply(CL, `[[`, "level"))
  NLEV <- length(levels(LEVS))
  
  ## CONVERT CONTOUR LINES TO POLYGONS
  pgons <- lapply(1:length(CL), function(i)
    Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
  spgons = SpatialPolygons(pgons)
  
  return(list(poly=spgons, levs=LEVS, nlev=NLEV))
  
} 
