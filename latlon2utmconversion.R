



lonlatToUTM <- function(lon, lat, epsg_code = 32733){

  #' Args ------------------------------------------------------------------------------------------
  #'
  #' lon           | numeric vector, Decimal longitude
  #' lat           | numeric vector, Decimal latitude
  #' epsg_code     | integer, ESPG code for the UTM zone (codes searchable via rgdal::make_EPSG())
  #'
  #' -----------------------------------------------------------------------------------------------

  require(sp)
  require(rgdal)

  xy <- data.frame(ID = 1:length(lat), x = lon, y = lat)
  coordinates(xy) <- c("x", "y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
  res <- spTransform(xy, CRS(paste0("+init=epsg:", epsg_code)))

  return(as.data.frame(res))
}

# Attempt to rewrite into sf:
# Issues with output data type

#' # Rewritten for sf:
#' lonlatToUTM <- function(lon, lat, epsg_code = 32733){
#'   
#'   #' Args ------------------------------------------------------------------------------------------
#'   #' 
#'   #' lon           | numeric vector, Decimal longitude
#'   #' lat           | numeric vector, Decimal latitude
#'   #' epsg_code     | integer, ESPG code for the UTM zone (codes searchable via rgdal::make_EPSG())
#'   #' 
#'   #' -----------------------------------------------------------------------------------------------
#'   
#'   require(sf)
#'   
#'   print(length(lat))
#'   print(length(lon))
#'   
#'   xy <- st_as_sf(data.frame(ID = 1:length(lat), x = lon, y = lat), coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84")
#'   res <- st_coordinates(st_transform(xy, crs = paste0("+init=epsg:", epsg_code)))
#'   names(res) <- c("x", "y")
#'   
#' 
#'   return(as.data.frame(res))
#' }
