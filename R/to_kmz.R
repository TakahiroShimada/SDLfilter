#' @aliases to_kmz
#' @title Generate KMZ from locations and track
#' @description Function to generate a kmz file from tracking data. 
#' This is a wrapper of \code{\link[plotKML]{kml}}, 
#' specifically designed to generate a kmz file from tracking data.
#' @param sdata A data frame containing location data of \bold{one} individual, 
#' with the following column headers: "id", "DateTime", "lat", "lon". 
#' "id" is an identifier of the individual. 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} 
#' or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' @param crs A number specifying the European Petroleum Survey Group (EPSG) code for the input location data.
#' @param file.name A character specifying the output file name. 
#' @param ... Optional arguments passed to \code{\link[plotKML]{kml_layer}} for points.
#' @importFrom sf st_as_sf st_set_crs st_cast
#' @importFrom plotKML kml_open kml_layer kml_close kml_compress
#' @importFrom stats complete.cases
#' @importFrom dplyr summarise
#' @export
#' @return A kmz file
#' @author Takahiro Shimada
#' @seealso \code{\link{map_track}}
#' @examples
#' ## Tracking data of two green turtles
#' data(turtle); data(turtle2)
#' 
#' ## Filter data
#' d1 <- ddfilter(dupfilter(turtle))
#' d2 <- ddfilter(dupfilter(turtle2))
#' 
#' ## Combine two data
#' d <- list(d1, d2) 
#' 
#' \dontrun{
#' ## Generate a kmz file from each tracking data
#' shp <- 'http://maps.google.com/mapfiles/kml/pal2/icon18.png'
#' for(i in 1:2){
#'   # labels for points (date and time)
#'   pn <- d[[i]]$DateTime
#'   
#'   # Apply a colour gradient to points based on the date and time
#'   pt_col <- hcl.colors(n = nrow(d[[i]]), palette = 'Zissou 1')
#'  
#'   # Generate a kmz file
#'   to_kmz(d[[i]], shape = shp, colour_scale = pt_col, colour = pn, points_names = pn, LabelScale = 0)
#'  }
#' }



to_kmz <- function(sdata, crs = 4326, file.name = 'id', ...){
  
  #### Input data
  sdata <- sdata[stats::complete.cases(sdata[,c("lon","lat")]),]
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  ## ID
  j <- levels(factor(sdata$id))
  
  ## File name
  if(file.name %in% 'id'){
    kml_name <- paste0(j, '.kml')
  } else {
    kml_name <- paste0(file.name, '.kml')
  }
  
  
  # Points
  pts <- sf::st_as_sf(sdata, coords = c("lon", "lat"))
  pts <- sf::st_set_crs(pts, crs)
  
  ## Lines
  ln <- dplyr::summarise(pts, do_union = F)
  ln <- sf::st_cast(ln, "LINESTRING")
  
  
  #### Generate a kmz file
  plotKML::kml_open(file.name = kml_name, folder.name = j, overwrite = TRUE)
  plotKML::kml_layer(subfolder.name = 'Track', obj = ln, width = 3, colour = '#bdbdbd')#lines_sldf
  plotKML::kml_layer(subfolder.name = 'Locations', obj = pts, ...)#, colour_scale = pt_col, colour = pn, points_names = pn,
  plotKML::kml_close(file.name = kml_name)
  plotKML::kml_compress(file.name = kml_name)
  unlink(kml_name)
}
