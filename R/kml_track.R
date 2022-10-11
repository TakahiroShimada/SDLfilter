#' @aliases kml_track
#' @title Generate KML from locations
#' @description Function to generate a kml file from tracking data. 
#' This is a wrapper of \code{\link[plotKML]{plotKML}} and \code{\link[plotKML]{kml}}, 
#' specifically designed to generate a kml file from tracking data.
#' @param sdata A data frame containing location data of \bold{one} individual, 
#' with the following column headers: "id", "DateTime", "lat", "lon". 
#' "id" is an identifier of the individual. 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} 
#' or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' @param crs A number specifying the European Petroleum Survey Group (EPSG) code for the input location data.  
#' @param output A string specifying whether to 'open' or 'save' the output. 
#' The output will be saved in the current working directory.
#' @param type Type of the output. 'point' or 'line.
#' @param ... Optional arguments passed to \code{\link[plotKML]{plotKML}} and \code{\link[plotKML]{kml}}.
#' @importFrom dplyr summarise
#' @importFrom sf st_as_sf st_set_crs st_cast
#' @importFrom plotKML plotKML kml
#' @importFrom stats complete.cases
#' @export
#' @return A kml file
#' @author Takahiro Shimada
#' @seealso \code{\link{map_track}}
#' @examples
#' #### Fastloc GPS data obtained from a green turtle
#' data(turtle)
#' 
#' \dontrun{
#' #### See the data on Google earth
#' ## points with time stamps
#' kml_track(turtle, output = 'open', type = 'point', 
#' points_names = turtle$DateTime, colour_scale = 'yellow')
#' 
#' ## lines
#' kml_track(turtle, output = 'open', type = 'line', colour = 'red')
#'  
#'
#' #### Save the location points to the current working directory    
#' shape <- "http://maps.google.com/mapfiles/kml/pal2/icon26.png"
#' kml_track(turtle, output = 'save', type = 'point', shape = shape, colour = 'yellow')
#' }



kml_track <- function(sdata, crs = 4326, output = 'open', type = 'point', ...){
  
  sdata <- sdata[stats::complete.cases(sdata[,c("lon","lat")]),]
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))

  #### ID
  j <- levels(factor(sdata$id))
  
  
  #### output type
  plt <- sf::st_as_sf(sdata, coords = c("lon", "lat"))
  plt <- sf::st_set_crs(plt, crs)
  
  if(type == 'line'){
    plt <- dplyr::summarise(plt, do_union = F)
    plt <- sf::st_cast(plt, "LINESTRING")
  }
  

  #### kml
  if(output == 'open'){
    plotKML::plotKML(obj = plt, ...)
  } else {
    plotKML::kml(obj = plt, ...)
  }
}