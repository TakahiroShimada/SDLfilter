#' @aliases kml_track
#' @title Generate KML from locations
#' @description Function to generate a kml file from tracking data. 
#' This is a wrapper of \code{\link[plotKML]{plotKML}} and \code{\link[plotKML]{kml}}, 
#' specifically designed to generate a kml file from tracking data.
#' @param sdata A data frame containing location data of \bold{one} individual, 
#' with the following column headers: "id", "DateTime", "lat", "lon". 
#' "id" is an identifier of the individual. 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' @param sdata.CRS Coordinate reference system (CRS) for the input location data.
#' If the input data is not in WGS, the specific CRS needs to be supplied as a PROJ.4 notation or EPSG code.
#' (e.g. "+proj=utm +zone=37 +datum=WGS84 +units=m +no_defs" or "EPSG:32637")  
#' @param output A string specifying whether to 'open' or 'save' the output. 
#' The output will be saved in the current working directory.
#' @param type Type of the output. 'point' or 'line.
#' @param ... Optional arguments passed to \code{\link[plotKML]{plotKML}} and \code{\link[plotKML]{kml}}.
#' @import sp
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



kml_track<-function(sdata, sdata.CRS = 'WGS', output = 'open', type = 'point', ...){
  
  sdata <- sdata[stats::complete.cases(sdata[,c("lon","lat")]),]
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))

  #### ID
  j <- levels(factor(sdata$id))
  
  
  #### Coordinate reference system
  if(sdata.CRS == 'WGS'){
    sdata.CRS <- '+proj=longlat +ellps=WGS84 +datum=WGS84'
  }
  
  
  #### output type
  if(type == 'point'){
    #### Points
    sp::coordinates(sdata) <- ~ lon + lat
    sp::proj4string(sdata) <- sp::CRS(sdata.CRS)
    
  } else {
    #### Lines
    lines <- sp::Line(cbind(sdata$lon, sdata$lat))
    lineString <- sp::Lines(list(lines), ID=j)
    spLines <- sp::SpatialLines(list(lineString), proj4string=sp::CRS(sdata.CRS))
    df.lines <- data.frame(ID=j)
    sdata <- sp::SpatialLinesDataFrame(spLines, data=df.lines, match.ID=F)
  }
  

  #### kml
  if(output == 'open'){
    plotKML::plotKML(obj=sdata, ...)
  } else {
    plotKML::kml(obj=sdata, ...)
  }
}