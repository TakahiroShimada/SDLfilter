#' @aliases dupfilter_exact
#' @title Filter temporally and spatially exact duplicates
#' @description Function to filter temporally and spatially exact locations in tracking data.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @importFrom dplyr distinct
#' @export
#' @details This is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function.
#' It looks for temporally and spatially exact locations and retains only a single fix (latitude/longitude pair) per time and location. 
#' @return The input data frame is returned with temporally and spatially exact duplicates removed. 
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 doi:\href{http://doi.org/10.1007/s00227-015-2771-0}{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_qi}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_space}}, \code{\link{track_param}}

  
dupfilter_exact <- function(sdata){
  
  ## Original sample size
  OriginalSS <- nrow(sdata)
  
  ## qi format
  sdata <- within(sdata, {
    qi[qi %in% "A"] <- "-1"
    qi[qi %in% "B"] <- "-2"
    qi[qi %in% "Z"] <- "-3"
    qi <- as.numeric(as.character(qi))
  })
  
  ## Date & time
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  
  #### Filter temporally and spatially exact duplicates
  sdata <- dplyr::distinct(sdata, .data$id, .data$DateTime, .data$lat, .data$lon, .data$qi, .keep_all = TRUE)

  #### Report the summary of filtering
  ## Filtered data
  FilteredSS <- nrow(sdata)
  RemovedSamplesN <- OriginalSS-FilteredSS
  
  ## Print report
  cat("dupfilter_exact removed", RemovedSamplesN, "of", OriginalSS, "locations.", fill = TRUE)

  #### return the output
  return(sdata)
}
