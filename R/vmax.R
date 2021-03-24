#' @aliases vmax
#' @title Estimate maximum linear speed
#' @description Function to estimate the maximum linear speed between two consecutive locations.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} 
#' or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param qi An integer specifying the lowest quality index of a location that is qualified to be used in the estimation. 
#' Default is 5 (e.g. 5 GPS satellite or more).
#' @param prob A numeric value to specify a sample quantile. Default is 0.99.
#' @param ... Extra arguments passed to \code{\link{dupfilter}}.
#' @import sp
#' @importFrom raster pointDistance
#' @importFrom stats quantile
#' @export
#' @details The function first calculates the linear speed between each pair of two consecutive locations. 
#' It then discards extreme values, based on the specified quantile, to exclude potential outliers from the estimation process. 
#' The maximum value in the retained dataset (i.e. without outliers) represents the maximum linear speed at which 
#' an animal would travel between two consecutive locations.   
#' @return Maximum linear speed (vmax) estimated from the input data. The unit is km/h. 
#' @author Takahiro Shimada
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening. 
#' \emph{Marine Ecology Progress Series} 457:171-180 \doi{10.3354/meps09747}
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter_speed}}, \code{\link{track_param}}, \code{\link{dupfilter}}


vmax<-function(sdata, qi=5, prob=0.99, ...){
  
  #### Organize data
  ## qi format
  sdata <- within(sdata, {
    qi[qi %in% "A"] <- "-1"
    qi[qi %in% "B"] <- "-2"
    qi[qi %in% "Z"] <- "-3"
    qi <- as.numeric(as.character(qi))
  })
  
  ## Date & time
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  
  ## Subset data by quality index
  sdata<-sdata[sdata$qi>=qi,]
  
  
  ## Filter duplicate locations
  sdata <- dupfilter(sdata, ...)
  
  
  ## Get movement parameters
  sdata <- track_param(sdata, param = c('time', 'distance', 'speed'))
  
  
  #### Maximum speed given # percentile considered outliers
  speed<-sdata$pSpeed[!(is.na(sdata$pSpeed))]
  Vmax<-stats::quantile(speed, prob)
  
  
  #### Report the results
  SampleSize<-round(nrow(sdata)*prob)
  cat("\n")
  cat("The maximum linear speed (Vmax) was estimated using", SampleSize, "locations.", fill = TRUE)
  cat("Vmax:", round(Vmax,1), "km/h", fill = TRUE)

  
  #### Maximum speed given # percentile considered outliers
  return(Vmax)
}

