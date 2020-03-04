#' @aliases vmax
#' @title Estimate maximum linear speed
#' @description Function to estimate the maximum linear speed between two consecutive locations.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' "id" is the unique representing an individual. 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the numerical quality index associated with each location fix where a greater number indicates a higher accuracy 
#' (e.g. the number of GPS satellites involved in estimation).
#' @param qi An integer specifying the lowest quality index of a location that is qualified to be used in the estimation. 
#' Default is 5 (e.g. 5 GPS satellite or more).
#' @param prob A numeric value to specify a sample quantile. Default is 0.99.
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
#' @note The input data must not contain temporal or spatial duplicates.
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening. 
#' \emph{Marine Ecology Progress Series} 457:171-180 doi:\href{http://doi.org/10.3354/meps09747}{10.3354/meps09747}
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter_speed}}


vmax<-function(sdata, qi=5, prob=0.99){
  
  #### Organize data
  ## Subset data by quality index
  sdata<-sdata[sdata$qi>=qi,]
  
  
  ## Sort data in alphabetical and chronological order
  sdata<-with(sdata, sdata[order(id, DateTime),])
  row.names(sdata)<-1:nrow(sdata)
  
  
  ## Get Id of each animal
  IDs<-levels(factor(sdata$id))
  
  
  ## Hours from a previous and to a subsequent location (pTime & sTime)
  stepTime<-function(j){
      timeDiff<-diff(sdata[sdata$id %in% j, "DateTime"])
      units(timeDiff)<-"hours"
      c(as.numeric(timeDiff), NA)
  } 
  
  sTime<-unlist(lapply(IDs, stepTime))  
  sdata$pTime<-c(NA, sTime[-length(sTime)])
  sdata$sTime<-sTime
  
  
  ## Distance from a previous and to a subsequent location (pDist & sDist)
  for(j in IDs){
    # CUrrent location as "SpatialPoints"
    turtle<-sdata[sdata$id %in% j,]  
    LatLong<-data.frame(Y=turtle$lat, X=turtle$lon)
    sp::coordinates(LatLong)<-~X+Y
    sp::proj4string(LatLong)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    
    #pDist
    sdata[sdata$id %in% j,"pDist"]<-c(NA, raster::pointDistance(LatLong[-length(LatLong)], LatLong[-1], lonlat=T)/1000)
    
    #sDist
    sdata[sdata$id %in% j,"sDist"]<-c(raster::pointDistance(LatLong[-1], LatLong[-length(LatLong)], lonlat=T)/1000,NA)
  }     
  
  
  # Speed from a previous and to a subsequent location in km/h
  sdata$pSpeed<-sdata$pDist/sdata$pTime
  speed<-sdata$pSpeed[!(is.na(sdata$pSpeed))]
  
  
  #### Maximum speed given # percentile considered outliers
  Vmax<-stats::quantile(speed, prob)
  
  
  #### Report the results
  SampleSize<-round(nrow(sdata)*prob)
  cat("\n")
  cat("The maximum linear speed (Vmax) was estimated using", SampleSize, "locations.", fill = TRUE)
  cat("Vmax:", round(Vmax,1), "km/h", fill = TRUE)

  
  #### Maximum speed given # percentile considered outliers
  return(Vmax)
}

