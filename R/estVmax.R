#' @aliases est.vmax
#' @title Estimate maximum linear speed
#' @description This function estimates the maximum linear speed between two consecutive locations as described in Shimada et al. (2012)
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' This filter is independently applied to a subset of data grouped by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the numerical quality index associated with each fix where the greater number represents better quality 
#' (e.g. number of GPS satellites used for estimation).
#' @param qi An integer specifying minimum quality index associated with a location used for the estimation. Default is 5.
#' @param prob numeric value of a probability to obtain sample quantiles. Default is 0.99.
#' @import sp
#' @importFrom raster pointDistance
#' @importFrom stats quantile
#' @export
#' @details The function first calculates the linear speeds between each pair of two consecutive locations. 
#' It discards extreme values based on the quantile specified by a user (default is 0.99). 
#' This is to deal with outliers potentially contained in the original data set. 
#' The maximum value in the retained dataset (i.e. without outliers) represents the maximum linear speed at which 
#' an animal would travel between two consecutive locations.   
#' @return A vector is returned. The unit is in kilometres per hour. 
#' @author Takahiro Shimada
#' @note Input data must not contain temporal or spatial duplicates.
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening. 
#' Marine Ecology Progress Series 457:171-180 doi:10.3354/meps09747
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter.speed}}


est.vmax<-function(sdata, qi=5, prob=0.99){
  
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
  cat("The maximum linear speed (Vmax) was estimated using", SampleSize, "locations")
  cat("\n")
  cat("Vmax:", round(Vmax,1), "km/h")
  cat("\n\n")
  
  
  #### Maximum speed given # percentile considered outliers
  return(Vmax)
}

