#' @aliases est.maxvlp
#' @title Estimate maximum one-way linear speed of a loop trip
#' @description This function estimates the maximum one-way linear speed of a loop trip as described in Shimada et al. (2012).
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' This filter is independently applied to a subset of data grouped by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the numerical quality index associated with each fix where the greater number represents better quality 
#' (e.g. number of GPS satellites used for estimation).
#' @param qi An integer specifying the minimum quality index associated with a location used for the estimation. Default is 4.
#' @param prob A numeric value specifying a probability to obtain sample quantiles. Default is 0.99.
#' @import sp
#' @importFrom raster pointDistance
#' @importFrom stats quantile
#' @export
#' @details The function first detects a "loop trip". 
#' Loop trip behaviour is represented by spatial departure and return with more than 3 consecutive locations (Shimada et al 2012). 
#' It then calculates the net (i.e. straight-line) distance between the departure and turning location as well as 
#' the turning and return location of the loop trip, and from that calculated the net speed in and out. 
#' It discards extreme values based on the quantile specified by a user (default is 0.99). 
#' This is to exclude outliers potentially contained in the original data set. 
#' The maximum value in the retained dataset (i.e. without outliers) represents the maximum one-way linear speed at which 
#' an animal would travel during a loop trip. A minimum of 8 locations are required to run this function.
#' @return A vector is returned. The unit is in kilometres per hour. 
#' @author Takahiro Shimada
#' @note Input data must not contain temporal or spatial duplicates.
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening. 
#' Marine Ecology Progress Series 457:171-180 doi:10.3354/meps09747
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter.loop}}


est.maxvlp<-function(sdata, qi=4, prob=0.99){
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
  calcDist<-function(j){
      turtle<-sdata[sdata$id %in% j,]  
      LatLong<-data.frame(Y=turtle$lat, X=turtle$lon)
      sp::coordinates(LatLong)<-~X+Y
      sp::proj4string(LatLong)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      
      #pDist
      c(NA, raster::pointDistance(LatLong[-length(LatLong)], LatLong[-1], lonlat=T)/1000)
  }
  
  sdata$pDist<-unlist(lapply(IDs, calcDist))
  sdata$sDist<-c(sdata$pDist[-1], NA)
  
  
  # Speed from a previous and to a subsequent location in km/h
  sdata$pSpeed<-sdata$pDist/sdata$pTime
  sdata$sSpeed<-sdata$sDist/sdata$sTime
  
  
  ## Calculate inner angle in degree
  LatLong<-data.frame(Y=sdata$lat, X=sdata$lon, tms=sdata$DateTime, id=sdata$id)
  sp::coordinates(LatLong)<-~X+Y
  sp::proj4string(LatLong)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  tr<-trip::trip(LatLong, c("tms", "id"))
  sdata$inAng<-trip::trackAngle(tr)
  sdata<-with(sdata, sdata[complete.cases(inAng),])
  
  
  #### Exclude datasets less than 6 locations
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<8])
  sdata<-with(sdata, sdata[!id %in% id.exclude,])

  ## Renew IDs and row names
  IDs<-levels(factor(sdata$id))
  row.names(sdata)<-1:nrow(sdata)
  
 
  #### Identify loop trips
  ## continuous straight movements 
  # function to identify straight movements: (1=straight, 0=curve)
  straight<-function(i){
    if (sdata$inAng[i]>90){
      1
    } else if (sdata$inAng[i-2]>90 && sdata$inAng[i-1]>90 && sdata$inAng[i]<90 && sdata$inAng[i+1]<90 && sdata$inAng[i+2]>90){
      1
    } else if(sdata$inAng[i-2]>90 && sdata$inAng[i-1]<90 && sdata$inAng[i]<90 && sdata$inAng[i+1]>90 && sdata$inAng[i+2]>90){
      1 
    } else {
      0
    }
  }
  
  # Apply the above funtion to each data set seperately
  straight.group<-function(j){
    start<-as.numeric(rownames(sdata[sdata$id %in% j,][4,]))
    end<-as.numeric(rownames(sdata[sdata$id %in% j,][1,]))+(nrow(sdata[sdata$id %in% j,])-4)
    group<-unlist(lapply(start:end, straight))
    c(3, 3, 3, group, 3, 3, 3)
  }
  
  sdata$straightMove<-unlist(lapply(IDs, straight.group))

  
  ## Identify the first and last points of each straight movement 
  # function to identify start and end points: (1=start, 2=end, others=3)
  start.straight<-function(i){
    if(sdata$straightMove[i]==0 && sdata$straightMove[i+1]==1){
      1
    } else if (sdata$straightMove[i]==0 && sdata$straightMove[i-1]==1){
      2
    } else {
      3
    }
  }
  
  
  # Apply the above funtion to each data set seperately
  start.straight.group<-function(j){
    start<-as.numeric(rownames(sdata[sdata$id %in% j,][2,]))
    end<-as.numeric(rownames(sdata[sdata$id %in% j,][1,]))+(nrow(sdata[sdata$id %in% j,])-2)
    group<-unlist(lapply(start:end, start.straight))
    c(1, group, 2)
  }
  
  sdata$startEnd<-unlist(lapply(IDs, start.straight.group))
  
  
  ## Remove points with 3
  sdata<-sdata[sdata$startEnd<3,]
  sdata$rownames<-as.numeric(rownames(sdata))
  row.names(sdata)<-1:nrow(sdata)
  
  
  ## Extract start and end points (except first and last points of each data set)
  # function to identify actual start and end points: (1=start, 2=end, others=3)
  start.end<-function(i){
    if(sdata$startEnd[i]==1 && sdata$startEnd[i+1]==1){
      3
    } else if (sdata$startEnd[i]==1 && sdata$startEnd[i+1]==2){
      1
    } else if (sdata$startEnd[i]==2 && sdata$startEnd[i-1]==1) {
      2
    } else {
      3
    }
  }
  
  
  # Apply the above funtion to each data set seperately
  start.end.group<-function(j){
    sdataTEMP<-sdata[sdata$id %in% j,]
    rowNumbers<-as.numeric(rownames(sdataTEMP[c(-1, -nrow(sdataTEMP)),]))
    group<-unlist(lapply(rowNumbers, start.end))
    c(1, group, 2)
  }
  
  sdata$startEnd2<-unlist(lapply(IDs, start.end.group))
  
  
  ## Remove points with 3
  sdata<-sdata[sdata$startEnd2<3,]
  row.names(sdata)<-1:nrow(sdata)
  
  
  ## Extract start and end points (First point of each data set)
  # function to identify actual start and end points: (1=start, 2=end, others=3)
  FirstPoint<-function(i){
    if(sdata$startEnd[i]==1 && sdata$startEnd[i+1]==1){
      3
    } else {
      1
    }
  }
  
  LastPoint<-function(i){
    if(sdata$startEnd[i]==2 && sdata$startEnd[i-1]==2){
      3
    } else {
      2
    }
  }
  
  
  # Apply the above funtion to each data set seperately
  FirstEndPoints.group<-function(j){
    sdataTEMP<-sdata[sdata$id %in% j,]
    
    FirstRow<-as.numeric(rownames(sdataTEMP[1,]))
    FirstStatus<-unlist(lapply(FirstRow, FirstPoint))
    
    LastRow<-as.numeric(rownames(sdataTEMP[nrow(sdataTEMP),]))
    LastStatus<-unlist(lapply(LastRow, LastPoint))
    
    
    c(FirstStatus, sdataTEMP[c(-1, -nrow(sdataTEMP)), "startEnd2"], LastStatus)
  }
  
  sdata$startEnd3<-unlist(lapply(IDs, FirstEndPoints.group))
  
  
  ## Remove points with 3
  sdata<-sdata[sdata$startEnd3<3,]
  row.names(sdata)<-1:nrow(sdata)
  
  
  #### Calculate speed
  sTime<-unlist(lapply(IDs, stepTime))  
  sdata$pTime<-c(NA, sTime[-length(sTime)])
  sdata$sTime<-sTime
  
  
  ## Distance from a previous and to a subsequent location (pDist & sDist)
  sdata$pDist<-unlist(lapply(IDs, calcDist))
  sdata$sDist<-c(sdata$pDist[-1], NA)
  
  
  ## Speed from a previous and to a subsequent location in km/h
  sdata$pSpeed<-sdata$pDist/sdata$pTime
  sdata$sSpeed<-sdata$sDist/sdata$sTime
  
  
  #### Retain locations with more than two consecutive points 
  sdata$npoints<-unlist(with(sdata, tapply(rownames, id, function(x) c(diff(x),NA))))
  Vlp<-with(sdata, sdata[startEnd3==1 & npoints>2, "sSpeed"])
  
  
  #### Maximum Vlp given # percentile considered outliers
  MaxVlp<-stats::quantile(Vlp, prob)
  
  
  #### Report the results
  SampleSize<-round(length(Vlp)*prob)
  LoopTrips<-round(SampleSize/2)
  cat("\n")
  cat("The maximum one-way linear speed of a loop trip (Max.Vlp) was estimated using", SampleSize, "Vlp from", LoopTrips, "loop trips")
  cat("\n")
  cat("Max.Vlp:", round(MaxVlp,1), "km/h")
  cat("\n\n")
  cat("  Warning: insufficient data to detect a loop trip from", id.exclude)
  cat("\n\n")
  
  
  #### Maximum Vlp given # percentile considered outliers
  return(MaxVlp)
}
  

