#' @aliases vmaxlp
#' @title Estimate maximum one-way linear speed of a loop trip
#' @description Function to estimate the maximum one-way linear speed of a loop trip as described in 
#' \href{http://www.int-res.com/abstracts/meps/v457/p171-180/}{Shimada et al. (2012)}.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or 
#' \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param qi An integer specifying the minimum quality index associated with a location used for the estimation. 
#' Default is 4 (e.g. 4 GPS satellite or more).
#' @param prob A numeric value to specify a sample quantile. Default is 0.99.
#' @param ... Extra arguments passed to \code{\link{dupfilter}}.
#' @import sp
#' @importFrom raster pointDistance
#' @importFrom stats quantile
#' @export
#' @details The function first detects a "loop trip". 
#' Loop trip behaviour is represented by spatial departure and return involving more than 3 consecutive locations 
#' \href{http://www.int-res.com/abstracts/meps/v457/p171-180/}{(Shimada et al. 2012)}. 
#' The function calculates the net (i.e. straight-line) distance between the departure and turning point as well as 
#' the turning point and return location of a loop trip. 
#' It then calculates the one-way travelling speed to or from each turning point for each loop trip. 
#' The function discards extreme values, based on the specified quantile, to exclude potential outliers from the estimation process.
#' The maximum value in the retained dataset (i.e. without outliers) represents the maximum one-way linear speed at which 
#' an animal would travel during a loop trip.
#' @return Maximum one-way linear speed of a loop trip (vmaxlp) estimated from the input data. The unit km/h.
#' @author Takahiro Shimada
#' @note The input data must not contain temporal or spatial duplicates. A minimum of 8 locations are required.
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening. 
#' \emph{Marine Ecology Progress Series} 457:171-180 \doi{10.3354/meps09747}
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter_loop}}, \code{\link{track_param}}, \code{\link{dupfilter}}


vmaxlp<-function(sdata, qi=4, prob=0.99, ...){
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
  sdata <- track_param(sdata, param = c('time', 'distance', 'speed', 'angle'))
  
  
  #### Exclude datasets less than 8 locations
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
  
  
  ## Get movement parameters
  sdata <- track_param(sdata, param = c('time', 'distance', 'speed'))
  
  
  #### Retain locations with more than two consecutive points 
  sdata$npoints<-unlist(with(sdata, tapply(rownames, id, function(x) c(diff(x),NA))))
  Vlp<-with(sdata, sdata[startEnd3==1 & npoints>2, "sSpeed"])
  
  
  #### Maximum Vlp given # percentile considered outliers
  MaxVlp<-stats::quantile(Vlp, prob)
  
  
  #### Report the results
  SampleSize<-round(length(Vlp)*prob)
  LoopTrips<-round(SampleSize/2)
  cat("\n")
  cat("The maximum one-way linear speed of a loop trip (vmaxlp) was estimated using", SampleSize, "Vlp from", LoopTrips, "loop trips.", fill = TRUE)
  cat("vmaxlp:", round(MaxVlp,1), "km/h", fill = TRUE)
  if(length(id.exclude)>0){
    message('Warning: insufficient data to estimate vlp from:')
    message(paste(id.exclude, collapse = ', '))
  }

  
  #### Maximum Vlp given # percentile considered outliers
  return(MaxVlp)
}
  

