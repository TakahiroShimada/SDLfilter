#' @aliases distance_filter
#' @title Filter locations by distance 
#' @description This function removes locations that are located beyond a given threshold distance.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' This filter is independently applied to a subset of data grouped by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the numerical quality index associated with each fix where the greater number represents better quality 
#' (e.g. number of GPS satellites used for estimation).
#' @param distance1 A numeric value specifying threshold distance from a previous fix. Default is 100 km. 
#' @param distance2 A numeric value specifying threshold distance to a subsequent fix. Default is 100 km. 
#' @param method An integer specifying how locations are filtered by distance. 
#' 1 = a location is removed if the distance EITHER from a previous and to a subsequent location exceeds a given threshold speed. 
#' 2 = a location is removed if the distance BOTH from a previous and to a subsequent location exceeds a given threshold speed. Default is FALSE.
#' @import sp
#' @importFrom raster pointDistance
#' @export
#' @details This function removes locations if the distance both/either from a previous and to a subsequent location exceeds a given threshold distance. 
#' @return A data frame is returned without locations identified by this filter. 
#' The following columns are added: "pTime", "pDist". 
#' "pTime" is hours from a previous fix. "pDist" is straight distance in kilometres from a previous fix. 
#' @author Takahiro Shimada



distance_filter<-function (sdata, distance1=100, distance2=100, method=2){
  
  OriginalSS<-nrow(sdata)
    
    max.distance<-function(sdata, distance1, distance2, method){
      #### Exclude data with less than 4 locations
      ndata<-table(sdata$id)
      id.exclude<-names(ndata[as.numeric(ndata)<4])
      excluded.data<-sdata[sdata$id %in% id.exclude,]
      sdata<-sdata[!(sdata$id %in% id.exclude),]
      
      
      #### Organize data
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
      # Function to calculate distances
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
      
      
    
      # Select locations at which the distance from a previous and to a subsequent location exceeds maximum linear distance
      ## Function to identify location to remove: (0 = remove, 1 = keep)
      if(method==2){
        overMax<-function(i)
        if(sdata$pDist[i]>distance1 && sdata$sDist[i]>distance2 && (!is.na(sdata$pDist[i])) && (!is.na(sdata$sDist[i]))){
          0
        } else {
          1
        }
      } else if (method==1) {
        overMax<-function(i)
        if((sdata$pDist[i]>distance1 | sdata$sDist[i]>distance2) && (!is.na(sdata$pDist[i])) && (!is.na(sdata$sDist[i]))){
          0
        } else {
          1
        }
      }
      
      ## Apply the above funtion to each data set seperately
      set.rm<-function(j){
        start<-as.numeric(rownames(sdata[sdata$id %in% j,][2,]))
        end<-as.numeric(rownames(sdata[sdata$id %in% j,][1,]))+(nrow(sdata[sdata$id %in% j,])-2)
        rm<-unlist(lapply(start:end, overMax))
        c(1, rm, 1)
      }
      
      sdata$overMax<-unlist(lapply(IDs, set.rm))
        
      sdata<-sdata[sdata$overMax==1,]
      
      
      #### Bring back excluded data
      if(nrow(excluded.data)>0){
        excluded.data[,c("pTime", "sTime", "pDist", "sDist", "overMax")]<-NA
        sdata<-rbind(sdata, excluded.data)
      } else {
        sdata<-sdata
      }
    }
      

  
  # Repeat the above function until no locations can be removed by this filter.
  sdata2<-max.distance(sdata, distance1, distance2, method)
  sdata3<-max.distance(sdata2, distance1, distance2, method)
  while(!(nrow(sdata2) %in% nrow(sdata3)))
  {
    sdata3<-max.distance(sdata2, distance1, distance2, method)
    sdata2<-max.distance(sdata3, distance1, distance2, method)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<4])
    
  ## Filtered data
  FilteredSS<-nrow(sdata3)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  if(length(id.exclude)>0){
      cat("distance_filter removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
      cat("  Warning: insufficient data to run distance_filter for", id.exclude)
      cat("\n")
  } else {
      cat("distance_filter removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
  }

  
  # Delete working columns and return the output
  drops<-c("overMax", "sTime", "sDist")
  sdata3<-sdata3[,!(names(sdata3) %in% drops)] 
  return(sdata3)
}
