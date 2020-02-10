#' @aliases ddfilter_speed
#' @title Filter locations by speed 
#' @description A partial component of \code{\link{ddfilter}}, although works as a stand-alone function. 
#' This function removes locations by a given threshold speed as described in Shimada et al. (2012).
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' The function filters the input data by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the numerical quality index associated with each location fix where the greater number indicates the higher accuracy 
#' (e.g. number of GPS satellites used for estimation).
#' @param vmax A numeric value specifying a threshold of speed from a previous and/or to a subsequent fix. 
#' Default is 8.9km/h. 
#' If this value is unknown, it can be estimated from \emph{sdata} using the function \code{\link{vmax}}.
#' @param method An integer specifying how locations should be filtered with \emph{vmax}. 
#' A location is removed if the speed from a previous and(1)/or(2) to a subsequent location exceeds \emph{vmax}. 
#' Default is 1 (both way).
#' @import sp
#' @importFrom raster pointDistance
#' @export
#' @details This function removes locations if the speed from a previous and/or to a subsequent location exceeds a given threshold speed. 
#' If \emph{vmax} is unknown, it can be estimated using the function \code{\link{vmax}}.
#' @return The input data is returned without locations identified by this filter. 
#' The following columns are added: "pTime", "sTime", "pDist", "sDist", "pSpeed", "sSpeed". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively. 
#' "pSpeed" and "sSpeed" are linear speed from a previous and to a subsequent fix respectively. 
#' @author Takahiro Shimada
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening.
#' \emph{Marine Ecology Progress Series} 457:171-180 doi:\href{http://doi.org/10.3354/meps09747}{10.3354/meps09747}
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter_loop}}, \code{\link{vmax}}



ddfilter_speed<-function (sdata, vmax=8.9, method=1){
  
  OriginalSS<-nrow(sdata)
    
    max.speed<-function(sdata=sdata, vmax=vmax,method=method){
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
      
      
    
      # Speed from a previous and to a subsequent location in km/h
      sdata$pSpeed<-sdata$pDist/sdata$pTime
      sdata$sSpeed<-sdata$sDist/sdata$sTime
    
    
      # Select locations at which the speed from a previous and to a subsequent location exceeds maximum linear traveling speed (Vmax)
      ## Function to identify location to remove: (0 = remove, 1 = keep)
      if(method==1){
        overMax<-function(i)
        if(sdata$pSpeed[i]>vmax && sdata$sSpeed[i]>vmax && (!is.na(sdata$pSpeed[i])) && (!is.na(sdata$sSpeed[i]))){
          0
        } else {
          1
        }
      } else if (method==2) {
        overMax<-function(i)
        if((sdata$pSpeed[i]>vmax | sdata$sSpeed[i]>vmax) && (!is.na(sdata$pSpeed[i])) && (!is.na(sdata$sSpeed[i]))){
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
        excluded.data[,c("pTime", "sTime", "pDist", "sDist", "pSpeed", "sSpeed", "overMax")]<-NA
        sdata<-rbind(sdata, excluded.data)
      } else {
        sdata<-sdata
      }
    }
      

  
  # Repeat the above function until no locations can be removed by this filter.
  sdata2<-max.speed(sdata=sdata, vmax=vmax, method=method)
  sdata3<-max.speed(sdata=sdata2, vmax=vmax, method=method)
  while(!(nrow(sdata2) %in% nrow(sdata3)))
  {
    sdata3<-max.speed(sdata=sdata2, vmax=vmax, method=method)
    sdata2<-max.speed(sdata=sdata3, vmax=vmax, method=method)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<4])
    
  ## Filtered data
  FilteredSS<-nrow(sdata3)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  cat("ddfilter_speed removed", RemovedSamplesN, "of", OriginalSS, "locations.", fill = TRUE)
  if(length(id.exclude)>0){
    message('Warning: ddfilter_speed not applied to the following data. Insufficient data.')
    message(paste(id.exclude, collapse = ', '))
  }
  # if(length(id.exclude)>0){
  #     cat('ddfilter.speed removed', RemovedSamplesN, 'of', OriginalSS, 'locations')
  #     # cat("\n")
  #     message('insufficient data to run ddfilter.speed for', id.exclude)
  #     # cat("\n")
  # } else {
  #     cat("ddfilter.speed removed", RemovedSamplesN, "of", OriginalSS, "locations")
  #     # cat("\n")
  # }

  
  # Delete working columns and return the output
  sdata3$overMax<-NULL
  # drops<-c("overMax", "sTime", "sDist", "pSpeed", "sSpeed")
  # sdata3<-sdata3[,!(names(sdata3) %in% drops)] 
  return(sdata3)
}
