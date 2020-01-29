#' @aliases dupfilter_space
#' @title Filter spatial duplicates
#' @description Function to filter spatial duplicates in tracking data.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' The function filters the input data by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the numerical quality index associated with each location fix where the greater number indicates the higher accuracy 
#' (e.g. the number of GPS satellites involved in estimation).
#' @param step.time Consecutive locations less than or equal to \emph{step.time} apart are considered temporal duplicates.
#' Default is 0 hours.
#' @param step.dist Consecutive locations less than or equal to \emph{step.dist} apart are considered spatial duplicates.
#' Default is 0 kilometres. 
#' @param conditional If TRUE, spatial duplicates are filtered only if they are less than or equal to \emph{step.time} apart. 
#' Default is FALSE.
#' @import sp
#' @importFrom raster pointDistance
#' @export
#' @details This function is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function.
#' First it identifies spatial duplicates by searching for consecutive fixes that were located within \emph{step.dist}.
#' For each group of spatial duplicates, the function then retains a single fix that is the nearest from a previous and to a subsequent locations.
#' @note A minimum of two locations per id is required.
#' @return The input data frame is returned with spatial duplicates removed. 
#' The following columns are added: "pTime", "sTime", "pDist", "sDist". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively.
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 doi:\href{http://doi.org/10.1007/s00227-015-2771-0}{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_exact}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_qi}}


dupfilter_space<-function (sdata, step.time=0, step.dist=0, conditional=FALSE){
  
  #### Sample size for unfiltered data
  OriginalSS<-nrow(sdata)
  
  
  #### Function to filter spatial duplicates
  dup.location<-function (sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional) {
    #### Exclude data with less than 4 locations
    ndata<-table(sdata$id)
    id.exclude<-names(ndata[as.numeric(ndata)<3])
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
    
    
    #### Select a location from successive spatial duplicates by distance and time 
    ## middle section 1
    if(conditional %in% "TRUE"){
      # Function to select the one to remove (0 = remove, 1 = keep)
      pick.time<-function(i) {
        if(sdata$pDist[i]<=step.dist && sdata$pTime[i]<=step.time && sdata$qi[i]<sdata$qi[i-1] &&
           (!is.na(sdata$pDist[i])) && (!is.na(sdata$pTime[i])) && (!is.na(sdata$qi[i])) && (!is.na(sdata$qi[i-1]))) {
          0
        } else {
          1
        }
      }
      
    } else {
      pick.time<-function(i){
        if(sdata$pDist[i]<=step.dist && sdata$qi[i]<sdata$qi[i-1] &&
           (!is.na(sdata$pDist[i])) && (!is.na(sdata$qi[i])) && (!is.na(sdata$qi[i-1]))) {
          0
        } else {
          1
        }
      }
    }
      
    # Apply the above funtion to each data set seperately
    rm.time<-function(j){
      ini<-min(as.numeric(rownames(sdata[sdata$id %in% j,])))+1
      las<-max(as.numeric(rownames(sdata[sdata$id %in% j,])))
      rm<-c(1, unlist(lapply(ini:las, pick.time)))
    }
    
    sdata$rm<-unlist(lapply(IDs, rm.time))
    
    
    ## middle section 2
    if(conditional %in% "TRUE"){
      # Function to select the one to remove (0 = remove, 1 = keep)
      pick.time2<-function(i){
        if(sdata$sDist[i]<=step.dist && sdata$sTime[i]<=step.time && sdata$qi[i]<=sdata$qi[i+1] &&
           (!is.na(sdata$sDist[i])) && (!is.na(sdata$sTime[i])) && (!is.na(sdata$qi[i])) && (!is.na(sdata$qi[i+1]))) {
          0
        } else {
          1
        }
      }

    } else {
      pick.time2<-function(i){
        if(sdata$sDist[i]<=step.dist && sdata$qi[i]<=sdata$qi[i+1] &&
           (!is.na(sdata$sDist[i])) && (!is.na(sdata$qi[i])) && (!is.na(sdata$qi[i+1]))) {
          0
        } else {
          1
        }
      }
    }

    # Apply the above funtion to each data set seperately
    rm.time2<-function(j){
      ini<-min(as.numeric(rownames(sdata[sdata$id %in% j,])))
      las<-max(as.numeric(rownames(sdata[sdata$id %in% j,])))-1
      rm2<-c(unlist(lapply(ini:las, pick.time2)),1)
    }

    sdata$rm2<-unlist(lapply(IDs, rm.time2))
    
    
    #### Remove spatial duplicates
    sdata<-with(sdata, sdata[rm==1 & rm2==1,])
    
    
    #### Bring back excluded data
    if(nrow(excluded.data)>0){
      excluded.data[,c("pTime", "sTime", "pDist", "sDist", "rm", "rm2")]<-NA
      sdata<-rbind(sdata, excluded.data)
    } else {
      sdata<-sdata
    }
    
  }  
  
  
  #### Repeat the function until no locations can be removed by this filter
  sdata2<-dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional)
  sdata3<-dup.location(sdata=sdata2, step.time=step.time, step.dist=step.dist, conditional=conditional)
  while(!(nrow(sdata2) %in% nrow(sdata3)))
  {
    sdata3<-dup.location(sdata=sdata2, step.time=step.time, step.dist=step.dist, conditional=conditional)
    sdata2<-dup.location(sdata=sdata3, step.time=step.time, step.dist=step.dist, conditional=conditional)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<3])
  
  ## Filtered data
  FilteredSS<-nrow(sdata3)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  cat("dupfilter_space removed", RemovedSamplesN, "of", OriginalSS, "locations.", fill = TRUE)
  if(length(id.exclude)>0){
    message('Warning: dupfilter_space not applied to ', id.exclude, '. Insufficient data.')
  } 

  #### Delete working columns and return the output
  drops<-c("rm", "rm2")
  # drops<-c("rm", "rm2", "sTime", "sDist")
  sdata3<-sdata3[,!(names(sdata3) %in% drops)]
  return(sdata3)
}
