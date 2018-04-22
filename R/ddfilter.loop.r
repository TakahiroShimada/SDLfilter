#' @aliases ddfilter.loop
#' @title Filter locations by quality index, inner angle, and speed
#' @description A partial component of ddfilter, although works as a stand-alone function. 
#' This function removes locations by speed, inner angle, and quality index as described in Shimada et al. (2012).
#' @param sdata A data frame containing columns with the following headers: 
#' "id", "DateTime", "lat", "lon", "qi". This filter is independently applied to a subset of data grouped by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the numerical quality index associated with each fix where the greater number represents better quality 
#' (e.g. number of GPS satellites used for estimation).
#' @param qi An integer specifying threshold quality index during a loop trip. Default is 4 satellites.
#' @param ia An integer specifying threshold inner angle during a loop trip. Default is 90 degrees.
#' @param maxvlp A numeric value specifying threshold speed during a loop trip. Default is 1.8 km/h. 
#' If this value is unknown, the function "est.maxvlp" can be used to estimate the value based on the supplied data.
#' @import sp trip
#' @importFrom raster pointDistance
#' @export
#' @details This function removes locations if all of the following criteria apply: 
#' the number of source satellites are less than or equal to "qi", 
#' the inner angle is less than and equal to "ia" and the speed either from a previous or to a subsequent location exceeds "maxvlp". 
#' If "maxvlp" is unknown, it can be estimated using the function "est.maxvlp". 
#' @return A data frame is returned with the locations identified by this filter removed. 
#' The following columns are added: "pTime", "sTime", "pDist", "sDist", "pSpeed", "sSpeed", "inAng". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively. 
#' "pSpeed" and "sSpeed" are linear speed from a previous and to a subsequent fix respectively. 
#' "inAng" is the angle between the bearings of lines joining successive location points.    
#' @author Takahiro Shimada
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) Improving data retention and home range estimates by data-driven screening. 
#' Marine Ecology Progress Series 457:171-180 doi:10.3354/meps09747
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter.speed}}, \code{\link{est.maxvlp}}



# Hierarchical screening
ddfilter.loop<-function(sdata, qi=4, ia=90, maxvlp=1.8){
  
  OriginalSS<-nrow(sdata)
  
  for(k in 1:qi) {

      LP.filter<-function (sdata, qi, ia, maxvlp){
  
          #### Loop filter
          rm.maxLPSPD<-function(sdata, qi, ia, maxvlp) {
            
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
            
            
            ## Calculate inner angle in degree
            LatLong<-data.frame(Y=sdata$lat, X=sdata$lon, tms=sdata$DateTime, id=sdata$id)
            sp::coordinates(LatLong)<-~X+Y
            sp::proj4string(LatLong)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
            tr<-trip::trip(LatLong, c("tms", "id"))
            sdata$inAng<-trip::trackAngle(tr)
            
            
            ### Remove location according to qi, inner angle and max LP speed
            ##function to identify location to remove: (0 = remove, 1 = keep)
            pick.rm<-function(i){
              if(sdata$qi[i]<=qi && sdata$inAng[i]<ia && sdata$pSpeed[i]>maxvlp &&
                 (!is.na(sdata$qi[i])) && (!is.na(sdata$inAng[i])) && (!is.na(sdata$pSpeed[i]))){
                0
              } else if(sdata$qi[i]<=qi && sdata$inAng[i]<ia && sdata$sSpeed[i]>maxvlp &&
                        (!is.na(sdata$qi[i])) && (!is.na(sdata$inAng[i])) && (!is.na(sdata$sSpeed[i]))){
                0
              } else {
                1  
              }
            }
            
            ## Apply the above funtion to each data set seperately
            set.rm<-function(j){
              start<-as.numeric(rownames(sdata[sdata$id %in% j,][2,]))
              end<-as.numeric(rownames(sdata[sdata$id %in% j,][1,]))+(nrow(sdata[sdata$id %in% j,])-2)
              rm<-unlist(lapply(start:end, pick.rm))
              c(1, rm, 1)
            }
            
            sdata$overLpMax<-unlist(lapply(IDs, set.rm))
            
            sdata<-sdata[sdata$overLpMax==1,]
            
            
            #### Bring back excluded data
            if(nrow(excluded.data)>0){
                excluded.data[,c("pTime", "sTime", "pDist", "sDist", "pSpeed", "sSpeed", "inAng", "overLpMax")]<-NA
                sdata<-rbind(sdata, excluded.data)
            } else {
                sdata<-sdata
            }
          }


    #### Repeat the function until no locations can be removed by this filter
        sdata2<-rm.maxLPSPD(sdata, qi, ia, maxvlp)
        sdata3<-rm.maxLPSPD(sdata2, qi, ia, maxvlp)
        
        while(!(nrow(sdata2) %in% nrow(sdata3)))
        {
          sdata3<-rm.maxLPSPD(sdata2, qi, ia, maxvlp)
          sdata2<-rm.maxLPSPD(sdata3, qi, ia, maxvlp)
        }
          
        sdata<-sdata3
      }
  
    sdata<-LP.filter(sdata, qi=k, ia, maxvlp)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<4])
  
  ## Filtered data
  FilteredSS<-nrow(sdata)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  if(length(id.exclude)>0){
      cat("ddfilter.loop removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
      cat("  Warning: insufficient data to run ddfilter.loop for", id.exclude)
      cat("\n")
  } else {
      cat("ddfilter.loop removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
  }
  
 
  #### Delete working columns and return the output
  sdata$overLpMax<-NULL
  return(sdata)
}
