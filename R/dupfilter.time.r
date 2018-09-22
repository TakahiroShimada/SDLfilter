#' @aliases dupfilter.time
#' @title Filter temporal duplicates
#' @description A partial component of dupfilter, although works as a stand-alone function. 
#' This function removes temporal duplicates.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' This filter is independently applied to a subset of data grouped by the unique "id". "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the numerical quality index associated with each fix where the greater number represents better quality 
#' (e.g. number of GPS satellites used for estimation).
#' @param step.time A numeric value specifying temporal interval between two consecutive locations. Default is 0 hours. 
#' Locations are considered temporal duplicates if the temporal interval is less than or equal to the user specified value.
#' @import sp
#' @importFrom raster pointDistance
#' @importFrom geosphere distGeo
#' @export
#' @details This function removes temporal duplicates according to the total distance from a previous and to a subsequent location. 
#' A fix with a shorter sum distance is retained.
#' @return Input data frame is returned with temporal duplicates removed according to total distance from a previous and to a subsequent location. 
#' The following column is added: "pTime". "pTime" is hours from a previous fix.
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' Marine Biology 163:1-14 doi:10.1007/s00227-015-2771-0
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter.exact}}, \code{\link{dupfilter.qi}}, \code{\link{dupfilter.space}}



dupfilter.time<-function (sdata, step.time=0) {
  
  #### Sample size for unfiltered data
  OriginalSS<-nrow(sdata)
  
  #### Function to filter temporal duplicates
      dup.timing<-function (sdata, step.time) {
        #### Exclude data with less than 3 locations
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
        
        
        #### Select a location from multiple fixes obtained at the same time
        ### Get turtle location as "SpatialPoints"
        LatLong<-data.frame(Y=sdata$lat, X=sdata$lon)
        sp::coordinates(LatLong)<-~X+Y
        sp::proj4string(LatLong)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
        
        
        ### Sum distance from a previous and to a subsequent location (km.psl)
        ## Function to calculate distance (mid-sections 1)
        kmPSL1<-function(i) {
          if(sdata$pTime[i]<=step.time && (!is.na(sdata$pTime[i]))) {
            ID<-sdata[i, "id"]
            sdataSub<-sdata[sdata$id %in% ID,]
            AdjRow<-min(as.numeric(rownames(sdataSub)))-1
            
            timeDiff<-difftime(sdataSub$DateTime, sdata$DateTime[i], units="hours")
            nextLoc<-min(which(timeDiff %in% min(timeDiff[timeDiff>step.time])))+AdjRow
            lastLoc<-max(which(timeDiff %in% max(timeDiff[timeDiff<(-step.time)])))+AdjRow
            raster::pointDistance(LatLong[i], LatLong[nextLoc], lonlat=T)/1000 + raster::pointDistance(LatLong[i], LatLong[lastLoc], lonlat=T)/1000
          } else {
            NA
          }
        }
        
        ## Apply the above funtion to each animal seperately 
        applyPSL1<-function(j) {
          ini<-min(as.numeric(rownames(sdata[sdata$id %in% j & sdata$pTime>step.time,][-1,])))
          las<-max(as.numeric(rownames(sdata[sdata$id %in% j & sdata$pTime>step.time,][-1,])))-1
          if(ini<=las){
            km.psl1<-unlist(lapply(ini:las, kmPSL1))
            FirstRow<-min(as.numeric(rownames(sdata[sdata$id %in% j,])))
            LastRow<-max(as.numeric(rownames(sdata[sdata$id %in% j,])))
            c(rep(NA,ini-FirstRow), km.psl1, rep(NA,LastRow-las))
          } else {
            rep(NA, nrow(sdata[sdata$id %in% j,]))
          }
        }
        
        km.psl1<-unlist(lapply(IDs, applyPSL1))
        sdata$PSL1<-km.psl1  
        
        
        ##function to calculate distance (mid-sections 2)
        kmPSL2<-function(i) {
          if(sdata$sTime[i]<=step.time && (!is.na(sdata$sTime[i]))) {
            ID<-sdata[i, "id"]
            sdataSub<-sdata[sdata$id %in% ID,]
            AdjRow<-min(as.numeric(rownames(sdataSub)))-1
            
            timeDiff<-difftime(sdataSub$DateTime, sdata$DateTime[i], units="hours")
            nextLoc<-min(which(timeDiff %in% min(timeDiff[timeDiff>step.time])))+ AdjRow
            lastLoc<-max(which(timeDiff %in% max(timeDiff[timeDiff<(-step.time)])))+ AdjRow
            raster::pointDistance(LatLong[i], LatLong[lastLoc], lonlat=T)/1000 + raster::pointDistance(LatLong[i], LatLong[nextLoc], lonlat=T)/1000
          } else {
            NA
          }
        }
        
        ## Apply the above funtion to each animal seperately
        applyPSL2<-function(j) {
          lastRow<-nrow(sdata[sdata$id %in% j & sdata$sTime>step.time,])
          ini<-min(as.numeric(rownames(sdata[sdata$id %in% j & sdata$sTime>step.time,][-lastRow,])))+1
          las<-max(as.numeric(rownames(sdata[sdata$id %in% j & sdata$sTime>step.time,][-lastRow,])))
          if(ini<=las){
            km.psl2<-unlist(lapply(ini:las, kmPSL2))
            FirstRow<-min(as.numeric(rownames(sdata[sdata$id %in% j,])))
            LastRow<-max(as.numeric(rownames(sdata[sdata$id %in% j,])))
            c(rep(NA,ini-FirstRow), km.psl2, rep(NA,LastRow-las))
          } else {
            rep(NA, nrow(sdata[sdata$id %in% j,]))
          }
        }
        
        km.psl2<-unlist(lapply(IDs, applyPSL2))
        sdata$PSL2<-km.psl2    
        
        
        ### Identify duplicates in the first and last rows
        ## First rows
        # Calculate distance from each of first points to the next points. 
        # If more than one pont exist in the next set of locations, choose the one which has the smallest PSL.
        
        ## Function to identify location to remove
        kmPSL3<-function(i) {
          if((sdata$pTime[i]<=step.time && !(is.na(sdata$pTime[i]))) | sdata$sTime[i]<=step.time) {
            ID<-sdata[i, "id"]
            sdataSub<-sdata[sdata$id %in% ID,]
            
            timeDiff<-as.numeric(difftime(sdataSub$DateTime, sdata$DateTime[i], units="hours"))
            nextLocs<-sdataSub[which(timeDiff>=min(timeDiff[timeDiff>step.time]) & timeDiff<=(min(timeDiff[timeDiff>step.time])+step.time)),]
            
            if(!(is.na(min(nextLocs$PSL1))) | !(is.na(min(nextLocs$PSL2)))) {
              if(all(is.na(nextLocs$PSL1))){
                PLSmin<-min(nextLocs[nextLocs$PSL2 %in% min(nextLocs$PSL2, na.rm=T),"PSL2"],na.rm=T)
                bestNextLoc<-nextLocs[nextLocs$PSL2 %in% PLSmin,][1,]
              } else if (all(is.na(nextLocs$PSL2))) {
                PLSmin<-min(nextLocs[nextLocs$PSL1 %in% min(nextLocs$PSL1, na.rm=T),"PSL1"],na.rm=T)
                bestNextLoc<-nextLocs[nextLocs$PSL1 %in% PLSmin,][1,]
              } else {
                PLS1min<-min(nextLocs[nextLocs$PSL1 %in% min(nextLocs$PSL1, na.rm=T),"PSL1"],na.rm=T)
                PLS2min<-min(nextLocs[nextLocs$PSL2 %in% min(nextLocs$PSL2, na.rm=T),"PSL2"],na.rm=T)
                PLSmin<-min(PLS1min, PLS2min)
                if(PLS1min<PLS2min) {
                  bestNextLoc<-nextLocs[nextLocs$PSL1 %in% PLSmin,][1,]
                } else {
                  bestNextLoc<-nextLocs[nextLocs$PSL2 %in% PLSmin,][1,]
                }
              }
              bestNextLocIn<-as.numeric(rownames(bestNextLoc))
              raster::pointDistance(LatLong[i], LatLong[bestNextLocIn], lonlat=T)/1000
            } else if(is.na(min(nextLocs$PSL1)) && is.na(min(nextLocs$PSL2))) {
              FirstnextLocIn<-min(as.numeric(rownames(nextLocs)))
              LastnextLocIn<-max(as.numeric(rownames(nextLocs)))
              Distances<-geosphere::distGeo(LatLong[i], LatLong[FirstnextLocIn:LastnextLocIn], a=6378137, f=1/298.257223563)/1000
              min(Distances)
            } else {
              bestNextLocIn<-as.numeric(rownames(nextLocs))
              raster::pointDistance(LatLong[i], LatLong[bestNextLocIn], lonlat=T)/1000
            }
          } else {
            NA
          }
        }
        
        
        ## Apply the above funtion to each animal seperately 
        applyPSL3<-function(j) {
          ini<-min(as.numeric(rownames(sdata[sdata$id %in% j,][1,])))
          las<-min(as.numeric(rownames(sdata[sdata$id %in% j & sdata$pTime>step.time,][-1,])))-1
          km.psl3<-unlist(lapply(ini:las, kmPSL3))
          c(km.psl3, rep(NA,nrow(sdata[sdata$id %in% j,])-length(km.psl3)))
        }
        
        km.psl3<-unlist(lapply(IDs, applyPSL3))
        sdata$PSL3<-km.psl3  
        
        
        ## Last rows
        # Calculate distance from each of last points to the previous points. 
        # If more than one pont exist in the next set of locations, choose the one which has the smallest PSL.
        
        ## Function to identify location to remove
        kmPSL4<-function(i) {
          if((sdata$sTime[i]<=step.time && !(is.na(sdata$sTime[i]))) | sdata$pTime[i]<=step.time) {
            ID<-sdata[i, "id"]
            sdataSub<-sdata[sdata$id %in% ID,]
            
            timeDiff<-as.numeric(difftime(sdata$DateTime[i], sdataSub$DateTime, units="hours"))
            lastLocs<-sdataSub[which(timeDiff>=min(timeDiff[timeDiff>step.time]) & timeDiff<=(min(timeDiff[timeDiff>step.time])+step.time)),]
            
            if(!(is.na(min(lastLocs$PSL1))) | !(is.na(min(lastLocs$PSL2)))) {
              if(all(is.na(lastLocs$PSL1))){
                PLSmin<-min(lastLocs[lastLocs$PSL2 %in% min(lastLocs$PSL2, na.rm=T),"PSL2"],na.rm=T)
                bestLastLoc<-lastLocs[lastLocs$PSL2 %in% PLSmin,][1,]
              } else if (all(is.na(lastLocs$PSL2))) {
                PLSmin<-min(lastLocs[lastLocs$PSL1 %in% min(lastLocs$PSL1, na.rm=T),"PSL1"],na.rm=T)
                bestLastLoc<-lastLocs[lastLocs$PSL1 %in% PLSmin,][1,]
              } else {  
                PLS1min<-min(lastLocs[lastLocs$PSL1 %in% min(lastLocs$PSL1, na.rm=T),"PSL1"],na.rm=T)
                PLS2min<-min(lastLocs[lastLocs$PSL2 %in% min(lastLocs$PSL2, na.rm=T),"PSL2"],na.rm=T)
                PLSmin<-min(PLS1min, PLS2min)
                if(PLS1min<PLS2min) {
                  bestLastLoc<-lastLocs[lastLocs$PSL1 %in% PLSmin,][1,]
                } else {
                  bestLastLoc<-lastLocs[lastLocs$PSL2 %in% PLSmin,][1,]
                }
              }
                bestLastLocIn<-as.numeric(rownames(bestLastLoc))
                raster::pointDistance(LatLong[i], LatLong[bestLastLocIn], lonlat=T)/1000
             } else if(is.na(min(lastLocs$PSL1)) && is.na(min(lastLocs$PSL2))) {
                FirstLastLocIn<-min(as.numeric(rownames(lastLocs)))
                LastLastLocIn<-max(as.numeric(rownames(lastLocs)))
                Distances<-geosphere::distGeo(LatLong[i], LatLong[FirstLastLocIn:LastLastLocIn], a=6378137, f=1/298.257223563)/1000
                min(Distances)
             } else {
                bestLastLocIn<-as.numeric(rownames(lastLocs))
                raster::pointDistance(LatLong[i], LatLong[bestLastLocIn], lonlat=T)/1000
             }
            } else {
              NA
            }
          }

        
        ## Apply the above funtion to each animal seperately 
        applyPSL4<-function(j) {
          lastRow<-nrow(sdata[sdata$id %in% j & sdata$sTime>step.time,])
          ini<-max(as.numeric(rownames(sdata[sdata$id %in% j & sdata$sTime>step.time,][-lastRow,])))+1
          las<-max(as.numeric(rownames(sdata[sdata$id %in% j,])))
          km.psl4<-unlist(lapply(ini:las, kmPSL4))
          c(rep(NA, nrow(sdata[sdata$id %in% j,])-(las-ini+1)), km.psl4)
        }
        
        km.psl4<-unlist(lapply(IDs, applyPSL4))
        sdata$PSL4<-km.psl4 
        
       
        
        
        #### Identify locations to remove
        ### middle section 1 (km.psl1)
        ## Function to identify location to remove: (0 = remove, 1 = keep)
        pick.rm1<-function(i) {
          if(sdata$sTime[i]<=step.time && sdata$PSL2[i]>sdata$PSL1[i+1] &&
             (!is.na(sdata$sTime[i])) && (!is.na(sdata$PSL2[i])) && (!is.na(sdata$PSL1[i+1]))) {
            0
          } else {
            1
          }
        }
        
        ## Apply the above funtion to each data set seperately
        set.rm1<-function(j) {
          ini<-min(as.numeric(rownames(sdata[sdata$id %in% j,])))
          las<-max(as.numeric(rownames(sdata[sdata$id %in% j,])))
          unlist(lapply(ini:las, pick.rm1))
        }
        
        ## Insert remarks to a row to remove
        sdata$rm1<-unlist(lapply(IDs, set.rm1))
          
      
        ### middle section 2 (km.psl2)
        ## Function to identify location to remove
        pick.rm2<-function(i) {
          if(sdata$pTime[i]<=step.time && sdata$PSL1[i]>=sdata$PSL2[i-1] &&
             (!is.na(sdata$pTime[i])) && (!is.na(sdata$PSL1[i])) && (!is.na(sdata$PSL2[i-1]))) {
            0
          } else {
            1
          }
        }
        
        ## Apply the above funtion to each data set seperately
        set.rm2<-function(j) {
          ini<-min(as.numeric(rownames(sdata[sdata$id %in% j,])))
          las<-max(as.numeric(rownames(sdata[sdata$id %in% j,])))
          unlist(lapply(ini:las, pick.rm2))
        }
        
        sdata$rm2<-unlist(lapply(IDs, set.rm2))
        
        
        ## First locations (km.psl3)
        set.rm3<-function(j) {
            
          sdataSub<-sdata[sdata$id %in% j,]
          
          if(all(is.na(sdataSub$PSL3))){
            rm3<-rep(1,nrow(sdata[sdata$id %in% j,]))
            return(rm3)
          } else {
            PSL3min<-min(sdataSub[sdataSub$PSL3 %in% min(sdataSub$PSL3, na.rm=T),"PSL3"],na.rm=T)
            bestFirstLocs<-sdata[sdata$id %in% j & sdata$PSL3 %in% PSL3min,]
            otherFirstLocs<-sdata[sdata$id %in% j & sdata$PSL3 > PSL3min & !is.na(sdata$PSL3),]
            AdjRow<-min(as.numeric(rownames(sdataSub)))-1
            bestFirstLocsIn<-as.numeric(rownames(bestFirstLocs))-AdjRow
            bestFirstLocIn<-as.numeric(rownames(bestFirstLocs[1,]))-AdjRow
            otherFirstLocsIn<-as.numeric(rownames(otherFirstLocs))-AdjRow
            rm3<-rep(1,nrow(sdata[sdata$id %in% j,]))
            #rm3[bestFirstLocsIn[-bestFirstLocIn]]<-0
            #rm3[otherFirstLocsIn]<-0
            rm3[bestFirstLocsIn]<-0
            rm3[bestFirstLocIn]<-1
            rm3[otherFirstLocsIn]<-0
            return(rm3)
          }
        }
        
        sdata$rm3<-unlist(lapply(IDs, set.rm3))
        
        
        ## last locations (km.psl4)
        set.rm4<-function(j) {
          
          sdataSub<-sdata[sdata$id %in% j,]
          
          if(all(is.na(sdataSub$PSL4))){
            rm4<-rep(1,nrow(sdata[sdata$id %in% j,]))
            return(rm4)
          } else {
            PSL4min<-min(sdataSub[sdataSub$PSL4 %in% min(sdataSub$PSL4, na.rm=T),"PSL4"],na.rm=T)
            bestLastLocs<-sdata[sdata$id %in% j & sdata$PSL4 %in% PSL4min,]
            otherLastLocs<-sdata[sdata$id %in% j & sdata$PSL4 > PSL4min & !is.na(sdata$PSL4),]
            AdjRow<-min(as.numeric(rownames(sdataSub)))-1
            bestLastLocsIn<-as.numeric(rownames(bestLastLocs))-AdjRow
            bestLastLocIn<-as.numeric(rownames(bestLastLocs[1,]))-AdjRow
            otherLastLocsIn<-as.numeric(rownames(otherLastLocs))-AdjRow
            rm4<-rep(1,nrow(sdataSub))
            rm4[bestLastLocsIn]<-0
            rm4[bestLastLocIn]<-1
            rm4[otherLastLocsIn]<-0
            return(rm4)
          }
        }
        
        sdata$rm4<-unlist(lapply(IDs, set.rm4))
        
        #### Remove errorneous locations obtained at the same time
        sdata<-with(sdata, sdata[rm1==1 & rm2==1 & rm3==1 & rm4==1,])
        
        #### Bring back excluded data
        if(nrow(excluded.data)>0){
          excluded.data[,c("pTime", "sTime", "PSL1", "PSL2", "PSL3", "PSL4", "rm1", "rm2", "rm3", "rm4")]<-NA
          sdata<-rbind(sdata, excluded.data)
        } else {
          sdata<-sdata
        }
      }

  
  #### Repeat the function until no locations can be removed by this filter
  sdata2<-dup.timing(sdata, step.time)
  sdata3<-dup.timing(sdata2, step.time)
  while(!(nrow(sdata2) %in% nrow(sdata3))) 
  {
    sdata3<-dup.timing(sdata2, step.time)
    sdata2<-dup.timing(sdata3, step.time)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<3])
  
  ## Filtered data
  FilteredSS<-nrow(sdata3)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  
  ## Print report
  if(length(id.exclude)>0){
      cat("dupfilter.time removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
      cat("  Warning: insufficient data to run dupfilter.time for", id.exclude)
      cat("\n")
  } else {
      cat("dupfilter.time removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
  }
  
  
  #### Delete working columns and return the output
  drops<-c("PSL1", "PSL2", "PSL3", "PSL4", "rm1", "rm2", "rm3", "rm4", "sTime")
  sdata3<-sdata3[,!(names(sdata3) %in% drops)] 
  return(sdata3)
}
