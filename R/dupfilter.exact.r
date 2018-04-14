#' @aliases dupfilter.exact
#' @title Filter temporally and spatially exact duplicates
#' @description A partial component of dupfilter, although works as a stand-alone function. 
#' This function removes temporally and spatially exact duplicates. 
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' This filter is independently applied to a subset of data grouped by the unique "id". 
#' "DateTime" is date & time in class POSIXct. "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the numerical quality index associated with each fix where the greater number represents better quality 
#' (e.g. number of GPS satellites used for estimation).
#' @import sp raster
#' @export
#' @details This function selects a fix from multiple fixes, which were simultaneously obtained at the same geographical coordinate.
#' @return Input data frame is returned with temporally and spatially exact duplicates removed. 
#' The following columns are added: "pTime", "sTime", "pDist", "sDist". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively.
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' Marine Biology 163:1-14 doi:10.1007/s00227-015-2771-0
#' @seealso dupfilter, dupfilter.qi, dupfilter.time, dupfilter.space


  
dupfilter.exact<-function (sdata){
  
  #### Sample size for unfiltered data
  OriginalSS<-nrow(sdata)
  
  
  #### Function to filter temporally and spatially exact duplicates 
  dup.exact <- function (sdata) {
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

      
      ### Identify duplicates (exact time and location)
      ## Function to identify exact duplicates: (0 = remove, 1 = keep)
      pick.dup<-function(i)
      {
        if(sdata$sTime[i]==0 && sdata$pTime[i+1]==0 && sdata$sDist[i]==0 && sdata$pDist[i+1]==0 && sdata$qi[i]==sdata$qi[i+1] && 
           (!is.na(sdata$sTime[i])) && (!is.na(sdata$pTime[i+1])) && (!is.na(sdata$sDist[i])) && (!is.na(sdata$pDist[i+1])) && 
           (!is.na(sdata$qi[i]))&& (!is.na(sdata$qi[i+1]))) {
          0
        } else {
          1
        }
      }
      
      # Apply the above funtion to each data set seperately
      apply.dup<-function(j){
        ini<-as.numeric(rownames(sdata[sdata$id %in% j,][1,]))
        las<-as.numeric(rownames(sdata[sdata$id %in% j,][1,]))+(nrow(sdata[sdata$id %in% j,])-2)
        
        dup<-unlist(lapply(ini:las, pick.dup))
        c(dup,1)
      }
      
      sdata$rm<-unlist(lapply(IDs, apply.dup))
      
      
      ## First two rows
      #Function to identify exact duplicates
      pick.dup<-function(i)
      {
          if(sdata$sTime[i]==0 && sdata$sDist[i]==0 && sdata$qi[i]==sdata$qi[i+1] && 
             (!is.na(sdata$sTime[i])) && (!is.na(sdata$sDist[i])) && (!is.na(sdata$qi[i])) && (!is.na(sdata$qi[i+1]))) {
              0
          } else {
              1
          }
      }
      
      # Apply the above funtion to each data set seperately
      apply.dup<-function(j){
          ini<-min(as.numeric(rownames(sdata[sdata$id %in% j,])))
          dup<-unlist(lapply(ini, pick.dup))
          c(dup,rep(1, (nrow(sdata[sdata$id %in% j,])-1)))
      }
      
      sdata$rm2<-unlist(lapply(IDs, apply.dup))
      
      
      ## Last two rows
      #Function to identify exact duplicates
      pick.dup<-function(i)
      {
          if(sdata$sTime[i]==0 && sdata$sDist[i]==0 && sdata$qi[i]==sdata$qi[i+1] && 
             (!is.na(sdata$sTime[i])) && (!is.na(sdata$sDist[i])) && (!is.na(sdata$qi[i])) && (!is.na(sdata$qi[i+1]))) {
              0
          } else {
              1
          }
      }
      
      # Apply the above funtion to each data set seperately
      apply.dup<-function(j){
          ini<-max(as.numeric(rownames(sdata[sdata$id %in% j,]))-1)
          dup<-unlist(lapply(ini, pick.dup))
          c(rep(1, (nrow(sdata[sdata$id %in% j,])-2)), dup, 1)
      }
      
      sdata$rm3<-unlist(lapply(IDs, apply.dup))
      
      
      sdata<-with(sdata, sdata[rm==1 & rm2==1 & rm3==1,])
      
      
      #### Bring back excluded data
      if(nrow(excluded.data)>0){
        excluded.data[,c("pTime", "sTime", "pDist", "sDist", "rm", "rm2", "rm3")]<-NA
        sdata<-rbind(sdata, excluded.data)
      } else {
        sdata<-sdata
      }
      
  }  
  
  
  #### Repeat the function until no locations can be removed by this filter
  sdata2<-dup.exact(sdata)
  sdata3<-dup.exact(sdata2)
  while(!(nrow(sdata2) %in% nrow(sdata3)))
  {
    sdata3<-dup.exact(sdata2)
    sdata2<-dup.exact(sdata3)
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
      cat("dupfilter.exact removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
      cat("  Warning: insufficient data to run dupfilter.exact for", id.exclude)
      cat("\n")
  } else {
      cat("dupfilter.exact removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
  }
  
  
  #### Delete working columns and return the output
  drops<-c("rm", "rm2", "rm3")
  sdata3<-sdata3[,!(names(sdata3) %in% drops)] 
  return(sdata3)
}
