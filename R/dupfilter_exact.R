#' @aliases dupfilter_exact
#' @title Filter temporally and spatially exact duplicates
#' @description Function to filter temporally and spatially exact locations in tracking data.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' The function filters the input data by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the numerical quality index associated with each location fix where a greater number indicates a higher accuracy 
#' (e.g. the number of GPS satellites involved in estimation).
#' @import sp
#' @importFrom raster pointDistance
#' @importFrom plyr rbind.fill
#' @export
#' @details This is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function.
#' It looks for temporally and spatially exact locations and retains only a single fix (latitude/longitude pair) per time and location. 
#' @return The input data frame is returned with temporally and spatially exact duplicates removed. 
#' The following columns are added: "pTime", "sTime", "pDist", "sDist". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively.
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 doi:\href{http://doi.org/10.1007/s00227-015-2771-0}{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_qi}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_space}}


  
dupfilter_exact<-function (sdata){
  
  #### Sample size for unfiltered data
  OriginalSS<-nrow(sdata)
  
  
  #### Function to filter temporally and spatially exact duplicates 
  dup.exact <- function (sdata=sdata) {
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
      
      # ## Hours from a previous and to a subsequent location (pTime & sTime)
      # stepTime<-function(j){
      #     timeDiff<-diff(sdata[sdata$id %in% j, "DateTime"])
      #     units(timeDiff)<-"hours"
      #     c(as.numeric(timeDiff), NA)
      # } 
      # 
      # sTime<-unlist(lapply(IDs, stepTime))  
      # sdata$pTime<-c(NA, sTime[-length(sTime)])
      # sdata$sTime<-sTime
      # 
      # 
      # ## Distance from a previous and to a subsequent location (pDist & sDist)
      # # Function to calculate distances
      # calcDist<-function(j){
      #   turtle<-sdata[sdata$id %in% j,]  
      #   LatLong<-data.frame(Y=turtle$lat, X=turtle$lon)
      #   sp::coordinates(LatLong)<-~X+Y
      #   sp::proj4string(LatLong)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      #   
      #   #pDist
      #   c(NA, raster::pointDistance(LatLong[-length(LatLong)], LatLong[-1], lonlat=T)/1000)
      # }
      # 
      # sdata$pDist<-unlist(lapply(IDs, calcDist))
      # sdata$sDist<-c(sdata$pDist[-1], NA)
      sdata <- track_param(sdata, param = c('time', 'distance'))
      
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
        # excluded.data[,c("pTime", "sTime", "pDist", "sDist", "rm", "rm2", "rm3")]<-NA
        sdata <- plyr::rbind.fill(sdata, excluded.data)
      } else {
        sdata<-sdata
      }
      
  }  
  
  
  #### Repeat the function until no locations can be removed by this filter
  sdata2<-dup.exact(sdata=sdata)
  sdata3<-dup.exact(sdata=sdata2)
  while(!(nrow(sdata2) %in% nrow(sdata3)))
  {
    sdata3<-dup.exact(sdata=sdata2)
    sdata2<-dup.exact(sdata=sdata3)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<3])
  
  ## Filtered data
  FilteredSS<-nrow(sdata3)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  cat("dupfilter_exact removed", RemovedSamplesN, "of", OriginalSS, "locations.", fill = TRUE)
  if(length(id.exclude)>0){
    message('Warning: dupfilter_exact not applied to the following data. Insufficient data.')
    message(paste(id.exclude, collapse = ', '))
  }
  
  #### Delete working columns and return the output
  drops<-c("rm", "rm2", "rm3")
  # drops<-c("rm", "rm2", "rm3", "sTime", "sDist")
  sdata3<-sdata3[,!(names(sdata3) %in% drops)] 
  return(sdata3)
}
