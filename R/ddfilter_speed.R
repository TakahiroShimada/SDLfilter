#' @aliases ddfilter_speed
#' @title Filter locations by speed 
#' @description A partial component of \code{\link{ddfilter}}, although works as a stand-alone function. 
#' This function removes locations by a given threshold speed as described in Shimada et al. (2012).
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param vmax A numeric value specifying a threshold of speed from a previous and/or to a subsequent fix. 
#' Default is 8.9km/h. 
#' If this value is unknown, it can be estimated from \emph{sdata} using the function \code{\link{vmax}}.
#' @param method An integer specifying how locations should be filtered with \emph{vmax}. 
#' A location is removed if the speed from a previous and(1)/or(2) to a subsequent location exceeds \emph{vmax}. 
#' Default is 1 (both way).
#' @import sp
#' @importFrom raster pointDistance
#' @importFrom plyr rbind.fill
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
#' \emph{Marine Ecology Progress Series} 457:171-180 \doi{10.3354/meps09747}
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter_loop}}, \code{\link{vmax}}, \code{\link{track_param}}



ddfilter_speed<-function (sdata, vmax=8.9, method=1){
  
  ## Original columns
  # headers <- names(sdata)
  
  ## Original sample size
  OriginalSS <- nrow(sdata)
  
  ## qi format
  sdata <- within(sdata, {
    qi[qi %in% "A"] <- "-1"
    qi[qi %in% "B"] <- "-2"
    qi[qi %in% "Z"] <- "-3"
    qi <- as.numeric(as.character(qi))
  })
  
  ## Date & time
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  
    max.speed<-function(sdata=sdata, vmax=vmax, method=method){
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
      
      ## Get movement parameters
      sdata <- track_param(sdata, param = c('time', 'distance', 'speed'))
    
    
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
      
      ## Apply the above function to each data set separately
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
        # excluded.data[,c("pTime", "sTime", "pDist", "sDist", "pSpeed", "sSpeed", "overMax")]<-NA
        sdata <- plyr::rbind.fill(sdata, excluded.data)
      } else {
        sdata <- sdata
      }
    }
      

  
  # Repeat the above function until no locations can be removed by this filter.
  sdata2<-max.speed(sdata=sdata, vmax=vmax, method=method)
  sdata3<-max.speed(sdata=sdata2, vmax=vmax, method=method)
  while(!(nrow(sdata2) == nrow(sdata3)))
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
    message('Warning: insufficient data to apply ddfilter_speed to:')
    message(paste(id.exclude, collapse = ', '))
  }

  
  # Delete working columns and return the output
  # sdata3<-sdata3[,headers]
  return(sdata3)
}
