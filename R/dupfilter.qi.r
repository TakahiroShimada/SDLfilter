#' @aliases dupfilter.qi
#' @title Filter temporal duplicates by quality index
#' @description A partial component of dupfilter, although works as a stand-alone function. 
#' This function removes temporal duplicates according to the associated quality index.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' This filter is independently applied to a subset of data grouped by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the numerical quality index associated with each fix where the greater number represents better quality 
#' (e.g. number of GPS satellites used for estimation).
#' @param step.time A numeric value specifying temporal interval between two consecutive locations. 
#' Default is 0 hours. Locations are considered temporal duplicates if the temporal interval is 
#' less than or equal to the user specified value.
#' @export
#' @details This function selects a fix from multiple fixes, which were simultaneously obtained but associated with a different level of quality index. 
#' The fix with the higher quality index is selected over the others.
#' @return Input data frame is returned with temporal duplicates removed according to quality index. 
#' The following columns are added: "pTime", "sTime". "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively.
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' Marine Biology 163:1-14 doi:10.1007/s00227-015-2771-0
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter.exact}}, \code{\link{dupfilter.time}}, \code{\link{dupfilter.space}}



dupfilter.qi<-function (sdata, step.time=0){
  
  #### Sample size for unfiltered data
  OriginalSS<-nrow(sdata)
  
  
  #### Function to filter temporal duplicates by quality index
  dup.qi<-function (sdata, step.time){
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
    
    
    # Remove duplicates by quality index: (0 = remove, 1 = keep)
    pick.dup<-function(i)
    {
      if(sdata$sTime[i]<=step.time && sdata$qi[i]<sdata$qi[i+1] &&
         (!is.na(sdata$sTime[i])) && (!is.na(sdata$qi[i])) && (!is.na(sdata$qi[i+1]))) {
        0
      } else if(sdata$pTime[i]<=step.time && sdata$qi[i]<sdata$qi[i-1] &&
                (!is.na(sdata$pTime[i])) && (!is.na(sdata$qi[i])) && (!is.na(sdata$qi[i-1]))) {
        0
      } else {
        1
      }
    }
    
    sdata$rm<-unlist(lapply(1:(nrow(sdata)), pick.dup))
    sdata<-sdata[sdata$rm==1,]
    
    #### Bring back excluded data
    if(nrow(excluded.data)>0){
      excluded.data[,c("pTime", "sTime", "rm")]<-NA
      sdata<-rbind(sdata, excluded.data)
    } else {
      sdata<-sdata
    }
  }
  
  
  #### Repeat the function until no locations can be removed by this filter
  sdata2<-dup.qi(sdata,step.time)
  sdata3<-dup.qi(sdata2,step.time)
  while(!(nrow(sdata2) %in% nrow(sdata3)))
  {
    sdata3<-dup.qi(sdata2,step.time)
    sdata2<-dup.qi(sdata3,step.time)
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
      cat("dupfilter.qi removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
      cat("  Warning: insufficient data to run dupfilter.qi for", id.exclude)
      cat("\n")
  } else {
      cat("dupfilter.qi removed", RemovedSamplesN, "of", OriginalSS, "locations")
      cat("\n")
  }
  
  
  
  #### Delete working columns and return the output
  sdata3$rm<-NULL
  return(sdata3)
}
