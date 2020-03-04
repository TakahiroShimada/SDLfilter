#' @aliases dupfilter_qi
#' @title Filter temporal duplicates by quality index
#' @description Function to filter temporal duplicates in tracking data by quality index.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' The function filters the input data by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the numerical quality index associated with each location fix where a greater number indicates a higher accuracy 
#' (e.g. the number of GPS satellites involved in estimation).
#' @param step.time Consecutive locations less than or equal to \emph{step.time} apart are considered temporal duplicates.
#' Default is 0 hours.
#' @export
#' @details This function is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function. 
#' First it identifies spatial duplicates by searching for consecutive fixes that were located within \emph{step.dist}. For each group of spatial duplicates, 
#' It looks for temporal duplicates and retains a fix with the highest quality index.
#' @return The input data frame is returned with temporal duplicates removed by the quality index. 
#' The following columns are added: "pTime", "sTime". "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively.
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 doi:\href{http://doi.org/10.1007/s00227-015-2771-0}{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_exact}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_space}}



dupfilter_qi<-function (sdata=sdata, step.time=0){
  
  #### Sample size for unfiltered data
  OriginalSS<-nrow(sdata)
  
  
  #### Function to filter temporal duplicates by quality index
  dup.qi<-function (sdata=sdata, step.time=step.time){
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
  sdata2<-dup.qi(sdata=sdata, step.time=step.time)
  sdata3<-dup.qi(sdata=sdata2, step.time=step.time)
  while(!(nrow(sdata2) %in% nrow(sdata3)))
  {
    sdata3<-dup.qi(sdata=sdata2, step.time=step.time)
    sdata2<-dup.qi(sdata=sdata3, step.time=step.time)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<3])
    
  ## Filtered data
  FilteredSS<-nrow(sdata3)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  cat("dupfilter_qi removed", RemovedSamplesN, "of", OriginalSS, "locations.", fill = TRUE)
  if(length(id.exclude)>0){
    message('Warning: dupfilter_qi not applied to the following data. Insufficient data.')
    message(paste(id.exclude, collapse = ', '))
  } 

  
  
  #### Delete working columns and return the output
  sdata3$rm<-NULL
  # drops<-c("rm", "sTime")
  # sdata3<-sdata3[,!(names(sdata3) %in% drops)] 
  return(sdata3)
}
