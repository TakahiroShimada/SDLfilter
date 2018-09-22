#' @aliases dupfilter
#' @title Filter temporal and/or spatial duplicates
#' @description Function to remove temporal and/or spatial duplicates.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' This filter is independently applied to a subset of data grouped by the unique "id". "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' "qi" is the numerical quality index associated with each fix where the greater number represents better quality 
#' (e.g. number of GPS satellites used for estimation).
#' @param step.time A numeric value specifying temporal interval between two consecutive locations. Default is 0 hours. 
#' Locations are considered temporal duplicates if the temporal interval is less than or equal to the user specified value.
#' @param step.dist A numeric value specifying spatial interval between two consecutive locations. Default is 0 kilometres. 
#' Locations are considered spatial duplicates if the spatial interval is less than or equal to the user specified value.
#' @param conditional If TRUE, spatial duplicates are removed only if temporal interval between the locations is 
#' less than the time specified in "step.time". Default is FALSE.
#' @export
#' @details A fix associated with a higher quality index is retained over other duplicated fixes. 
#' If temporal duplicates are associated with same quality index, a fix located closest to a previous and a subsequent location is retained.
#' @return Input data frame is returned with spatial and temporal duplicates removed. 
#' The following columns are added: "pTime", "pDist". 
#' "pTime" is hours from a previous fix. "pDist" is straight distance in kilometres from a previous fix. 
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' Marine Biology 163:1-14 doi:10.1007/s00227-015-2771-0
#' @seealso \code{\link{dupfilter.exact}}, \code{\link{dupfilter.qi}}, \code{\link{dupfilter.time}}, \code{\link{dupfilter.space}}
#' @examples 
#' #### Load data sets
#' ## Fastloc GPS data obtained from a green turtle
#' data(turtle)
#' 
#' 
#' #### Apply dupfilter
#' turtle.dup <- dupfilter(turtle)


dupfilter<-function(sdata, step.time=0, step.dist=0, conditional=FALSE){
  
  #### Sample size for unfiltered data
  OriginalSS<-nrow(sdata)
  
  #### Function to filter temporal and/or spatial duplicates
  dupfilter.all<-function (sdata, step.time, step.dist, conditional) {
    ## a. same timing and location
    sdata<-dupfilter.exact(sdata)
    
    ## b. quality index
    sdata<-dupfilter.qi(sdata, step.time)
    
    ## c. same timing
    sdata<-dupfilter.time(sdata, step.time)
    
    ## d. same location
    sdata<-dupfilter.space(sdata, step.time, step.dist, conditional)
  }
  
  cat("\n")
  sdata3<-dupfilter.all(sdata, step.time, step.dist, conditional)
    

  #### Report the summary of filtering
  FilteredSS<-nrow(sdata3)
  RemovedSamplesN<-OriginalSS-FilteredSS
  RemovedSamplesP<-round((1-(FilteredSS/OriginalSS))*100,2)
  
  cat("\n")
  cat("Input data:", OriginalSS, "locations")
  cat("\n")
  cat("Filtered data:", FilteredSS, "locations")
  cat("\n")
  cat("dupfilter removed ", RemovedSamplesN, " locations (", RemovedSamplesP, "% of original data)", sep="")
  cat("\n")
  cat("\n")
  
  #### Return the filtered data set
  return(sdata3)
}
