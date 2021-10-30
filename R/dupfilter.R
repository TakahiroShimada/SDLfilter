#' @aliases dupfilter
#' @title Filter temporal and/or spatial duplicates
#' @description Function to filter temporal and spatial duplicates in tracking data 
#' and retain only a single fix per time and location.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param step.time Consecutive locations less than or equal to \emph{step.time} apart are considered temporal duplicates.
#' Default is 0 hours.
#' @param step.dist Consecutive locations less than or equal to \emph{step.dist} apart are considered spatial duplicates.
#' Default is 0 kilometres.
#' @param conditional If TRUE, spatial duplicates are filtered only if they are less than or equal to \emph{step.time} apart. 
#' Default is FALSE.
#' @export
#' @details This function filters temporal and spatial duplicates in tracking data.
#' It first filters temporally and spatially exact locations. 
#' It then looks for temporal duplicates and retains a fix with the highest quality index.
#' When temporal or spatial duplicates are associated with the same quality index, 
#' the function retains a location that is nearest from a previous and to a subsequent location.
#' @return The input data frame is returned containing only a single fix (latitude/longitude pair) per time and location.
#' The following columns are added: "pTime", "sTime", "pDist", "sDist". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively. 
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 \doi{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter_exact}}, \code{\link{dupfilter_qi}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_space}}, \code{\link{track_param}}
#' @examples 
#' #### Load data sets
#' ## Fastloc GPS data obtained from a green turtle
#' data(turtle)
#' 
#' 
#' #### Apply dupfilter
#' turtle.dup <- dupfilter(turtle)


dupfilter <- function(sdata, step.time=0, step.dist=0, conditional=FALSE){
  
  #### Sample size for unfiltered data
  OriginalSS <- nrow(sdata)
  
  #### Function to filter temporal and/or spatial duplicates
  dupfilter_all <- function(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional){
    ## a. same timing and location
    sdata <- dupfilter_exact(sdata)
    
    ## b. quality index
    sdata <- dupfilter_qi(sdata=sdata, step.time=step.time)
    
    ## c. same timing
    sdata <- dupfilter_time(sdata=sdata, step.time=step.time)
    
    ## d. same location
    sdata <- dupfilter_space(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional)
  }
  
  cat("\n")
  sdata3 <- dupfilter_all(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional)
    

  #### Report the summary of filtering
  FilteredSS <- nrow(sdata3)
  RemovedSamplesN <- OriginalSS-FilteredSS
  RemovedSamplesP <- round((1-(FilteredSS/OriginalSS))*100,2)
  
  cat("\n")
  cat("Input data:", OriginalSS, "locations", fill = TRUE)
  cat("Filtered data:", FilteredSS, "locations", fill = TRUE)
  cat("dupfilter removed ", RemovedSamplesN, " locations (", RemovedSamplesP, "% of original data)", sep="", fill = TRUE)
  cat("\n")
  
  #### Return the filtered data set
  return(sdata3)
}
