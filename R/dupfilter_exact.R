#' @aliases dupfilter_exact
#' @title Filter temporally and spatially exact duplicates
#' @description Function to filter temporally and spatially exact locations in tracking data.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' The function filters the input data by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the numerical quality index associated with each location fix where a greater number indicates a higher accuracy 
#' (e.g. the number of GPS satellites involved in estimation).
#' @importFrom dplyr distinct
#' @export
#' @details This is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function.
#' It looks for temporally and spatially exact locations and retains only a single fix (latitude/longitude pair) per time and location. 
#' @return The input data frame is returned with temporally and spatially exact duplicates removed. 
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 doi:\href{http://doi.org/10.1007/s00227-015-2771-0}{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_qi}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_space}}, \code{\link{track_param}}

  
dupfilter_exact <- function(sdata){
  
  #### Sample size for unfiltered data
  OriginalSS <- nrow(sdata)

  #### Filter temporally and spatially exact duplicates
  sdata <- dplyr::distinct(sdata, .data$id, .data$DateTime, .data$lat, .data$lon, .data$qi, .keep_all = TRUE)

  #### Report the summary of filtering
  ## Filtered data
  FilteredSS <- nrow(sdata)
  RemovedSamplesN <- OriginalSS-FilteredSS
  
  ## Print report
  cat("dupfilter_exact removed", RemovedSamplesN, "of", OriginalSS, "locations.", fill = TRUE)

  #### return the output
  return(sdata)
}
