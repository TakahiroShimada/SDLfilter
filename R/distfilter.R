#' @aliases distfilter
#' @title Filter locations by distance 
#' @description This function removes locations that are located beyond a specified distance.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' @param max.dist A numeric value specifying a threshold of distance between successive locations. Default is 100 km. 
#' @param method An integer specifying how locations should be filtered with \emph{max.dist}. 
#' A location is removed if the distance from a previous and(1)/or(2) to a subsequent location exceeds \emph{max.dist}. 
#' Default is 1 (both way).
#' @param ia An integer (0 to 180) specifying an inner angle (in degrees) between consecutive locations, 
#' beyond which the locations are considered potential outliers. 
#' Default (NA) ignores this option. See details. 
#' @importFrom dplyr bind_rows
#' @export
#' @details This function removes locations if the distance from a previous and/or to a subsequent location exceeds \emph{max.dist} and
#' the inner angle is less than \emph{ia}. If \emph{ia} is NA (default), inner angles are not considered in the filtering.
#' @return The input data is returned without locations identified by this filter. 
#' The following columns are added: "pDist", "sDist", 'inAng'. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively.
#' "inAng" is the degree between the bearings of lines joining successive location points.
#' @author Takahiro Shimada
#' @examples
#' #### Load data sets
#' ## Fastloc GPS data obtained from a green turtle
#' data(turtle)
#' 
#' ## A Map for the example site
#' data(Australia)
#' data(SandyStrait)
#' 
#' 
#' #### Filter temporal and/or spatial duplicates
#' turtle.dup <- dupfilter(turtle, step.time=1/60, step.dist=0.001)
#'  
#' 
#' #### distfilter
#' turtle.dist <- distfilter(turtle.dup, max.dist = 50, ia = 20)
#' 
#' 
#' #### Plot data removed or retained by ddfilter
#' ## Entire area
#' p1 <- to_map(turtle.dup, bgmap=Australia, point.size = 2, line.size = 0.5, axes.lab.size = 0, 
#'             multiplot = FALSE, point.bg = "red",
#'             title.size=15, title="Entire area")[[1]] + 
#'   geom_point(aes(x=lon, y=lat), data=turtle.dist, size=2, fill="yellow", shape=21)+
#'   geom_point(aes(x=x, y=y), data=data.frame(x=c(154, 154), y=c(-22, -22.5)), 
#'              size=3, fill=c("yellow", "red"), shape=21) + 
#'   annotate("text", x=c(154.3, 154.3), y=c(-22, -22.5), label=c("Retained", "Removed"), 
#'            colour="black", size=4, hjust = 0)
#'
#' ## Zoomed in
#' p2 <- to_map(turtle.dup, bgmap=SandyStrait, xlim=c(152.7, 153.2), ylim=(c(-25.75, -25.24)), 
#'             axes.lab.size = 0, point.size = 2, point.bg = "red", line.size = 0.5, 
#'             multiplot = FALSE, title.size=15, title="Zoomed in")[[1]] + 
#' geom_path(aes(x=lon, y=lat), data=turtle.dist, linewidth=0.5, colour="black", linetype=1) + 
#' geom_point(aes(x=lon, y=lat), data=turtle.dist, size=2, colour="black", shape=21, fill="yellow")
#'
#' gridExtra::marrangeGrob(list(p1, p2), nrow=1, ncol=2)


distfilter <- function (sdata, max.dist = 100, method = 1, ia = NA){
  
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
  
  
  max.distance <- function(sdata = sdata, max.dist = max.dist, method = method, ia = ia){
    #### Exclude data with less than 4 locations
    ndata <- table(sdata$id)
    id.exclude <- names(ndata[as.numeric(ndata) < 4])
    excluded.data <- sdata[sdata$id %in% id.exclude,]
    sdata <- sdata[!(sdata$id %in% id.exclude),]
    
    if(nrow(sdata) > 0){
      #### Organize data
      ## Sort data in alphabetical and chronological order
      sdata <- with(sdata, sdata[order(id, DateTime),])
      row.names(sdata) <- 1:nrow(sdata)
      
      
      ## Get Id of each animal
      IDs <- levels(factor(sdata$id))
      
      
      ## Function to identify location to remove: (0 = remove, 1 = keep)
      if(is.na(ia)){
        
        ## Get movement parameters
        sdata <- track_param(sdata, param = 'distance')
        
        ## function
        if(method == 1){
          overMax<-function(i)
            if(sdata$pDist[i] > max.dist && sdata$sDist[i] > max.dist && (!is.na(sdata$pDist[i])) && (!is.na(sdata$sDist[i]))){
              0
            } else {
              1
            }
        } else if (method == 2) {
          overMax<-function(i)
            if((sdata$pDist[i] > max.dist | sdata$sDist[i] > max.dist) && (!is.na(sdata$pDist[i])) && (!is.na(sdata$sDist[i]))){
              0
            } else {
              1
            }
        }
        
      } else {
        
        ## Get movement parameters
        sdata <- track_param(sdata, param = c('distance', 'angle'))
        
        ## function
        if(method == 1){
          overMax<-function(i)
            if(sdata$pDist[i] > max.dist && sdata$sDist[i] > max.dist && sdata$inAng[i] < ia && 
               (!is.na(sdata$pDist[i])) && (!is.na(sdata$sDist[i])) && (!is.na(sdata$inAng[i]))){
              0
            } else {
              1
            }
        } else if (method == 2) {
          overMax <- function(i)
            if((sdata$pDist[i] > max.dist | sdata$sDist[i] > max.dist) && sdata$inAng[i] < ia && 
               (!is.na(sdata$pDist[i])) && (!is.na(sdata$sDist[i])) && (!is.na(sdata$inAng[i]))){
              0
            } else {
              1
            }
        }
      }
      
      
      ## Apply the above function to each data set separately
      set.rm <- function(j){
        start <- as.numeric(rownames(sdata[sdata$id %in% j,][2,]))
        end <- as.numeric(rownames(sdata[sdata$id %in% j,][1,])) + (nrow(sdata[sdata$id %in% j,])-2)
        rm <- unlist(lapply(start:end, overMax))
        c(1, rm, 1)
      }
      
      sdata$overMax <- unlist(lapply(IDs, set.rm))
      
      sdata <- sdata[sdata$overMax == 1,]
    }

    
    #### Bring back excluded data
    if(nrow(excluded.data) > 0){
      sdata <- dplyr::bind_rows(sdata, excluded.data)
    } else {
      sdata <- sdata
    }
  }
  
  
  
  # Repeat the above function until no locations can be removed by this filter.
  sdata2 <- max.distance(sdata = sdata, max.dist = max.dist, method = method, ia = ia)
  sdata3 <- max.distance(sdata = sdata2, max.dist = max.dist, method = method, ia = ia)
  while(!(nrow(sdata2) == nrow(sdata3)))
  {
    sdata3 <- max.distance(sdata = sdata2, max.dist = max.dist, method = method, ia = ia)
    sdata2 <- max.distance(sdata = sdata3, max.dist = max.dist, method = method, ia = ia)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata <- table(as.character(sdata$id))
  id.exclude <- names(ndata[as.numeric(ndata) < 4])
  
  ## Filtered data
  FilteredSS <- nrow(sdata3)
  RemovedSamplesN <- OriginalSS - FilteredSS
  if(RemovedSamplesN > 0){
    RemovedSamplesP <- round((1-(FilteredSS/OriginalSS))*100,2)
  } else {
    RemovedSamplesP <- 0
  }
  
  ## Print report
  cat("\n")
  cat("Input data:", OriginalSS, "locations", fill = TRUE)
  cat("Filtered data:", FilteredSS, "locations", fill = TRUE)
  cat("distfilter removed ", RemovedSamplesN, " locations (", RemovedSamplesP, "% of original data)", sep="", fill = TRUE)
  if(length(id.exclude) > 0){
    message('Warning: insufficient data to apply distfilter to:')
    message(paste(id.exclude, collapse = ', '))
  }
  cat("\n")
  
  
  # Delete working columns and return the output
  drop.vars <- c("pTime", "sTime", "pSpeed", "sSpeed", "meanSpeed", "meanAngle", 'overMax')
  sdata3 <- sdata3[,!(names(sdata3) %in% drop.vars)] 
  return(sdata3)
}
