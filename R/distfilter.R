#' @aliases distfilter
#' @title Filter locations by distance 
#' @description This function removes locations that are located beyond a specified distance.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon". 
#' The function filters the input data by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' @param max.dist A numeric value specifying a threshold of distance between successive locations. Default is 100 km. 
#' @param method An integer specifying how locations should be filtered with \emph{max.dist}. 
#' A location is removed if the distance from a previous and(1)/or(2) to a subsequent location exceeds \emph{max.dist}. 
#' Default is 1 (both way).
#' @import sp
#' @importFrom raster pointDistance
#' @export
#' @details This function removes locations if the distance from a previous and/or to a subsequent location exceeds \emph{max.dist}. 
#' @return The input data is returned without locations identified by this filter. 
#' The following columns are added: "pDist", "sDist". 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively.
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
#' turtle.dup <- dupfilter(turtle, step.time=5/60, step.dist=0.001)
#'  
#' 
#' #### distfilter
#' turtle.dist <- distfilter(turtle.dup)
#' 
#' 
#' #### Plot data removed or retained by ddfilter
#' ## Entire area
#' p1<-plotMap(turtle.dup, bgmap=Australia, point.size = 2, line.size = 0.5, axes.lab.size = 0, 
#'             sb.distance=200, multiplot = FALSE, point.bg = "red",
#'             title.size=15, title="Entire area")[[1]] + 
#'   geom_point(aes(x=lon, y=lat), data=turtle.dist, size=2, fill="yellow", shape=21)+
#'   geom_point(aes(x=x, y=y), data=data.frame(x=c(154, 154), y=c(-22, -22.5)), 
#'              size=3, fill=c("yellow", "red"), shape=21) + 
#'   annotate("text", x=c(154.3, 154.3), y=c(-22, -22.5), label=c("Retained", "Removed"), 
#'            colour="black", size=4, hjust = 0)
#'
#' ## Zoomed in
#' p2<-plotMap(turtle.dup, bgmap=SandyStrait, xlim=c(152.7, 153.2), ylim=(c(-25.75, -25.24)), 
#'             axes.lab.size = 0, sb.distance=10, point.size = 2, point.bg = "red", line.size = 0.5, 
#'             multiplot = FALSE, title.size=15, title="Zoomed in")[[1]] + 
#' geom_path(aes(x=lon, y=lat), data=turtle.dist, size=0.5, colour="black", linetype=1) + 
#' geom_point(aes(x=lon, y=lat), data=turtle.dist, size=2, colour="black", shape=21, fill="yellow")
#'
#' gridExtra::marrangeGrob(list(p1, p2), nrow=1, ncol=2)


distfilter <- function (sdata, max.dist=100, method=1){
  
  OriginalSS<-nrow(sdata)
  
  max.distance<-function(sdata=sdata, max.dist=max.dist, method=method){
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
    
    
    
    # Select locations at which the distance from a previous and to a subsequent location exceeds maximum linear distance
    ## Function to identify location to remove: (0 = remove, 1 = keep)
    if(method==1){
      overMax<-function(i)
        if(sdata$pDist[i]>max.dist && sdata$sDist[i]>max.dist && (!is.na(sdata$pDist[i])) && (!is.na(sdata$sDist[i]))){
          0
        } else {
          1
        }
    } else if (method==2) {
      overMax<-function(i)
        if((sdata$pDist[i]>max.dist | sdata$sDist[i]>max.dist) && (!is.na(sdata$pDist[i])) && (!is.na(sdata$sDist[i]))){
          0
        } else {
          1
        }
    }
    
    ## Apply the above funtion to each data set seperately
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
      excluded.data[,c("pDist", "sDist", "overMax")]<-NA
      excluded.data[,c("pDist", "sDist", "overMax")]<-NA
      sdata<-rbind(sdata, excluded.data)
    } else {
      sdata<-sdata
    }
  }
  
  
  
  # Repeat the above function until no locations can be removed by this filter.
  sdata2<-max.distance(sdata=sdata, max.dist=max.dist, method=method)
  sdata3<-max.distance(sdata=sdata2, max.dist=max.dist, method=method)
  while(!(nrow(sdata2) %in% nrow(sdata3)))
  {
    sdata3<-max.distance(sdata=sdata2, max.dist=max.dist, method=method)
    sdata2<-max.distance(sdata=sdata3, max.dist=max.dist, method=method)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<4])
  
  ## Filtered data
  FilteredSS<-nrow(sdata3)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  cat("distfilter removed", RemovedSamplesN, "of", OriginalSS, "locations.", fill = TRUE)
  if(length(id.exclude)>0){
    message('Warning: distfilter not applied to ', id.exclude, '. Insufficient data.')
  }
  
  # Delete working columns and return the output
  drops<-c("overMax")
  sdata3<-sdata3[,!(names(sdata3) %in% drops)] 
  return(sdata3)
}