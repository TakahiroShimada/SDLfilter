#' @aliases ddfilter
#' @title Filter locations using a data driven filter
#' @description Function to remove locations by a data driven filter as described in Shimada et al. (2012).
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or 
#' \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param vmax A numeric value specifying a threshold of speed from a previous and/or to a subsequent fix. 
#' Default is 8.9km/h. 
#' If this value is unknown, it can be estimated from \emph{sdata} using the function \code{\link{vmax}}.
#' @param vmaxlp A numeric value specifying a threshold of speed, which is used to evaluate the locations of loop trips. 
#' Default is 1.8 km/h.
#' If this value is unknown, it can be estimated from \emph{sdata} using the function \code{\link{vmaxlp}}.
#' @param qi An integer specifying a threshold of quality index, which is used to evaluate the locations of loop trips. 
#' Default is 4.
#' @param ia An integer specifying a threshold of inner angle, which is used to evaluate the locations of loop trips. 
#' Default is 90 degrees.
#' @param method An integer specifying how locations should be filtered with \emph{vmax}. 
#' A location is removed if the speed from a previous and(1)/or(2) to a subsequent location exceeds \emph{vmax}. 
#' Default is 1 (both way).
#' @importFrom gridExtra marrangeGrob
#' @export
#' @details Locations are removed if the speed from a previous and/or to a subsequent location exceeds \emph{vmax}, 
#' or if all of the following criteria apply: the associated quality index is less than or equal to \emph{qi}, 
#' the inner angle is less than or equal to \emph{ia} and the speed either from a previous 
#' or to a subsequent location exceeds \emph{vmaxlp}. 
#' If \emph{vmax} and \emph{vmaxlp} are unknown, they can be estimated using the functions \code{\link{vmax}} and \code{\link{vmaxlp}} respectively.
#' @return The input data is returned without locations identified by this filter. 
#' The following columns are added: "pTime", "sTime", "pDist", "sDist", "pSpeed", "sSpeed", "inAng". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively. 
#' "pSpeed" and "sSpeed" are linear speed from a previous and to a subsequent fix respectively. 
#' "inAng" is the degree between the bearings of lines joining successive location points.
#' @author Takahiro Shimada
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening. 
#' \emph{Marine Ecology Progress Series} 457:171-180 \doi{10.3354/meps09747}
#' @seealso \code{\link{ddfilter_speed}}, \code{\link{ddfilter_loop}}, \code{\link{vmax}}, \code{\link{vmaxlp}}
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
#' #### ddfilter
#' ## Using the built-in function to estimate the threshold speeds
#' V <- vmax(turtle.dup)
#' VLP <- vmaxlp(turtle.dup)
#' turtle.dd <- ddfilter(turtle.dup, vmax=V, vmaxlp=VLP)
#' 
#' ## Or using user specified threshold speeds
#' turtle.dd <- ddfilter(turtle.dup, vmax=9.9, qi=4, ia=90, vmaxlp=2.0)
#' 
#' 
#' #### Plot data removed or retained by ddfilter
#' ## Entire area
#' p1<-map_track(turtle.dup, bgmap=Australia, point.size = 2, line.size = 0.5, axes.lab.size = 0, 
#'             sb.distance=200, multiplot = FALSE, point.bg = "red",
#'             title.size=15, title="Entire area")[[1]] + 
#'   geom_point(aes(x=lon, y=lat), data=turtle.dd, size=2, fill="yellow", shape=21)+
#'   geom_point(aes(x=x, y=y), data=data.frame(x=c(154, 154), y=c(-22, -22.5)), 
#'              size=3, fill=c("yellow", "red"), shape=21) + 
#'   annotate("text", x=c(154.3, 154.3), y=c(-22, -22.5), label=c("Retained", "Removed"), 
#'            colour="black", size=4, hjust = 0)
#'
#' ## Zoomed in
#' p2<-map_track(turtle.dup, bgmap=SandyStrait, xlim=c(152.7, 153.2), ylim=(c(-25.75, -25.24)), 
#'             axes.lab.size = 0, sb.distance=10, point.size = 2, point.bg = "red", line.size = 0.5, 
#'             multiplot = FALSE, title.size=15, title="Zoomed in")[[1]] + 
#' geom_path(aes(x=lon, y=lat), data=turtle.dd, size=0.5, colour="black", linetype=1) + 
#' geom_point(aes(x=lon, y=lat), data=turtle.dd, size=2, colour="black", shape=21, fill="yellow")
#'
#' gridExtra::marrangeGrob(list(p1, p2), nrow=1, ncol=2)



ddfilter<-function(sdata, vmax=8.9, vmaxlp=1.8, qi=4, ia=90, method=1) {
  
  #### Sample size of the input data
  OriginalSS<-nrow(sdata)
 
  #### Run ddfilters
  cat("\n")
  sdata<-ddfilter_speed(sdata=sdata, vmax=vmax, method=method)
  sdata<-ddfilter_loop(sdata=sdata, qi=qi, ia=ia, vmaxlp=vmaxlp)
  
  
  #### Report the summary of filtering
  FilteredSS<-nrow(sdata)
  RemovedSamplesN<-OriginalSS-FilteredSS
  RemovedSamplesP<-round((1-(FilteredSS/OriginalSS))*100,2)
  
  cat("\n")
  cat("Input data:", OriginalSS, "locations", fill = TRUE)
  cat("Filtered data:", FilteredSS, "locations", fill = TRUE)
  cat("ddfilter removed ", RemovedSamplesN, " locations (", RemovedSamplesP, "% of original data)", sep="", fill = TRUE)
  cat("\n")
  
  #### Return the filtered data set
  return(sdata)
}
