#' @aliases to_map
#' @title Plot location data on a map
#' @description Function to plot tracking data on a map or a satellite image. 
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon". 
#' The function creates a map for each unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} 
#' or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' @param xlim,ylim Limits for x and y axes. 
#' If not specified, the values are determined as the maximum range of the input data plus an additional margin (see \emph{margin}).    
#' @param margin Set the amount of spaces added around the periphery of the plot. The value is scaled to the plot. 
#' The smaller value increases the margin.
#' @param bgmap A data frame of a background map data, containing the following headers: "long", "lat", "group". 
#' If not specified, the \code{\link[maps]{world}} map is used. 
#' Google Maps ("terrain", "satellite", "roadmap", "hybrid") can also be queried.
#' @param google.key If Google Maps are queried, a valid API key (a string) needs to be specified here. See \code{\link[ggmap]{register_google}} for details.
#' @param map.bg Background colour of the map. This argument is ignored when any Google Maps is selected.
#' @param map.col Outline colour of the map. This argument is ignored when any Google Maps is selected.
#' @param zoom Map zoom for Google Maps. Default (NULL) to estimate the zoom from each data set. 
#' For other options, see \code{\link[ggmap]{get_map}} for details. 
#' @param point.bg The colour to fill in a symbol.
#' @param point.col The colour for the outline of a symbol.
#' @param point.symbol An integer or a string to specify the symbol type. See \code{\link[ggplot2:aes_linetype_size_shape]{shape}} for details. 
#' @param point.size An integer to specify the size of the symbol.
#' @param line.col The colour of the line that connects consecutive points.
#' @param line.type The type of the line that connects consecutive points. See \code{\link[ggplot2:aes_linetype_size_shape]{linetype}} for details. 
#' @param line.size An integer to specify the thickness (width) of the line that connects consecutive points. 
#' @param sb.distance An integer to specify the length of the scale bar. If not specified, approximately a quarter of the 
#' plotting range will be used. 
#' @param sb.lwd An integer to specify the thickness (width) of the scale bar.
#' @param sb.line.col The colour of the scale bar.
#' @param sb.text.size An integer to specify the text size for the scale bar.
#' @param sb.text.col The colour of the text for the scale bar.
#' @param sb.space Set the amount of space between the scale bar and the text. 
#' The value is scaled to the plot. The smaller value increases the space.
#' @param title The main title for each plot. If not specified, the "id" will be used.
#' @param title.size An integer to specify the size of the title.
#' @param axes.text.size An integer to specify the size of the axes characters.
#' @param axes.lab.size An integer to specify the size of the axes labels.
#' @param multiplot Logical. If TRUE (default), multiple plots are displayed on the same page.
#' @param nrow An integer to specify the number of rows in the multiple plot page.
#' @param ncol An integer to specify the number of columns in the multiple plot page.
#' @import ggmap ggplot2
#' @importFrom ggsn scalebar
#' @importFrom gridExtra marrangeGrob
#' @importFrom sf st_as_sf st_distance
#' @importFrom maps map
#' @export
#' @return An arrangelist is returned when \emph{multiplot} is TRUE. Otherwise a list is returned. 
#' @author Takahiro Shimada
#' @seealso \code{\link{dupfilter}}, \code{\link{ddfilter}}, \code{\link{vmax}}, \code{\link{vmaxlp}}
#' @examples
#' #### Load data sets
#' ## Fastloc GPS data obtained from two green turtles
#' data(turtle)
#' data(turtle2)
#' turtles<-rbind(turtle, turtle2)
#' 
#' #### Filter temporal and/or spatial duplicates
#' turtle.dup <- dupfilter(turtles, step.time=5/60, step.dist=0.001)
#'  
#' 
#' #### ddfilter
#' V <- vmax(turtle.dup)
#' VLP <- vmaxlp(turtle.dup)
#' turtle.dd <- ddfilter(turtle.dup, vmax=V, vmaxlp=VLP)
#' 
#' 
#' #### Plot filtered data for each animal
#' ## using the low-resolution world map
#' to_map(turtle.dd, point.size = 2, line.size = 0.5, axes.lab.size = 0, ncol=2, nrow=1)
#'
#'\dontrun{
#' ## using the high-resolution google satellite images
#' to_map(turtle.dd, bgmap = "satellite", google.key = "key", ncol=2)
#'}


#### Plot data removed or retained by ddfilter
to_map <- function(sdata, xlim=NULL, ylim=NULL, margin=10, 
                    bgmap=NULL, google.key=NULL, map.bg="grey", map.col="black", zoom=NULL, 
                    point.bg="yellow", point.col="black", point.symbol=21, point.size=1,
                    line.col="lightgrey", line.type=1, line.size=0.5,
                    sb.distance=NULL, sb.lwd=1, sb.line.col="black", sb.text.size=4, sb.text.col="black", sb.space=3,
                    title="id", title.size=11, axes.text.size=11, axes.lab.size=11,
                    multiplot=TRUE, nrow=1, ncol=1){
  
  #### Get data to plot
  ID <- as.character(unique(sdata$id))
  ID <- ID[!is.na(ID)]
  
  ## Date & time
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  
  p.all<-lapply(1:length(ID), function(i){
    #### Subset data
    sdata.temp<-sdata[sdata$id %in% ID[i],]
    
    #### Get the extent to plot
    x1<-min(sdata.temp$lon); x2<-max(sdata.temp$lon)
    y1<-min(sdata.temp$lat); y2<-max(sdata.temp$lat)
    range<-max(c(x2-x1, y2-y1))
    extra<-range/margin
    
    if((x2-x1)<(y2-y1)){
      difference<-range-(x2-x1)
      xrange<-c(x1-extra-difference/2, x2+extra+difference/2)
      yrange<-c(y1-extra, y2 + extra)
    } else {
      difference<-range-(y2-y1)
      xrange<-c(x1-extra, x2+extra)
      yrange<-c(y1-extra-difference/2, y2+extra+difference/2)
    }
    
    if(is.null(xlim) | is.null(ylim)){
      xlim<-xrange
      ylim<-yrange
    } else {
      xlim<-xlim; ylim<-ylim
    } 
    
    extra2<-max(c(diff(xlim), diff(ylim)))/margin
    
    #### Background map
    if(is.null(bgmap)){
      if(inherits(try(ggplot2::map_data('world', xlim=xlim, ylim=ylim)), "try-error")){
        map.data <- ggplot2::map_data('world', xlim=c(-180, 180), ylim=c(-90, 90))
      } else {
        map.data<-try(ggplot2::map_data('world', xlim=xlim, ylim=ylim))
      }

      p <-ggplot(data=sdata.temp)+
        geom_polygon(aes_string(x="long", y="lat", group="group"), data=map.data, bg=map.bg, colour=map.col)
      
    } else if(any(bgmap %in% c("terrain", "satellite", "roadmap", "hybrid"))) {
      ggmap::ggmap_show_api_key()
      ggmap::register_google(key = google.key)
      
      if(is.null(zoom)){
        lonlength <- diff(xlim)
        latlength <- diff(ylim)
        zoomlon <- ceiling( log2( 360*2 / lonlength) )
        zoomlat <- ceiling( log2( 180*2 / latlength) )
        zm <- min(zoomlon, zoomlat)
        
        map.data<-ggmap::get_map(location = c(lon = mean(xlim), lat = mean(ylim)), 
                                 color = "color", source = "google", maptype = bgmap, 
                                 zoom = zm)
        
      } else {
        map.data<-ggmap::get_map(location = c(lon = mean(xlim), lat = mean(ylim)), 
                                 color = "color", source = "google", maptype = bgmap, zoom=zoom)
      }
      
      p <- ggmap::ggmap(map.data)  
      
    } else {
      map.data<-bgmap
      p <-ggplot(data=sdata.temp)+
        geom_polygon(aes_string(x="long", y="lat", group="group"), data=map.data, bg=map.bg, colour=map.col)
    }
    
    
    #### Plot locations on the map
    p <- p +
      geom_path(aes_string(x="lon", y="lat"), data=sdata.temp, colour=line.col, linetype = line.type, size=line.size)+
      geom_point(aes_string(x="lon", y="lat"), data=sdata.temp, size=point.size, colour=point.col, shape=point.symbol, fill=point.bg) +
      coord_fixed(xlim=xlim, ylim=ylim, ratio=1) + 
      theme_classic() + 
      theme(axis.title.x = element_text(colour="black", size=axes.lab.size), 
            axis.title.y = element_text(colour="black", size=axes.lab.size), 
            axis.text.x = element_text(colour="black", size=axes.text.size),
            axis.text.y = element_text(colour="black", size=axes.text.size),
            plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) 
    
    
    ##### Add title
    if(title %in% "id"){
      p <- p + ggtitle(ID[i]) + theme(plot.title = element_text(hjust = 0.5, size=title.size))
    } else {
      p <- p + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5, size=title.size))
    }
    
    #### Add scale
    # Get parameters
    if(is.null(sb.distance)){
      # sb.distance <- raster::pointDistance(c(xlim[1], ylim[1]), c(xlim[2], ylim[1]), lonlat = TRUE)/4
      # sb_mat <- rbind(c(xlim[1], ylim[1]), c(xlim[2], ylim[1]))
      # sb.distance <- terra::distance(sb_mat, lonlat = TRUE)
      sb_df <- data.frame(lon = c(xlim[1], xlim[2]), lat = c(ylim[1], ylim[1]))
      sb_sf <- sf::st_as_sf(sb_df, coords = c('lon', 'lat'), crs = 4326)
      sb.distance <- sf::st_distance(sb_sf)[1,2]
      sb.distance <- as.numeric(sb.distance)/4
      digi <- nchar(trunc(sb.distance))
      sb.distance <- round(sb.distance/10^(digi-1)) * 10^(digi-1)
      sb.distance <- as.numeric(sb.distance)/1000
    }
    
    sb<-ggsn::scalebar(x.min=xlim[1]+extra2, x.max=xlim[2]-extra2, y.min=ylim[1]+extra2, y.max=ylim[2]-extra2,
                       dist = sb.distance, dist_unit = "km", transform = TRUE, model = 'WGS84', location="bottomleft", st.dist=.03)
    
    sb.xmin<-min(sb[[1]]$data$x); sb.xmax<-max(sb[[1]]$data$x)
    sb.ymin<-min(sb[[1]]$data$y); sb.ymax<-max(sb[[1]]$data$y)
    
    sb.df<-data.frame(x=c(sb.xmin, sb.xmax), y=c(sb.ymax, sb.ymax))
    
    # Add scale bar
    p + geom_line(aes_string(x="x", y="y"), data=sb.df, colour=sb.line.col, lwd=sb.lwd) +
      annotate("text", x=mean(c(sb.xmin, sb.xmax)), y=sb.ymin-extra2/sb.space, 
               label=paste0(sb.distance, " km"), colour=sb.text.col, size=sb.text.size)
  })
  
  if(isTRUE(multiplot)){
    gridExtra::marrangeGrob(p.all, nrow=nrow, ncol=ncol, top=NULL)
  } else {
    p.all
  }
}

