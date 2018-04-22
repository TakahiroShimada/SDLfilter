#' @aliases plotMap
#' @title Plot location data
#' @description Function to easily plot locations on a map or a satellite image. 
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon". 
#' Locations are independently plotted to each subset of data grouped by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. "lat" and "lon" are the recorded latitude and longitude in decimal degrees. 
#' @param xlim Limits for x axis. If not specified, the maximum range of the input data plus an additional margin (see \emph{margin}) is used.    
#' @param ylim Limits for x axis. See \emph{xlim} for details. 
#' @param margin Set the amount of areas added around the periphery of the plot. The value is scaled to the plot. 
#' The smaller value increases the margin area.
#' @param bgmap A background map in data.frame, which contains the following headers: "long", "lat", "group"
#' as the output of the \strong{broom} package. If not specified, the "world" map provided by the \strong{maps} package is used. 
#' The Google Maps ("terrain", "satellite", "roadmap", "hybrid") can also be queried.
#' @param map.bg Backgroud colour of the map. This argument is used only when the input backgroud map is a vector.
#' @param map.col Ouline colour of the map. This argument is used only when the input backgroud map is a vector.
#' @param zoom Map zoom for the Google Maps. See \code{\link[ggmap]{get_map}} for details. 
#' @param point.bg The colour to fill in a symbol.
#' @param point.col The colour for the outline of a symbol.
#' @param point.symbol An integer or a string to specify the symbol type. See \code{\link[ggplot2]{shape}} for details. 
#' @param point.size An integer to specify the size of the symbol.
#' @param line.col The colour of the line that connects consecutive points.
#' @param line.type The type of the line that connects consecutive points. See \code{\link[ggplot2]{linetype}} for details. 
#' @param line.size An interger to specify the thickness (width) of the line that connects consecutive points. 
#' @param sb.distance An integer to specify the length of the scale bar. If not specified, approximately a quater of the 
#' plotting range will be used. 
#' @param sb.lwd An interger to specify the thickness (width) of the scale bar.
#' @param sb.line.col The colour of the scale bar.
#' @param sb.text.size An integer to specify the text size for the scale bar.
#' @param sb.text.col The colour of the text for the scale bar.
#' @param sb.space Set th e amount of space between the scale bar and the text for the scale bar. 
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
#' @export
#' @return An arrangelist is returned when multiplot is TRUE. Otherwise a list is returned. 
#' @author Takahiro Shimada
#' @seealso \code{\link{dupfilter}}, \code{\link{ddfilter}}, \code{\link{est.vmax}}, \code{\link{est.maxvlp}}
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
#' V <- est.vmax(turtle.dup)
#' VLP <- est.maxvlp(turtle.dup)
#' turtle.dd <- ddfilter(turtle.dup, vmax=V, maxvlp=VLP)
#' 
#' 
#' #### Plot filtered data for each animal
#' ## using the low-resolution world map
#' plotMap(turtle.dd, point.size = 2, line.size = 0.5, axes.lab.size = 0, 
#'          ncol=2, nrow=1, point.bg = "yellow")
#'
#' \dontrun{
#' ## using the high-resolution google satellite images
#' plotMap(turtle.dd, point.size = 2, line.size = 0.5, axes.lab.size = 0, ncol=2, nrow=1, 
#'          point.bg = "yellow", bgmap = "satellite", sb.line.col = "white", sb.text.col = "white")}



#### Plot data removed or retained by ddfilter
plotMap<-function(sdata, xlim=NULL, ylim=NULL, margin=10, 
                   bgmap=NULL, map.bg="grey", map.col="black", zoom="auto", 
                   point.bg="yellow", point.col="black", point.symbol=21, point.size=1,
                   line.col="lightgrey", line.type=1, line.size=0.5,
                   sb.distance=NULL, sb.lwd=1, sb.line.col="black", sb.text.size=4, sb.text.col="black", sb.space=3,
                   title="id", title.size=11, axes.text.size=11, axes.lab.size=11,
                   multiplot=TRUE, nrow=1, ncol=1){
  
  #### Get data to plot
  ID<-levels(factor(sdata$id))
  
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
      map.data<-map_data('world', xlim=xlim, ylim=ylim)
      p <-ggplot()+
        geom_polygon(aes(x=map.data$long, y=map.data$lat, group=map.data$group), data=map.data, bg=map.bg, colour=map.col)
      
    } else if(bgmap %in% "terrain" || bgmap %in% "satellite" || bgmap %in% "roadmap" || bgmap %in% "hybrid") {
      map.data<-ggmap::get_map(location = c(lon = mean(xlim), lat = mean(ylim)), 
                               color = "color", source = "google", maptype = bgmap, zoom=zoom)
      p <-ggmap::ggmap(map.data)
    } else {
      map.data<-bgmap
      p <-ggplot()+
        geom_polygon(aes(x=map.data$long, y=map.data$lat, group=map.data$group), data=map.data, bg=map.bg, colour=map.col)
    }
    
    
    #### Plot locations on the map
    p <- p +
      geom_path(aes(x=sdata.temp$lon, y=sdata.temp$lat), data=sdata.temp, colour=line.col, linetype = line.type, size=line.size)+
      geom_point(aes(x=sdata.temp$lon, y=sdata.temp$lat), data=sdata.temp, size=point.size, colour=point.col, shape=point.symbol, fill=point.bg) +
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
      sb.distance<-round(((xlim[2]-xlim[1])*111.139)/4)
      sb<-ggsn::scalebar(x.min=xlim[1]+extra2, x.max=xlim[2]-extra2, y.min=ylim[1]+extra2, y.max=ylim[2]-extra2,
                         dist = sb.distance, dd2km = TRUE, model = 'WGS84', location="bottomleft", st.dist=.03)
    } else {
      sb<-ggsn::scalebar(x.min=xlim[1]+extra2, x.max=xlim[2]-extra2, y.min=ylim[1]+extra2, y.max=ylim[2]-extra2,
                         dist = sb.distance, dd2km = TRUE, model = 'WGS84', location="bottomleft", st.dist=.03)
    }
    sb.xmin<-min(sb[[1]]$data$x)
    sb.xmax<-max(sb[[1]]$data$x)
    sb.ymin<-min(sb[[1]]$data$y)
    sb.ymax<-max(sb[[1]]$data$y)
    
    sb.df<-data.frame(x=c(sb.xmin, sb.xmax), y=c(sb.ymax, sb.ymax))
    
    # Add scale bar
    p + geom_line(aes(x=sb.df$x, y=sb.df$y), data=sb.df, colour=sb.line.col, lwd=sb.lwd) +
      annotate("text", x=mean(c(sb.xmin, sb.xmax)), y=sb.ymin-extra2/sb.space, 
               label=paste0(sb.distance, " km"), colour=sb.text.col, size=sb.text.size)
  })
  
  if(isTRUE(multiplot)){
    gridExtra::marrangeGrob(p.all, nrow=nrow, ncol=ncol)
  } else {
    p.all
  }
}
 