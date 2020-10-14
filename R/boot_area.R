#' @aliases boot_area
#' @title Cumulative analysis of collective areas by bootstrapping 
#' @description Function to calculate collective areas (merged x\% Utilisation Distributions or UDs) of n individuals by bootstrapping.
#' @param data A matrix or list of RasterLayer objects. 
#' Each row of the matrix or each RasterLayer object contains a utilisation distribution 
#' (or other statistics that sums to 1 - e.g. proportion of time spent).
#' \bold{The grid size and geographical extent must be consistent across each row of the matrix or each RasterLayer object.}
#' The function assumes that each column of the matrix is associated with a unique geographical location or 
#' that each RasterLayer has exactly the same geographical extent and resolution. 
#' @param cell.size A numeric value specifying the grid cell size of the input data in metres. 
#' @param R An integer specifying the number of iterations.
#' @param percent An integer specifying the percent volume of each UD to be considered in the analysis. 
#' @param quantiles A vector or a number to specify the quantiles to be calculated in the summary of the results. 
#' @importFrom raster values
#' @importFrom raster res
#' @importFrom plyr rbind.fill
#' @importFrom stats aggregate sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @details This function calculates collective areas (e.g. 50\% UDs) of 1 to n individuals by bootstrapping.
#' @return A list containing two data frames - raw results and summary (mean, sd, sem and quantiles at each sample size).
#' @author Takahiro Shimada
#' @references Shimada T, Thums M, Hamann M, Limpus CJ, Hays GC, FitzSimmons N, Wildermann NE, Duarte CD, Meekan MG (in press) 
#' Optimising sample sizes for animal distribution analysis using tracking data. 
#' \emph{Methods in Ecology and Evolution} n/a(n/a) doi:\href{http://doi.org/10.1111/2041-210X.13506}{10.1111/2041-210X.13506}
#' @examples
#' \dontrun{
#' 
#' #1 Utilisation distributions of flatback turtles.
#' data(curtis)
#' 
#' #2 Calculate collective areas from 1000 random permutation
#' area <- boot_area(curtis, R = 1000, percent = 50)
#' 
#' #3 Find the minimum sample size required to estimate the general distribution.
#' a <- asymptote(area)
#' 
#' #4 Plot the mean collective area and rational function fit relative to the sample sizes.
#' ggplot(data = area$summary)+
#'   geom_point(aes(x = N, y = mu/1e+6), alpha = 0.5) + 
#'   geom_path(data = a$results, aes(x = x, y = ys/1e+6)) +        
#'   labs(x = "N", y = expression(Area~(km^2)))
#' }



boot_area <- function(data, cell.size = NA, R = 1000, percent = 50, quantiles = c(0.25, 0.5, 0.75)) {
    
    
    #### Process time
    start_time <- Sys.time()
    
    #### Input data
    if(class(data[[1]]) == "RasterLayer"){
      ## density values to a matrix
      dens_all_list <- lapply(1:length(data), function(j){
        raster::values(data[[j]])
      })
      
      if(is.null(names(data))){
        names(dens_all_list) <- 1:length(data)
      } else {
        names(dens_all_list) <- names(data)
      }
      dens_all <- do.call(rbind, dens_all_list)
      
      ## Cell size
      cell.size <- raster::res(data[[1]])
      
    } else {
      dens_all <- data
    }
    
    #### Names
    nam <- rownames(dens_all)
    
    #### Crop to the area of interest
    dens_all_sum <- colSums(dens_all)
    dens_all <- dens_all[,which(dens_all_sum > 0)]

    #### Standardise UD values (i.e. density sums to 1)
    row_sum <- rowSums(dens_all)
    l <- lapply(1:nrow(dens_all), function(x) {dens_all[x,] / row_sum[x]})
    dens_all <- do.call(rbind, l)
    
    #### Areas under xx% UD
    l <- lapply(1:nrow(dens_all), function(x) percent_vol(dens_all[x,], percent = percent))
    dens_all <- do.call(rbind, l)


    #### Function to combine layers
    getTotalArea <- function(i){
      
      ## Random order of the input layers
      n <- sample(nrow(dens_all), nrow(dens_all))
      
      ## Combined areas
      Combinelayers <- lapply(1:nrow(dens_all), function(j){
        layers <- dens_all[n[1:j],]
        if(j > 1){
          comb_layer <- colSums(layers)
        } else {
          comb_layer <- layers
        }
        comb_area <- length(comb_layer[comb_layer > 0]) * cell.size^2
        data.frame(N = j, Area = comb_area)
      })
      
      comb_df <- plyr::rbind.fill(Combinelayers)
      
      ## progress bar
      Sys.sleep(0.1)
      utils::setTxtProgressBar(pb, i)
      
      ## Output
      return(comb_df)
    }
    
    
    ## create a progress bar
    pb <- utils::txtProgressBar(min = 0, max = R, width = 50, style = 3)
    CombArea.list <- lapply(1:R, getTotalArea)
    close(pb)
    
    ## Combined area in data frame
    total.area <- plyr::rbind.fill(CombArea.list)
    
    
    #### Summary stats
    summary.data <- stats::aggregate(Area ~ N, data = total.area, FUN = function(x) {
      c(mu = mean(x),
        std = stats::sd(x),
        sem = stats::sd(x)/sqrt(length(x)),
        quantile(x, probs = quantiles))
    })
    summary.data <- cbind(N = summary.data[,1], as.data.frame(summary.data[,-1]))
    quantile_header <- quantiles * 100
    colnames(summary.data) <- c("N", "mu", "std", "sem", paste0("Qu", quantile_header))
    
    #### Running time
    run.time <- as.numeric(difftime(Sys.time(), start_time, units = 'min'))
    M <- floor(run.time); S <- round((run.time - M) * 60)
    message('Runtime: ', M, ' minutes ', S, ' seconds')
    
    #### Outputs
    list(data = total.area, summary = summary.data)
}
  
