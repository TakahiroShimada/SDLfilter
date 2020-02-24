#' @aliases boot_overlap
#' @title Bootstrap overlaps between Utilisation Distributions (UDs) 
#' @description Function to calculate overlaps between UDs relative to sample size by bootstrapping.
#' @param data A matrix or list of RasterLayer objects. 
#' Each row of the matrix or each RasterLayer object contains utilisation distribution 
#' (or other statistics that sums to 1 - e.g. proportion of time spent).
#' \bold{The grid size and geographical extent must be consistent across each row of the matrix or each RasterLayer object.}
#' The function assumes that each column of the matrix is associated with a unique geographical location or 
#' that each RasterLayer has exactly the same geographical extent and resolution. 
#' @param R An integer specifying the number of iterations.
#' @param method The overlap quantification method. 
#' "HR" is for the proportion of an individual's home range overlapped by the known habitats of other individuals. 
#' "PHR" is for the probability of an individual to be within the known habitats of other individuals. 
#' "VI", "BA" and "UDOI" quantify overlap between UDs using the full probabilistic properties as described in Fieberg and Kochanny (2005). 
#' For the latter three options, the function calculates overlaps between each additional UD and a collective UD. 
#' To generate a collective UD, each UD is overlaid and averaged at each grid cell so the probability density of the collective UD sums up to 1 (Shimada et al. in prep).
#' @param percent An integer specifying the percent volume of each UD to be considered in the analysis. 
#' @param quantiles A vector or a number to specify the quantiles to be calculated in the summary of the results. 
#' @importFrom raster values
#' @importFrom plyr rbind.fill
#' @importFrom stats aggregate sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @details This function calculates and bootstraps overlap between UDs based on the areas ("HR"), 
#' areas of collective UDs and the probability distribution of each individual ("PHR"), 
#' or the probability distribution of an individual and an averaged probability distribution of collective individuals ("VI", "BA", "UDOI").
#' @return A list containing two data frames - raw results and summary (mean, sd, sem and quantiles at each sample size).
#' @author Takahiro Shimada
#' @references Fieberg J & Kochanny CO (2005) Quantifying home-range overlap: The importance of the utilization distribution. 
#' \emph{The Journal of Wildlife Management}, 69(4), 1346–1359. doi: 10.2193/0022-541x(2005)69[1346:Qhotio]2.0.Co;2
#' @references Shimada et al. in prep
#' @examples
#' \dontrun{
#' 
#' #1 Utilisation uistributions of flatback turtles.
#' data(curtis)
#' 
#' #2 Calculate overlap probability from 1000 random permutation.
#' overlap <- boot_overlap(curtis, R = 1000, method = "PHR")
#' 
#' #3 Find the minimum sample size required to estimate the general distribution.
#' a <- asymptote(overlap)
#' 
#' #4 Plot the mean probability and rational function fit relative to the sample sizes.
#' ggplot(data = overlap$summary)+
#'   geom_point(aes(x = N, y = mu), alpha = 0.5) + 
#'   geom_path(data = a$results, aes(x = x, y = ys)) + 
#'   geom_vline(xintercept = a$min.n, linetype = 2) +
#'   labs(x = "N", y = "Overlap probability")
#' }




boot_overlap <- function(data, R = 1000, method = "PHR", percent = 100, quantiles = c(0.25, 0.5, 0.75)) {
  
  #### Process time
  start_time <- Sys.time()
  
  #### Input data
  if(class(data[[1]]) == "RasterLayer"){
    #### Vecterise density values
    dens_all_list <- lapply(1:length(data), function(j){
      raster::values(data[[j]])
    })
    
    if(is.null(names(data))){
      names(dens_all_list) <- 1:length(data)
    } else {
      names(dens_all_list) <- names(data)
    }
    dens_all <- do.call(rbind, dens_all_list)
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
  
  #### Areas under xx% volume
  # if(method == "HR"){
  #   l <- lapply(1:nrow(dens_all), function(x) percent_vol(dens_all[x,], percent))
  #   dens_all <- do.call(rbind, l)
  # }
  
  #### Quantify overlap
  overlap_index <- function(i){
    
    ## Random order of the input layers
    n <- sample(nrow(dens_all), nrow(dens_all))
    
    ## Randomly selected HR to compare with
    dens_a <- dens_all[n[1],]
    
    ## Overlap between dens_a and collective UD
    index.list <- lapply(2:nrow(dens_all), function(k){
      
        ## combined (mean) ud of the subsets
        dens_sub <- dens_all[n[2:k],]
        
        ## Areas under xx% volume (only for collective areas)
        if(percent < 100){
          if(is.matrix(dens_sub)){
            l <- lapply(1:nrow(dens_sub), function(x) percent_vol(dens_sub[x,], percent))
            dens_sub <- do.call(rbind, l)
          } else {
            dens_sub <- percent_vol(dens_sub, percent)
          }
        } 
        
        if(is.matrix(dens_sub)){
          if(method %in% c("HR", "PHR")){
            dens_sub <- colSums(dens_sub)
          } else {
            dens_sub <- colMeans(dens_sub)
          }
        }

        ## Calculate overlaps
        if(method == "HR") {
          goi <- which(dens_a*dens_sub > 0)
          index <- length(dens_a[goi]) / length(dens_a[dens_a > 0])
        } else if(method == "PHR") {
          goi <- which(dens_a*dens_sub > 0)
          index <- sum(dens_a[goi])
        } else if (method == "VI"){
          goi <- which(dens_a*dens_sub > 0)
          min.val <- pmin(dens_a[goi], dens_sub[goi])
          index <- sum(min.val)
        } else if(method == "BA") {
          index <- sum(sqrt(dens_a*dens_sub))
        } else if(method == "UDOI") {
          goi <- which(dens_a*dens_sub > 0)
          val <- dens_a[goi]*dens_sub[goi]
          index <- sum(val) * length(goi)
        }
    
    ## Results
    df <- data.frame(matrix(c(i, k, index, nam[n[1]], c(nam[n[2:k]])), nrow = 1))
    df[,1:ncol(df)] <- lapply(df[,1:ncol(df)], as.character)
    df[,1:3] <- lapply(df[,1:3], as.numeric)
    return(df)
  })
    
    index.df <- plyr::rbind.fill(index.list)
    
      
    #### progress bar
    Sys.sleep(0.1)
    utils::setTxtProgressBar(pb, i)
    
    #### Output
    return(index.df)
  }
  
  #### Run overlap_index function for R times
  pb <- utils::txtProgressBar(min = 0, max = R, width = 50, style = 3)
  overlap_list <- lapply(1:R, overlap_index)
  close(pb)
  
  overlap_df <- plyr::rbind.fill(overlap_list)
  colnames(overlap_df) <- c('iteration', 'N', 'overlap', paste0('id', 1:(ncol(overlap_df)-3)))
  
  #### Summary stats
  summary.data <- stats::aggregate(overlap ~ N, data = overlap_df, FUN = function(x) {
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
  list(data = overlap_df, summary = summary.data)
}

