#' @aliases combn_overlap
#' @title Quantifying overlaps between all possible combination of Utilisation Distributions (UDs) 
#' @description Function to calculate overlaps between all possible combination of UDs relative to sample size.
#' @param data A matrix or list of RasterLayer objects. 
#' Each row of the matrix or each RasterLayer object contains a utilisation distribution 
#' (or other statistics that sums to 1 - e.g. proportion of time spent).
#' \bold{The grid size and geographical extent must be consistent across each row of the matrix or each RasterLayer object.}
#' The function assumes that each column of the matrix is associated with a unique geographical location or 
#' that each RasterLayer has exactly the same geographical extent and resolution. 
#' @param method The overlap quantification method. 
#' "HR" is for the proportion of an individual's home range overlapped by the known habitats of other individuals. 
#' "PHR" is for the probability of an individual to be within the known habitats of other individuals. 
#' "VI", "BA" and "UDOI" quantify overlap between UDs using the full probabilistic properties as described in Fieberg and Kochanny (2005). 
#' For the latter three options, the function calculates overlaps between each additional UD and a collective UD. 
#' To generate a collective UD, each UD is overlaid and averaged at each grid cell so the probability density of the collective UD sums up to 1.
#' @param percent An integer specifying the percent volume of each UD to be considered in the analysis. 
#' @param quantiles A vector or a number to specify the quantiles to be calculated in the summary of the results. 
#' @importFrom raster values
#' @importFrom stats aggregate sd
#' @importFrom utils setTxtProgressBar txtProgressBar combn
#' @export
#' @details This function calculates overlap between all possible combination of input UDs based on the areas ("HR"), 
#' areas of collective UDs and the probability distribution of each individual ("PHR"), 
#' or the probability distribution of an individual and an averaged probability distribution of collective individuals ("VI", "BA", "UDOI").
#' @return A list containing two data frames - raw results and summary (mean, sd, sem and quantiles at each sample size).
#' @author Takahiro Shimada
#' @references Shimada T, Thums M, Hamann M, Limpus CJ, Hays GC, FitzSimmons N, Wildermann NE, Duarte CD, Meekan MG (2021) 
#' Optimising sample sizes for animal distribution analysis using tracking data. 
#' \emph{Methods in Ecology and Evolution} 12(2):288-297 \doi{10.1111/2041-210X.13506}
#' @references Fieberg J & Kochanny CO (2005) Quantifying home-range overlap: The importance of the utilization distribution. 
#' \emph{The Journal of Wildlife Management}, 69(4), 1346–1359. \doi{10.2193/0022-541x(2005)69[1346:Qhotio]2.0.Co;2}
#' @seealso \code{\link{boot_overlap}}, \code{\link{boot_area}}
#' @examples
#' \dontrun{
#' 
#' #1 Utilisation uistributions of flatback turtles (n = 15).
#' data(ud_matrix)
#' 
#' #2 Calculate overlap probability from all combination of the UDs.
#' overlap <- combn_overlap(ud_matrix, method = "PHR")
#' 
#' #3 Find the minimum sample size required to estimate the general distribution.
#' a <- asymptote(overlap, upper.degree = 10, ci.level = NULL)
#' 
#' #4 Plot the mean probability and rational function fit relative to the sample sizes.
#' ggplot(data = a$results, aes(x = x, y = y))+
#'   geom_point() +
#'   geom_hline(yintercept = a$h.asymptote*0.95, linetype = 2) +
#'   scale_x_continuous(breaks = seq(0, 15, 3), limits = c(2,15), name = "Animals tracked (n)") +
#'   scale_y_continuous(limits = c(0.5,1), name = "Overlap probability")
#' }




combn_overlap <- function(data, method = "PHR", percent = 100, quantiles = c(0.25, 0.5, 0.75)) {
  
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
  
  
  ## empty vectors to save results
  n <- nrow(dens_all) - 1
  sub_combn <- lapply(1:n, function(k){
    utils::combn(n, k, simplify = FALSE)
  })
  n.row <- sum(sapply(sub_combn, length))*nrow(dens_all)
  
  new_data <- rep(0, n.row)
  existing_data <- rep('', n.row)
  overlap <- rep(0, n.row)
  N <- rep(0, n.row)
  
  
  #### Quantify overlap for all combination of input data
  rn <- 0
  pb <- utils::txtProgressBar(min = 0, max = nrow(dens_all), width = 50, style = 3)
  for(i in 1:nrow(dens_all)){
    
    ## New data
    dens_a <- dens_all[i,]
    
    ## Existing data
    n <- (1:nrow(dens_all))[-i]
    
    # Generate all combination of the input layers taken k at a time.
    for(k in 1:length(n)){
      sub_combn <- utils::combn(n, k, simplify = FALSE)
      
      
      for(h in 1:length(sub_combn)){
        rn <- rn + 1
        
        ## combined ud of the subsets (i.e. existing data)
        dens_sub <- dens_all[sub_combn[[h]],]
        
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
        new_data[rn] <- i
        existing_data[rn] <- paste(sub_combn[[h]], collapse=',')
        overlap[rn] <- index
        N[rn] <- length(sub_combn[[h]])+1
      }
    }
 

    #### progress bar
    Sys.sleep(0.1)
    utils::setTxtProgressBar(pb, i)
    
  }

  close(pb)
  overlap_df <- data.frame(new_data, existing_data, overlap, N)

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

