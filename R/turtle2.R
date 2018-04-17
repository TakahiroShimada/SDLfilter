#' @aliases turtle2
#' @title Green turtle tracking data 2
#' @description A dataset containing Fastloc GPS locations of a green turtle tracked in Moreton Bay, Australia.
#' @format A data frame with 276 rows and 5 variables:
#' \describe{
#'   \item{id}{identifier for each animal}
#'   \item{DateTime}{date & time in class POSIXct}
#'   \item{lat}{latitude in decimal degrees}
#'   \item{lon}{longitude in decimal degrees}
#'   \item{qi}{numerical quality index associated with each fix where the greater number represents better quality
#'    (e.g. number of GPS satellites used for estimation)}
#' }
#' @source Shimada T, Jones R, Limpus C, Groom R, Hamann M (2016) 
#' Long-term and seasonal patterns of sea turtle home ranges in warm coastal foraging habitats: Implications for conservation. 
#' Marine Ecology Progress Series 562:163-179. doi: 10.3354/meps11972
"turtle2"