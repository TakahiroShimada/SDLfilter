## Test environments
* win-builder: R-devel


## R CMD check results

There were no ERRORs or WARNINGs, and 2 NOTES as below when run on windows (win-builder).
Please note each of the points within the note is verified and all information is correct.


* checking CRAN incoming feasibility ... [17s] NOTE
Maintainer: 'Takahiro Shimada <taka.shimada@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13506
    From: man/flatback.Rd
          man/ud_matrix.Rd
    Status: 403
    Message: Forbidden
    
    
* checking Rd files ... NOTE
checkRd: (-1) ud_raster.Rd:19: Lost braces
    19 | Inter-nesting utilisation distributions of 15 flatback turtles (/emph{Natator depressus}) that nested in Curtis Island, Australia.
       |                                                                      ^
checkRd: (-1) ud_raster.Rd:20: Lost braces
    20 | The UDs were calculated using the sample tracking data /code{/link{flatback}} and reduced grid resolution (1 km)
       |                                                             ^
checkRd: (-1) ud_raster.Rd:20: Lost braces
    20 | The UDs were calculated using the sample tracking data /code{/link{flatback}} and reduced grid resolution (1 km)
       |                                                                   ^
checkRd: (-1) ud_raster.Rd:21: Lost braces
    21 | instead of 50m as used in /href{https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13506}{Shimada et al. (2021)}.
       |                                ^
checkRd: (-1) ud_raster.Rd:21: Lost braces
    21 | instead of 50m as used in /href{https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13506}{Shimada et al. (2021)}.
       |                                                                                                         ^
checkRd: (-1) ud_raster.Rd:22: Lost braces
    22 | See /href{https://github.com/TakahiroShimada/SDLfilter}{GitHub} for an example code of UD estimation.
       |          ^
checkRd: (-1) ud_raster.Rd:22: Lost braces
    22 | See /href{https://github.com/TakahiroShimada/SDLfilter}{GitHub} for an example code of UD estimation.
       |                                                        ^
checkRd: (-1) ud_raster.Rd:13: Lost braces
    13 | \emph{Methods in Ecology and Evolution} 12(2):288-297 /doi{10.1111/2041-210X.13506}
       |                                                           ^