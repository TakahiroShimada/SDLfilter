## Test environments
* local debian 11 install: R 4.2.0
* win-builder: R-devel


## R CMD check results

There were no ERRORs or WARNINGs, or NOTEs when run on debian (local).
There were no ERRORs or WARNINGs, and 1 NOTE as below when run on windows (win-builder).
Please note each of the points within the note is verified and all information is correct.


* checking CRAN incoming feasibility ... [19s] NOTE
Maintainer: 'Takahiro Shimada <taka.shimada@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13506
    From: man/flatback.Rd
          man/ud_matrix.Rd
    Status: 503
    Message: Service Unavailable