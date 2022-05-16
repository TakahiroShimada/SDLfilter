## Test environments
* local debian 11 install: R 4.2.0
* win-builder: R-devel


## R CMD check results

There were no ERRORs or WARNINGs, or NOTEs when run on debian (local).
There were no ERRORs or WARNINGs, and 1 NOTE as below when run on windows (win-builder).

Please note this package was on CRAN but has been archived because the dependent package (plotKML) has been archived.
I verified that other points within the note are all correct.


* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Takahiro Shimada <taka.shimada@gmail.com>'

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  Shimada (8:295, 8:345, 8:571)
  al (8:306, 8:356, 8:582)
  et (8:303, 8:353, 8:579)

Found the following (possibly) invalid URLs:
  URL: http://www.int-res.com/abstracts/meps/v457/p171-180/ (moved to https://www.int-res.com/abstracts/meps/v457/p171-180/)
    From: man/vmaxlp.Rd
    Status: 200
    Message: OK
  URL: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13506
    From: man/flatback.Rd
          man/ud_matrix.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1111/2041-210X.13506
    From: README.md
    Status: 503
    Message: Service Unavailable

Found the following (possibly) invalid DOIs:
  DOI: 10.1111/2041-210X.13506
    From: DESCRIPTION
          inst/CITATION
    Status: Service Unavailable
    Message: 503


