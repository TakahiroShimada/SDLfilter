## Test environments
* local OS X install, R 3.4.3
* local ubuntu 16.04 install, R 3.4.3
* ubuntu 14.04 on travis-ci, R 3.4.2
* win-builder, devel
* local windows 7 install, R 3.4.3

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---

* I have run R CMD check on the downstream dependencies. Please see below for the summary results.


 ## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.3 (2017-11-30) |
|system   |x86_64, linux-gnu            |
|ui       |X11                          |
|language |en_AU:en_GB:en               |
|collate  |en_AU.UTF-8                  |
|tz       |Australia/Brisbane           |
|date     |2018-02-07                   |

 ## Packages

|package    |*  |version  |date       |source          |
|:----------|:--|:--------|:----------|:---------------|
|data.table |   |1.10.4-3 |2017-10-27 |cran (@1.10.4-) |
|geosphere  |   |1.5-7    |2017-11-05 |cran (@1.5-7)   |
|raster     |   |2.6-7    |2017-11-13 |cran (@2.6-7)   |
|SDLfilter  |   |1.0.0    |2018-02-07 |local (@1.0.0)  |
|sp         |   |1.2-7    |2018-01-19 |cran (@1.2-7)   |
|trip       |   |1.5.0    |2016-10-18 |cran (@1.5.0)   |

 # Check results

0 packages with problems


## Resubmission
This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to title case.

* Added a DOI to each reference in the description field as instructed.

