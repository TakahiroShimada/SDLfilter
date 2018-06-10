## Test environments
* local ubuntu 16.04 install, R 3.4.4
* ubuntu 14.04.5 on travis-ci, R 3.5.0
* win-builder, devel


## R CMD check results

There were no ERRORs, WARNINGs or NOTEs when run on ubuntu (local and travis-ci) or devtools::build_win(version = "R-devel").


## Reverse dependencies

* I have run R CMD check on the downstream dependencies. Please see below for the summary results.


# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.4 (2018-03-15) |
|system   |x86_64, linux-gnu            |
|ui       |RStudio (1.1.453)            |
|language |en_AU:en                     |
|collate  |en_AU.UTF-8                  |
|tz       |Australia/Brisbane           |
|date     |2018-06-10                   |

## Packages

|package    |*  |version |date       |source                               |
|:----------|:--|:-------|:----------|:------------------------------------|
|data.table |   |1.11.4  |2018-05-27 |cran (@1.11.4)                       |
|geosphere  |   |1.5-7   |2017-11-05 |cran (@1.5-7)                        |
|ggmap      |   |2.6.1   |2016-01-23 |cran (@2.6.1)                        |
|ggplot2    |*  |2.2.1   |2016-12-30 |cran (@2.2.1)                        |
|ggsn       |   |0.4.0   |2017-03-20 |cran (@0.4.0)                        |
|gridExtra  |   |2.3     |2017-09-09 |cran (@2.3)                          |
|raster     |   |2.6-7   |2017-11-13 |cran (@2.6-7)                        |
|SDLfilter  |*  |1.1.1   |2018-06-10 |local (TakahiroShimada/SDLfilter@NA) |
|sp         |   |1.3-1   |2018-06-05 |cran (@1.3-1)                        |
|trip       |   |1.5.0   |2016-10-18 |cran (@1.5.0)                        |

# Check results

0 packages with problems

