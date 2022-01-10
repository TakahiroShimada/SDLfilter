## Test environments
* local debian 10 install: R 4.1.1
* win-builder: R-devel


## R CMD check results

There were no ERRORs or WARNINGs, or NOTEs when run on debian (local)
There were no ERRORs nor WARNINGs and one NOTEs as below when run on windows (win-builder).
Words and URLs identified as 'possibly misspelled' or 'possibly invalid' are verified to be correct. 
  
  Maintainer: 'Takahiro Shimada <taka.shimada@gmail.com>'
  
  New submission
  
  Package was archived on CRAN
  
  Possibly misspelled words in DESCRIPTION:
    GPS (8:34)
    Shimada (8:295, 8:345, 8:571)
    al (8:306, 8:356, 8:582)
    et (8:303, 8:353, 8:579)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2021-10-06 as requires archived package
      'landmap'.
  
  Found the following (possibly) invalid URLs:
    URL: https://doi.org/10.2193/0022-541x(2005)69[1346:Qhotio]2.0.Co;2
      From: man/boot_overlap.Rd
      Status: 500
      Message: Internal Server Error


## Reverse dependencies

* I have run R CMD check on the downstream dependencies. Please see below for the summary results.


# Platform

|field    |value                                                 |
|:--------|:-----------------------------------------------------|
|version  |R version 4.1.2 (2021-11-01)                          |
|os       |Debian GNU/Linux 10 (buster)                          |
|system   |x86_64, linux-gnu                                     |
|ui       |RStudio                                               |
|language |en_AU:en                                              |
|collate  |en_AU.UTF-8                                           |
|ctype    |en_AU.UTF-8                                           |
|tz       |Australia/Brisbane                                    |
|date     |2022-01-10                                            |
|rstudio  |1.4.1717 Juliet Rose (desktop)                        |
|pandoc   |2.11.4 @ /usr/lib/rstudio/bin/pandoc/ (via rmarkdown) |

# Dependencies

|package   |old   |new   |Δ  |
|:---------|:-----|:-----|:--|
|SDLfilter |2.1.2 |2.2.0 |*  |

# Revdeps


# Check results

0 packages with problems


