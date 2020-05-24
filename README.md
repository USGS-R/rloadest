rloadest
========

USGS water science R functions for LOAD ESTimation of constituents in rivers and streams.

## Package Status
[![status](https://img.shields.io/badge/USGS-Orphan-red.svg)](https://owi.usgs.gov/R/packages.html#orphan)

This package is currently in an 'orphaned' state, and
looking for a new sponser and maintainer. For more information, see:
[https://owi.usgs.gov/R/packages.html#orphan](https://owi.usgs.gov/R/packages.html#orphan)

If you are interested in becoming the official sponser and maintainer of `rloadest`, please email gs-w_r_admin@usgs.gov.

In the meantime, we rely on community involvement to report and fix bugs.

### Current build tests:

|Linux|Test Coverage|
|----------|------------|
| [![travis](https://travis-ci.org/USGS-R/rloadest.svg?branch=master)](https://travis-ci.org/USGS-R/rloadest)|[![Coverage Status](https://coveralls.io/repos/github/USGS-R/rloadest/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/rloadest?branch=master)|

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
[https://github.com/USGS-R/rloadest/issues](https://github.com/USGS-R/rloadest/issues)

Follow `@USGS_R` on Twitter for updates on USGS R packages:

[![Twitter Follow](https://img.shields.io/twitter/follow/USGS_R.svg?style=social&label=Follow%20USGS_R)](https://twitter.com/USGS_R)


## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/rloadest/blob/master/CONDUCT.md) for more information.

## Package Installation
To install the `rloadest` package, first install the "remotes" package. Then, using the remotes package, install "smwrData", "smwrBase", "smwrGraphs", "smwrStats", and "smwrQW":

```r
remotes::install_github("USGS-R/smwrData")
remotes::install_github("USGS-R/smwrBase")
remotes::install_github("USGS-R/smwrGraphs")
remotes::install_github("USGS-R/smwrStats")
remotes::install_github("USGS-R/smwrQW")
remotes::install_github("USGS-R/rloadest")
```

## Model Archive

When using the `rloadest` model, it is important to be able to reproduce the results in the future. The following version of R and package dependencies were used most recently to pass the embedded tests within this package. There is no guarantee of reproducible results using future versions of R or updated versions of package dependencies.

```
devtools::session_info()
Session info -----------------------------------------------------------------------------
 setting  value                       
 version  R version 3.4.1 (2017-06-30)
 system   x86_64, mingw32             
 ui       RStudio (1.0.143)           
 language (EN)                        
 collate  English_United States.1252  
 tz       America/Chicago             
 date     2017-07-27                  

Packages ---------------------------------------------------------------------------------
 package       * version  date       source                          
 akima           0.6-2    2016-12-20 CRAN (R 3.4.0)                  
 assertthat      0.2.0    2017-04-11 CRAN (R 3.4.0)                  
 base          * 3.4.1    2017-06-30 local                           
 bindr           0.1      2016-11-13 CRAN (R 3.4.0)                  
 bindrcpp        0.2      2017-06-17 CRAN (R 3.4.0)                  
 boot            1.3-19   2017-02-11 CRAN (R 3.4.1)                  
 car             2.1-5    2017-07-04 CRAN (R 3.4.1)                  
 codetools       0.2-15   2016-10-05 CRAN (R 3.4.1)                  
 coin            1.2-1    2017-07-17 CRAN (R 3.4.1)                  
 compiler        3.4.1    2017-06-30 local                           
 curl            2.8.1    2017-07-21 CRAN (R 3.4.1)                  
 dataRetrieval * 2.7.2    2017-05-23 CRAN (R 3.4.1)                  
 datasets      * 3.4.1    2017-06-30 local                           
 devtools        1.13.2   2017-06-02 CRAN (R 3.4.1)                  
 digest          0.6.12   2017-01-27 CRAN (R 3.4.0)                  
 dplyr           0.7.2    2017-07-20 CRAN (R 3.4.1)                  
 glue            1.1.1    2017-06-21 CRAN (R 3.4.0)                  
 graphics      * 3.4.1    2017-06-30 local                           
 grDevices     * 3.4.1    2017-06-30 local                           
 grid            3.4.1    2017-06-30 local                           
 hms             0.3      2016-11-22 CRAN (R 3.4.0)                  
 httr            1.2.1    2016-07-03 CRAN (R 3.3.1)                  
 jsonlite        1.5      2017-06-01 CRAN (R 3.4.0)                  
 KernSmooth      2.23-15  2015-06-29 CRAN (R 3.4.1)                  
 lattice         0.20-35  2017-03-25 CRAN (R 3.4.1)                  
 leaps           3.0      2017-01-10 CRAN (R 3.4.0)                  
 lme4            1.1-13   2017-04-19 CRAN (R 3.4.0)                  
 lubridate     * 1.6.0    2016-09-13 CRAN (R 3.4.0)                  
 magrittr        1.5      2014-11-22 CRAN (R 3.4.0)                  
 MASS            7.3-47   2017-02-26 CRAN (R 3.4.1)                  
 Matrix          1.2-10   2017-05-03 CRAN (R 3.4.1)                  
 MatrixModels    0.4-1    2015-08-22 CRAN (R 3.4.0)                  
 memoise         1.1.0    2017-04-21 CRAN (R 3.4.0)                  
 methods       * 3.4.1    2017-06-30 local                           
 mgcv            1.8-17   2017-02-08 CRAN (R 3.4.0)                  
 minqa           1.2.4    2014-10-09 CRAN (R 3.4.0)                  
 modeltools      0.2-21   2013-09-02 CRAN (R 3.4.0)                  
 multcomp        1.4-6    2016-07-14 CRAN (R 3.4.0)                  
 mvtnorm         1.0-6    2017-03-02 CRAN (R 3.4.0)                  
 NADA            1.6-1    2017-03-31 CRAN (R 3.4.0)                  
 nlme            3.1-131  2017-02-06 CRAN (R 3.4.1)                  
 nloptr          1.0.4    2014-08-04 CRAN (R 3.4.0)                  
 nnet            7.3-12   2016-02-02 CRAN (R 3.4.1)                  
 parallel        3.4.1    2017-06-30 local                           
 pbkrtest        0.4-7    2017-03-15 CRAN (R 3.4.0)                  
 pkgconfig       2.0.1    2017-03-21 CRAN (R 3.4.0)                  
 plyr            1.8.4    2016-06-08 CRAN (R 3.3.1)                  
 quantreg        5.33     2017-04-18 CRAN (R 3.4.0)                  
 R6              2.2.2    2017-06-17 CRAN (R 3.4.0)                  
 randtests       1.0      2014-11-17 CRAN (R 3.4.0)                  
 Rcpp            0.12.12  2017-07-15 CRAN (R 3.4.1)                  
 readr           1.1.1    2017-05-16 CRAN (R 3.4.0)                  
 reshape2        1.4.2    2016-10-22 CRAN (R 3.4.0)                  
 rlang           0.1.1    2017-05-18 CRAN (R 3.4.1)                  
 rloadest      * 0.4.5    2017-07-26 local                           
 rstudioapi      0.6      2016-06-27 CRAN (R 3.3.1)                  
 sandwich        2.3-4    2015-09-24 CRAN (R 3.4.0)                  
 segmented       0.5-2.1  2017-06-14 CRAN (R 3.4.0)                  
 smwrBase      * 1.1.3    2017-05-02 Github (USGS-R/smwrBase@f7bef98)
 smwrGraphs    * 1.1.3    2017-07-27 local                           
 smwrQW        * 0.7.13   2017-07-26 local                           
 smwrStats     * 0.7.6    2017-07-26 local                           
 sp              1.2-5    2017-06-29 CRAN (R 3.4.1)                  
 SparseM         1.77     2017-04-23 CRAN (R 3.4.0)                  
 splines         3.4.1    2017-06-30 local                           
 stats         * 3.4.1    2017-06-30 local                           
 stats4          3.4.1    2017-06-30 local                           
 stringi         1.1.5    2017-04-07 CRAN (R 3.4.0)                  
 stringr         1.2.0    2017-02-18 CRAN (R 3.4.0)                  
 survival        2.41-3   2017-04-04 CRAN (R 3.4.0)                  
 TH.data         1.0-8    2017-01-23 CRAN (R 3.4.0)                  
 tibble          1.3.3    2017-05-28 CRAN (R 3.4.0)                  
 tools           3.4.1    2017-06-30 local                           
 truncnorm       1.0-7    2014-01-21 CRAN (R 3.3.0)                  
 utils         * 3.4.1    2017-06-30 local                           
 withr           1.0.2    2016-06-20 CRAN (R 3.3.1)                  
 XML             3.98-1.9 2017-06-19 CRAN (R 3.4.0)                  
 xml2            1.1.1    2017-01-24 CRAN (R 3.4.0)                  
 zCompositions   1.0.3-1  2016-04-14 CRAN (R 3.4.0)                  
 zoo             1.8-0    2017-04-12 CRAN (R 3.4.0)
```

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."



