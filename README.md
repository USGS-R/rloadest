rloadest
========

USGS water science R functions for LOAD ESTimation of constituents in rivers and streams.

## Package Status
[![status](https://img.shields.io/badge/USGS-Orphan-red.svg)](https://owi.usgs.gov/R/packages.html#orphan)

This package is currently in an 'orphaned' state, and
looking for a new maintainer. For more information, see:
[https://owi.usgs.gov/R/packages.html#orphan](https://owi.usgs.gov/R/packages.html#orphan)

If you are interested in becoming the official maintainer of `rloadest`, please email gs-w_r_admin@usgs.gov.

In the meantime, we rely on community involvement to report and fix bugs. Please

### Current build tests:

|Linux|Test Coverage|
|----------|------------|
| [![travis](https://travis-ci.org/USGS-R/rloadest.svg?branch=master)](https://travis-ci.org/USGS-R/rloadest)||[![Coverage Status](https://coveralls.io/repos/github/USGS-R/rloadest/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/rloadest?branch=master)|

## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to this project. See the [code of conduct](https://github.com/USGS-R/rloadest/blob/master/CONDUCT.md) for more information.

##Package Installation
To install the `rloadest` package:

1. Install R (version 3.0 or greater) from: [https://cran.r-project.org/](https://cran.r-project.org/)

2. Install RStudio from: [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)

3. Add the USGS R repository to your "Rprofile" to get automatic updates. Run the following code:
  
  ```r
  rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
  write('\noptions(repos=c(getOption(\'repos\'),
    CRAN=\'https://cloud.r-project.org\',
    USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

  cat('Your Rprofile has been updated to include GRAN.
    Please restart R for changes to take effect.')
  ```

4. Restart R!

5. In the RStudio "Console" window (usually left or lower-left), run the following command:

  ```r
  install.packages("rloadest")
  ```
  

6. Update often. Next time you update your packages, `rloadest` will automatically update:

   ![update](images/update.png)

7. Make sure to keep your version of R up-to-date. CRAN and the USGS repository will only update packages for the most recent version of R, and one version behind that.


Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."



