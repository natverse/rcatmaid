# catmaid

This package provides access to the [CATMAID](http://catmaid.org/) API for 
[R](http://r-project.org/) users.  At present it simply provides for appropriately
authenticated GET/POST requests, optionally parsing JSON responses.

## Quick start
```r
# install
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferis/rcatmaid")

# use 
library(catmaid)

# examples
example(catmaid_login)
example(catmaid_POST)
```

## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/).

### Bleeding Edge
You can use the **devtools** package to install the development version:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferis/rcatmaid")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and [devtools](http://CRAN.R-project.org/package=devtools) to install this way.
