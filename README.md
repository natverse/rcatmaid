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

## Authentication
You will obviously need to have the details of a valid CATMAID instance to try this out.  It is recommended that you set these details by including code like this in in your .Rprofile file:

```r
options(catmaid.server="https://mycatmaidserver.org/catmaidroot",
  catmaid.authname="Calvin",catmaid.authpassword="hobbes",
  catmaid.username="calvin", catmaid.password="hobbesagain")
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
