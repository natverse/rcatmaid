# catmaid

This package provides access to the [CATMAID](http://catmaid.org/) API for 
[R](http://r-project.org/) users.  At present it provides low level functions 
for appropriately authenticated GET/POST requests, optionally parsing JSON responses.
There is also an intermediate level function to retrieve skeleton (i.e. neuron) 
information and a high level function to convert it to the representation of the
[nat](https://github.com/jefferis/nat) (NeuroAnatomy Toolbox) R package.

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
example(catmaid_get_compact_skeleton)

# use with nat
library(nat)
nl=read.neurons.catmaid(c(10418394,4453485), pid=1)
open3d()
plot3d(nl)
# plot the connectors - no built-in functionality yet for this in nat
sapply(nl, function(n) points3d(n$connectors[4:6], col=n$connectors$prepost+3) )
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

## Acknowledgements

Based on python code presently visible at:

* https://github.com/acardona/CATMAID/blob/master/scripts/remote/access.py
* https://github.com/schlegelp/CATMAID-to-Blender/blob/master/CATMAIDImport.py

by Albert Cardona and Philipp Schlegel. Released under the GPL-3 license.
