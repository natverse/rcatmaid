# catmaid

This package provides access to the [CATMAID](http://catmaid.org/) API for 
[R](http://r-project.org/) users.  At present it provides low level functions 
for appropriately authenticated GET/POST requests, optionally parsing JSON responses.
There are also intermediate level functions to retrieve skeleton (i.e. neuron) 
information and connectivity information for one or more neurons. Finally, there is
a high level function to convert neurons to the representation of the
[nat](https://github.com/jefferis/nat) (NeuroAnatomy Toolbox) R package, enabling
a wide variety of analyses.

## Quick start
```r
# install
if (!require("devtools")) install.packages("devtools")
# nb repo is rcatmaid, but R package name is catmaid
devtools::install_github("jefferis/rcatmaid")

# use 
library(catmaid)

# general help starting point
?catmaid

# examples
example(catmaid_login)
example(catmaid_fetch)
example(catmaid_get_compact_skeleton)

# use with nat
library(nat)
nl=read.neurons.catmaid(c(10418394,4453485), pid=1)
open3d()
# nb this also plots the connectors (i.e. synapses) 
# red = presynapses, cyan = postsynapses
plot3d(nl, WithConnectors=TRUE)
```
## Fancier example
This produces a 3D plot of the first and second order olfactory neurons
coloured according to the peripheral odorant receptor.
```r
# fetch olfactory receptor neurons
orns=read.neurons.catmaid("name:ORN (left|right)", .progress='text')
# calculate some useful metadata
orns[,'Or']= factor(sub(" ORN.*", "", orns[,'name']))

# repeat for their PN partners, note use of search by annotation
pns=read.neurons.catmaid("annotation:ORN PNs$", .progress='text')
pns[,'Or']= factor(sub(" PN.*", "", pns[,'name']))
# plot, colouring by odorant receptor
plot3d(orns, col=Or)
# note that we plot somata with a radius of 1500 nm
plot3d(pns, col=Or, soma=1500)
```

## Authentication
You will obviously need to have the details of a valid CATMAID instance to try 
this out.  It is recommended that you set these details by including code like 
this in in your .Rprofile file:

```r
options(catmaid.server="https://mycatmaidserver.org/catmaidroot",
  catmaid.authname="Calvin",catmaid.authpassword="hobbes",
  catmaid.username="calvin", catmaid.password="hobbesagain")
```

Once you have logged in at least once in the current R session using catmaid_login
or friends, the access credentials will be cached and you should no longer need
to authenticate explicitly.


## Installation
Currently there isn't a released version on [CRAN](http://cran.r-project.org/).

### Bleeding Edge
You can use the **devtools** package to install the development version:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferis/rcatmaid")
```

Note: Windows users need [Rtools](http://www.murdoch-sutherland.com/Rtools/) and
[devtools](http://CRAN.R-project.org/package=devtools) to install this way.

## Acknowledgements

Based on python code presently visible at:

* https://github.com/acardona/CATMAID/blob/master/scripts/remote/access.py
* https://github.com/schlegelp/CATMAID-to-Blender/blob/master/CATMAIDImport.py

by Albert Cardona and Philipp Schlegel. Released under the GPL-3 license.
