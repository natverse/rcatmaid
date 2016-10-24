[![Travis-CI Build Status](https://travis-ci.org/jefferis/rcatmaid.svg?branch=master)](https://travis-ci.org/jefferis/rcatmaid)
# catmaid

This package provides access to the [CATMAID](http://catmaid.org/) API for 
[R](http://r-project.org/) users.  At present it provides low level functions 
for appropriately authenticated GET/POST requests, optionally parsing JSON responses.
There are also intermediate level functions that retrieve skeleton (i.e. neuron) 
information, connectivity information for one or more neurons as well as a number 
of other API endpoints. Finally, there is
a high level function to convert neurons to the representation of the
[nat](https://github.com/jefferis/nat) (NeuroAnatomy Toolbox) R package, enabling
a wide variety of analyses.

It is strongly recommended to read through the instructions below, the [package
overview documentation](http://jefferis.github.io/rcatmaid/docs/catmaid-package)
and then skim the [reference documentation index](http://jefferis.github.io/rcatmaid/docs/reference.html)
, which groups the available functions into useful categories.

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
example(catmaid_get_neuronnames)

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
## Even fancier example
This follows on from the previous example. It identifies downstream partner
neurons of the ORNs and plots them in 3d coloured by their synaptic strength.
It then carries out morphological clustering with [NBLAST](http://bit.ly/nblast)
and plots the partner neurons according to those clusters.

```r
# find all the ORN downstream partners with at least 2 synapses
orn_partners=catmaid_query_connected(orns[,'skid'], minimum_synapses = 2)
# keep the ones not already in our set of PNs
# there are lots!
non_pn_downstream_ids=setdiff(unique(orn_partners$outgoing$partner), pns[,'skid'])
# download and plot those neurons
non_pn_downstream=read.neurons.catmaid(non_pn_downstream_ids, .progress='text')
plot3d(non_pn_downstream, col='grey', soma=1000)

# remove the last set of plotted neurons
npop3d()

## Plot, but colouring partners by number of synapses they receive from ORNs
# first collect those synapse numbers
library(dplyr)
totsyndf=orn_partners$outgoing %>% 
  group_by(partner) %>% 
  summarise(totsyn=sum(syn.count)) %>% 
  arrange(desc(totsyn))
hist(totsyndf$totsyn)
# now do the plot
clear3d()
# matlab style palette
jet.colors <-
colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
"#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# plot colouring by synapse number on a log scale 
# note that it is necessary to convert totsyndf$partner to a character
# vector to ensure that they are not treated as integer indices
plot3d(as.character(totsyndf$partner),  db=c(pns, non_pn_downstream), 
  col=jet.colors(10)[cut(totsyndf$totsyn, breaks = 2^(0:10))], soma=1000)

# Now let's cluster these other connected neurons
library(nat.nblast)
# convert to nblast-compatible format
# nb also convert from nm to um, resample to 1µm spacing and use k=5
# nearest neighbours of each point to define tangent vector
non_pn_downstream.dps=dotprops(non_pn_downstream/1e3, k=5, resample=1, .progress='text')
# now compute all x all NBLAST scores and cluster
non_pn_downstream.aba=nblast_allbyall(non_pn_downstream.dps, .progress='text')
non_pn_downstream.hc=nhclust(scoremat = non_pn_downstream.aba)
# plot result of clusterting as dendrogram, labelled by neuron name (rather than id)
plot(non_pn_downstream.hc, label=non_pn_downstream[,'name'])
# open new window
nopen3d()
# plot in 3d cutting into 2 clusters essentially left right
plot3d(non_pn_downstream.hc,db=non_pn_downstream, k=2, soma=1000)
clear3d() 
# 4 clusters - note local and projection neurons, gustatory neurons
plot3d(non_pn_downstream.hc,db=non_pn_downstream, k=4, soma=1000)
```

## Authentication
You will obviously need to have the login details of a valid CATMAID instance to try 
this out. 

### Setting package authentication options in your .Rprofile
It is recommended that you set these details by including code like 
this in in your .Rprofile file:

```r
options(catmaid.server="https://mycatmaidserver.org/catmaidroot",
  catmaid.authname="Calvin",catmaid.authpassword="hobbes",
  catmaid.username="calvin", catmaid.password="hobbesagain")
```

In this way authentication will happen transparently as required by all functions
that interact with the specified CATMAID server. Note that the CATMAID servers 
that I am aware of require two layers of password
protection, an outer HTTP auth type user/password combination as well as an inner
CATMAID-specific password.

### Token based authentication
As of December 2015 CATMAID is moving to token based authentication. For this
you will need to get an API token when you are logged into the CATMAID web 
client in your browser. See http://catmaid.github.io/dev/api.html#api-token for
details. 

You would then set your `.Rprofile` like this:

```r
options(catmaid.server="https://mycatmaidserver.org/catmaidroot",
  catmaid.authname="Calvin",catmaid.authpassword="hobbes",
  catmaid.token="9944b09199c62bcf9418ad846dd0e4bbdfc6ee4b")
```
Note that you will probably still need to specify you http username/password combination
even if you are using an API token to authenticate to the CATMAID server.

### Cached authentication 
Whether you use options in your `.Rprofile` as described above or you login 
explicitly at the start of a session by doing something like:

```r
catmaid_login(server="https://mycatmaidserver.org/catmaidroot",
              authname="Calvin",authpassword="hobbes",
              token="9944b09199c62bcf9418ad846dd0e4bbdfc6ee4b")
```

the access credentials will be cached for the rest of
the session. You can still authenticate explicitly to a different CATMAID server
(using `catmaid_login`) if you wish.

### Multiple servers
If you need to talk to more than one catmaid server in a single session then you 
must use `catmaid_login` to login into each server

```r
# log in to default server specified in .Rprofile
conn1=catmaid_login()
# log into another server, presumably with different credentials
conn2=catmaid_login(server='https://my.otherserver.com', ...)
```

and then use the returned connection objects with any calls you make e.g.

```r
# fetch neuron from server 1
n1=read.neuron(123, conn=conn1)
# fetch neuron from server 2
n2=read.neuron(123, conn=conn2)
```
nb you must use connection objects to talk to both servers because if no 
connection object is specified, the last connection will be re-used.

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

* https://github.com/catmaid/CATMAID/blob/master/scripts/remote/access.py
* https://github.com/catmaid/CATMAID/blob/master/django/applications/catmaid/urls.py
* https://github.com/schlegelp/CATMAID-to-Blender/blob/master/CATMAIDImport.py

by Albert Cardona and Philipp Schlegel. Released under the GPL-3 license.
