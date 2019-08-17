# catmaid (development version)

* New functions for handling meta annotations (i.e. annotations of annotations):
  `catmaid_get_meta_annotations()`, `catmaid_query_meta_annotations()`,
  `catmaid_set_meta_annotations()`, `catmaid_remove_meta_annotations()`
  thanks to @alexanderbates for the PR (#130).
* New functions for (un)locking neurons:
  `catmaid_lock_neurons()`, `catmaid_unlock_neurons()`
  thanks to @alexanderbates for the PR (#130).
* New functions to fetch soma information from neurons in a neuronlist:
  `soma()` fetches the XYZ location of the soma.
  `somaindex()` fetches the raw index of the skeleton node associated 
  with the soma (in range 1..N, number of vertices).
  `somaid()` fetches the identifier of the skeleton node associated with the soma
  thanks to @alexanderbates for the PR (#128).

# catmaid 0.9.9

* `catmaid_query_connected()` now returns stats for all connections but
  also allows a confidence threshold to be set. The default threshold of 1
  is a new more permissive behaviour (#113).
  Thanks to Sebastian Cachero / @schlegelp for the bug report.
* Fix bug in `catmaid_connection_getenv()` and friends in getting environment
  variables on some OSes. It turns out that we should switch to recommending 
  variables of the form `catmaid_*` (#110).
  Thanks to @SridharJagannathan for detective work and a PR with a fix.

# catmaid 0.9.8

* Fix bug in `catmaid_get_connectors_between()` when getting neuron names.
  There was an off by one error for the postsynaptic partner name (#109). 
  Thanks to @mmc46 for the bug report.

# catmaid 0.9.7

* New `catmaid_userids()` function converts user login names to ids.
* `catmaid_connection()` gets a new `config` argument to allow curl options to
  be set (#108). 
* Return format and documentation fix: tree node ids in
  `catmaid_get_connector_table()` always refer to the query skeleton and not
  the partner neuron (#107)

# catmaid 0.9.6

* Give `read.neurons.catmaid()` an optional fetch.annotations argument that 
  adds a second metadata data.frame as an attribute to the resultant 
  `neuronlist()` object (#18).
* Fix handling of CATMAID version string (#103)

# catmaid 0.9.5

* Stop `catmaid_get_connector_table()` returning duplicate rows when there are
 multiple connections between the same partner neurons at a given connector. 
 Thanks to Shanice Bailey and Istvan Taisz for the bug report (#106).
* Pass on pid in `catmaid_skids()`, thereby fixing an issue with 
  `read.neurons.catmaid()` with annotation queries when project id is not 1.
  Thanks to Marta Costa for the bug report (#105)

# catmaid 0.9.4

* `write_catmaid_selection()` accepts annotations/name specs

# catmaid 0.9.3

* workaround for bug in `catmaid_version()` (#103). Still waiting on changes in CATMAID
  for more permanent fix.
* fix import error with jsonlite version >=1.6

# catmaid 0.9.2

* Teach `catmaid_get_volumelist()` about new API return format (#102)
* Sync `catmaid_get_treenode_table()` with API (#101)
* make `catmaid_query_by_name()` treat query as regex (#100)
* Fix bug in metadata for read_catmaid_selection (#99)
* Sort tags for neurons so that they are always returned in the same order (#98)
* Add `catmaid_get_node_count()` to get the number of nodes per skeleton
* Teach `catmaid_get_treenodes_detail()` to search by labels - can use this e.g.
  to find all nodes (regardless of neuron) with a certain tag e.g. `soma`.
* Fix bug in `nsoma()` for duplicate skids; also speed-up
* Make `catmaid_login()` explain why passing server directly doesn't work
* Add name|nodes to results of `catmaid_get_connector_table()`
* Speed-up in `catmaid_get_compact_skeleton()`/`read.neuron.catmaid()`
* Teach `catmaid_get_node_count()` to handle NAs/dupes

# catmaid 0.9.1

* Add catmaid_get_labels, catmaid_set_labels, catmaid_remove_labels to 
  manipulate labels (aka tags) on treenodes and connectors (#95)
* Add catmaid_get_all_labels
* Add nsoma function to return number of somata (#96)
* Add catmaid_get_treenodes_detail to get location and other information for 
  many nodes at once (#94)
* Teach catmaid_get_connectors_between to find names for pre/post skids 
  (optional, #93)
* Teach catmaid_get_neuronnames to handle duplicate requests efficiently (#91)
* Teach catmaid_login to accept arguments in a list (#91)

# catmaid 0.9.0

This version is synced with the upcoming release of nat v1.9.0 which streamlines
some functionality but has some breaking changes in behaviour. It must be used
with nat >=1.8.12.900 from github.

* switch to Ops.catmaidneuron to match nat 1.9.0 in prep (and add version 
  requirement)
* Simple search for skids with exact annotation match (#89)
  Now you can do: catmaid_skids("ORN") or read.neurons.catmaid("ORN")
* Fix bug in catmaid_skids error message when looking for one skid (#90)

# catmaid 0.8.1

* Add function to get volumes from catmaid (#82)
* Add functions to read/write catmaid json selection files
* make catmaid_neuronnames returns NA for NA skids

# catmaid 0.8

This is a major version update because of the new support (and recommendation)
to use environment variables for login (.Rprofile is still supported).

* support fetching login information from environment variables (#80)
* catmaid_get_compact_skeleton now returns list of vectors (#79)
  (used to be a list of lists, but this was less convenient and not helpful)
* fix query_by_neuron_or_annotation adding empty annotations attribute

# catmaid 0.7.2

* Add catmaid_get_label_stats (use it to find neurons with soma)
* Add copy_tags_connectors (bring over tags/connector info)
* improve summary.catmaidneuron (#78)
* plot3d bugfix when there are NA points (and soma=T) (#77)

# catmaid 0.7.1

* only plot soma when explicitly tagged  (rather than plotting a sphere at the 
  root of the neuron if not tagged) (#62)
* only plot3d connectors if they exist (#73)
* Ensure nodes do not normally plot for catmaidneurons  (#74)
* doc fix for catmaid_get_treenode_table (#75)

# catmaid 0.7

This is a substantial release with many new functions/enhancements, changes to
match the API defined by the 2016.10.18 CATMAID release, support for token-based
authentication and numerous bug fixes.

* support for CATMAID token-based authentication (#35)
* Add catmaid_get_connectors_between to return connections between (multiple) 
  pre and postsynaptic partners - thanks to Zhihao Zheng (#53, #56)
* Add function to parse CATMAID urls e.g. to extract xyz position (#55)
* Add rename_neuron function (#54)
* Add catmaid_version function (#58)
* Add catmaid_user_history function (#57)
* Add catmaid_get_annotations_for_skeletons function (#38)
* Add catmaid_remove_annotations_for_skeletons function (#43)
* Add catmaid_add_volume to add 3D meshes to catmaid (#69)

* catmaid_get_connector_table can now accept multi skid specifications (#49)
* catmaid_get_connectors_between returns more informative errors (#51)

* fix "No encoding supplied: defaulting to UTF-8." warning (#59)
* stop catmaid_skids returning duplicates when there are multiple matching 
  annotations (#44)
* ensure that connectors are also scaled when xforming neurons (#47)
* catmaid_query_by_annotation returns neuron column as character vector (#46)
* catmaid_get_connector_table returns partner_skid columns as an integer (#48)
* fix bug in url for catmaid_get_review_status function (#45)
* Give sensible name to 1st column of connector data.frame (#42)
* ensure neurons without connectors scale properly (#41)
* fix bug in catmaid_connection_setenv() when some elements are null (#50)
* catmaid_get_annotations_for_skeletons - fix bug in skid order (#39)
* ensure catmaid_get_contributor_stats passes on connection (#37)
* ensure all functions pass on connection to catmaid_fetch (#60)
* connectors.neuronlist should handle neurons without connectors (#29)
* catmaid_skids should pass on pid (#64)
* fix catmaid_get_connector_table to reflect 2016.09.01-65 API change (#65)
* fix bug in catmaid_user_history with small queries (#70)

# catmaid 0.6

* support for upcoming httr 1.0 release
* fix: ensure catmaid connection objects are passed to all downstream functions
  e.g. so that you can you use different servers in the same session.
* fix queries that match multiple annotations when using a defined connection
  object
* fix: ensure that catmaid_fetch works even if server does not have a terminal /
  and remote path is also missing a slash
* fix: servers don't have to be https

# catmaid 0.5

This release 
* check for http status errors in catmaid (in case there is trouble connecting
  to site, bad URL etc)
* ... and errors in returned JSON when request is invalid
* export funcs to get/set login details as environment variables
* ... and clear those env vars (all useful for testing)

# catmaid 0.4

This release significantly enhances functionality for querying for neurons by
annotation or name, while also simplifying a number of plotting/analysis tasks
involving connectors (synapses).

* catmaid_query_by_neuronname was renamed to catmaid_query_by_name (since it can
  query by both neuron or annotation name as well as returning both annotations 
  and neurons)
* read.catmaid.neurons has richer attached metadata 
  (see https://github.com/jefferis/rcatmaid/issues/9)
* read.catmaid.neurons can make simple queries 
  (see https://github.com/jefferis/rcatmaid/issues/9)
* add catmaid_query_by_annotation to fetch objects tagged with a matching 
  annotation (rather than searching by object name).
* add connectors to get connector (synapse) information from a neuron
* add catmaid_catmaid_get_connector_table to see a list of incoming/outgoing
  connections for a given neuron.
* add plot3d.catmaidneuron to enable convenient plotting of synapses
* fix catmaid_get_neuronnames should return names in the same order as passed 
  skids (see https://github.com/jefferis/rcatmaid/issues/6)
* the catmaid skeleton_id is consistently referred to as skid in argument names
  and return values
* depend on nat (and therefore rgl). Import rather than depend on jsonlite.
* minor doc fixes

# catmaid 0.3

* cache login credentials for automatic reuse
* option to set/get environment variables for passwords etc
* Add catmaid_query_connected, catmaid_get_annotationlist, catmaid_query_by_neuronname
* low level catmaid_fetch function covers GET and POST requests
