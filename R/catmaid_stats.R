#' Get contributor statistics for neurons from CATMAID
#' 
#' @inheritParams read.neuron.catmaid
#' @return a list containing different statistics including construction and 
#'   review times (aggregated across all the specified input neurons). There 
#'   will also be 3 data.frames containing statistics for number of nodes and 
#'   pre/post-synaptic connectors broken down per user.
#'   
#'   \itemize{
#'   
#'   \item pre_contributors number of pre-synaptic connectors contributed per
#'   user.
#'   
#'   \item node_contributors number of skeleton nodes contributed per user.
#'   
#'   \item post_contributors number of post-synaptic connectors contributed per
#'   user.
#'   
#'   }
#'   
#' @export
#' @examples
#' \dontrun{
#' cs=catmaid_get_contributor_stats(skids=c(10418394,4453485))
#' # fetch user list
#' ul=catmaid_get_user_list()
#' # merge with list of node contributors and sort in descending order of
#' # contributions
#' library(dplyr)
#' left_join(cs$node_contributors, ul) %>%
#'   select(id,n, full_name) %>%
#'   arrange(desc(n))
#' }
#' @seealso \code{\link{catmaid_get_review_status}}
catmaid_get_contributor_stats<-function(skids, pid=1, conn=NULL, ...) {
  skids=catmaid_skids(skids, conn = conn, pid=pid)
  post_data=list()
  post_data[sprintf("skids[%d]", seq_along(skids)-1)]=as.list(skids)
  path=sprintf("/%d/skeleton/contributor_statistics_multiple", pid)
  res=catmaid_fetch(path, body=post_data, include_headers = F, conn=conn, ...)
  # convert lists to data.frames
  fix_list<-function(l) {
    if(!is.list(l)) return(l)
    df=data.frame(id=as.integer(names(l)), n=unlist(l, use.names = FALSE))
  }
  lapply(res, fix_list)
}

#' Fetch or translate catmaid users for current/specified connection/project
#' 
#' @inheritParams read.neuron.catmaid
#' @export
#' @return For \code{catmaid_get_user_list} a \code{data.frame} with 
#' @examples 
#' \donttest{
#' head(catmaid_get_user_list())
#' }
#' @seealso \code{\link{catmaid_get_review_status}, 
#'   \link{catmaid_get_contributor_stats}}
catmaid_get_user_list<-function(pid=1, conn=NULL, ...){
  catmaid_fetch('user-list', simplifyVector = T, pid=pid, conn=conn, ...)
}

#' @description \code{catmaid_userids} convert login names to numeric ids
#' @param x A character vector of login names. If \code{x} contains valid
#'   integer (or numeric) ids then they will be returned as is.
#' @return For \code{catmaid_userids} an integer vector of numeric ids
#' @rdname catmaid_get_user_list
#' @importFrom checkmate check_integerish asInteger
#' @export
#' @examples 
#' \donttest{
#' catmaid_userids(1)
#' catmaid_userids('greg')
#' }
catmaid_userids <- function(x, pid=1, conn=NULL, ...) {
  if(is.numeric(x)) {
    checkmsg=check_integerish(x)
    
    if(!isTRUE(checkmsg))
      stop("You have given me numeric input but some elements are not integers!",
         "\n  user ids must always be integers! Details:\n", checkmsg)
    return(asInteger(x))
  }
  # otherwise translate login names to ids
  ul=catmaid_get_user_list(pid=pid, conn=conn)
  mx=match(x, ul[['login']])
  ul[['id']][mx]
}

#' Fetch user contribution history
#' 
#' @param from Starting date for history
#' @param to End date for history (defaults to today's date)
#' @inheritParams read.neuron.catmaid
#'   
#' @return A data.frame with columns \itemize{
#'   
#'   \item full_name
#'   
#'   \item login
#'   
#'   \item id
#'   
#'   \item new_cable (in nm)
#'   
#'   \item new_connectors
#'   
#'   \item new_reviewed_nodes
#'   
#'   \item date
#'   
#'   }
#' @export
#' @importFrom dplyr bind_rows right_join as_data_frame
#' @examples
#' \dontrun{
#' catmaid_user_history(from="2016-01-01")
#' # last 2 days
#' catmaid_user_history(from = Sys.Date()-2)
#' }
catmaid_user_history <- function(from, to=Sys.Date(), pid=1L, conn=NULL, ...) {
  fromd=as.Date(from)
  if(fromd<as.Date("2001-01-01")) 
    stop("Invalid date: ",from, ". See ?Date for valid formats")
  tod=as.Date(to)
  if(tod<as.Date("2001-01-01")) 
    stop("Invalid date: ",to, ". See ?Date for valid formats")
  u=sprintf("%d/stats/user-history?pid=1&start_date=%s&end_date=%s", pid, fromd, tod)
  cf=catmaid_fetch(u, conn=conn, simplifyVector = T, ...)
  
  ul=catmaid_get_user_list(pid=pid, conn=conn, ...)
  ll=lapply(cf$stats_table, process_one_user_history)
  df=bind_rows(ll)
  # comes in with name new_treenodes but this is not correct
  names(df)[1]="new_cable"
  df$uid=rep(as.integer(names(cf$stats_table)), sapply(ll, nrow))
  df=right_join(ul[c("full_name","login","id")], df, by=c(id="uid"))
  as.data.frame(df)
}

#' catmaid_get_time_invested
#' 
#' 
#' Calculates the time individual users have spent working on a set of neurons
#' This API is a replica of the get_time_invested from python package `pymaid`.
#' The parameters `minimum_actions` and `max_inactive_time`  are used to fine tune how time
#' invested is calculated. By default, time is binned over 3 minutes in which a given user 
#' has to perform 3x10 actions for that interval to be counted as active.
#' The parameter `mode` can be one of 'SUM' or 'OVER_TIME' or 'ACTIONS'.
#' 'SUM' will return total time invested (in minutes) per user.
#' 'OVER_TIME' will return minutes invested/day over time.
#' 'ACTIONS' will return actions (node/connectors placed/edited) per day, for this choice 
#' the minimum_actions and max_inactive_time don't apply and hence every action is counted
#' @param skids could be skeleton id or neuron name or annotation
#' @param pid the project id
#' @param mode can be one of 'SUM','OVER_TIME','ACTIONS', where 'SUM' will return
#' the total time invested (in minutes), 'OVER_TIME' returns minutes invested per day over
#' time, 'ACTIONS' returns actions (node/connectors placed/edited) performed per day
#' @param minimum_actions the minimum number of actions per minute to be counted as active.
#' @param max_inactive_time Interval in minutes over which time invested is binned. Essentially determines how much time can be between bouts of activity.
#' @param treenodes Whether tree nodes need to be taken into account or not
#' @param connectors Whether connectors need to be taken into account or not
#' @param start_date The start date to compute activity from
#' @param end_date  The end date to compute end of activity at
#' @param conn  CATMAID connection instance
#' @param ... Additional arguments passed to the \code{\link{catmaid_fetch}} 
#'   function
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' tempval <- catmaid_skids('annotation:^ORN$')
#' skid_1 <-tempval[[1]]
#' catmaid_get_time_invested(skids=skid_1, mode='SUM')
#' }
catmaid_get_time_invested<-function(skids, pid=1, conn=NULL, mode=c('SUM','OVER_TIME','ACTIONS'),
                                    minimum_actions=10, max_inactive_time=3,
                                    treenodes=TRUE, connectors=TRUE, 
                                    start_date="2005-01-01",end_date="2099-01-01", ...) {
                                    
   #Step1: Process the arguments first..
    mode <- match.arg(mode)
    cat("\nUsing mode as : ", mode, "\n")
    # Maximal inactive time is the time interval bin in which no. of actions are computed (here 3 minutes)
    timeinterval <- max_inactive_time
    # The minimum number of actions parameter is per minute and hence it should be scaled 
    # for the given interval bin, hence here 10*3
    minimum_actions <- minimum_actions*timeinterval
    
    #Check the validity of the dates now..
    start_date_dt=as.POSIXct(as.POSIXct(start_date, tz = "UTC"), format = "%Y-%m-%dT%H:%M:%S")
    if(start_date_dt<as.POSIXct("2001-01-01", tz = "UTC")) 
      stop("Invalid date: ",start_date, ". See ?POSIXct for valid formats")
    end_date_dt = as.POSIXct(as.POSIXct(end_date, tz = "UTC"), format = "%Y-%m-%dT%H:%M:%S")
    if(end_date_dt<as.POSIXct("2001-01-01", tz = "UTC")) 
      stop("Invalid date: ",end_date, ". See ?POSIXct for valid formats")
    
    #Check the validity of the arguments now..
    if (treenodes ==  FALSE && connectors && FALSE) {stop("Stats cannot be performed as both treenodes and connectors are set
                                                          to FALSE")}
  
  #Step2: Convert the queries (whether regex or character or integer) to integer skeleton ids..
    skids <- catmaid_skids(skids, conn = conn, pid=pid)
  
  #Step3: Get the user list now..
    user_list <- catmaid_get_user_list(pid=pid, conn=conn)
  
  #Step4: Extract node and connector ids, tags for the individual neurons..
    list_cf_df <- extract_nodeconnectorids(skids,pid,connectors,conn =conn)
    
    #Get the ids of the nodes and connector_id of connectors
    overallcf_df ={}
    for (listidx in seq_along(list_cf_df)){
      overallcf_df$nodes$id <- c(list_cf_df[[listidx]]$nodes$id, overallcf_df$nodes$id)
      overallcf_df$connectors$connector_id <- c(list_cf_df[[listidx]]$connectors$connector_id, 
                                                overallcf_df$connectors$connector_id)
    }
    
    #Extract creator and editor ids for the individual neurons..
    node_ids = NULL
    connector_ids = NULL
    if (treenodes) {node_ids = overallcf_df$nodes$id}
    if (connectors) {connector_ids = overallcf_df$connectors$connector_id}
    # Convert ids to ints to ensure that they are formatted cleanly in the JSON post
    # body in the loop coming up
    connector_ids <- connector_ids[!is.na(connector_ids)]
    all_ids = as.integer(c(node_ids,connector_ids))
    #Remove elements that have na usually nodes with no connectors
    all_ids <- all_ids[!is.na(all_ids)]
    #Choose only unique ids as there could be overlap
    all_ids <- unique(all_ids)
    
  #Step5: Get user details for the node_ids, connector_ids..
    #Do the user details in chunks of 1000 so the CATMAID server can process them..
    chunkall_ids <- split(all_ids, ceiling(seq_along(all_ids)/20000))
    path=file.path("", pid, "node", "user-info")
    
    res = {}
    df_res <- NULL
    for (listidx in seq_along(chunkall_ids)){
      post_data=list()
      post_data[sprintf("node_ids[%d]", seq_along(chunkall_ids[[listidx]]))]=as.list(chunkall_ids[[listidx]])
      singleres=catmaid_fetch(path, body=post_data, include_headers = F, simplifyVector = T, conn=conn, ...)
      if(catmaid_error_check(singleres, stoponerror=FALSE)) {
        message("Failed to process chunk", listidx)
      } else res=append(res, singleres)
    }
  
    #Do some post processing on the details..
    #unlist while retaining the empty lists..
   # Take the first 4 cols user editor, creation/edition times
    node_details_df=list2df(res, cols = names(res[[1]][1:4]))
    node_details_df$node_id=names(res)
    
    # now handle review times separately
    review_details=res[sapply(res, function(x) length(x$reviewers))>=1]
    if (length(review_details) >= 1){
        review_details_df = data.frame(reviewer = unlist(lapply(review_details, '[[', 'reviewers'), 
                                                         use.names = F),
        review_time = unlist(lapply(review_details, '[[', 'review_times'), use.names = F),
                              stringsAsFactors = FALSE)
        review_details_df$node_id = rep(as.integer(names(review_details)),
                                    sapply(review_details, function(x)
                                      length(x$reviewers)))
    } else {
      review_details_df = data.frame(reviewer = character(),review_time = character(),
                                     node_id = integer())
    }
    
    # Rename column 'user' to 'creator'
    colnames(node_details_df)[colnames(node_details_df)=="user"] <- "creator"
  
    #Convert character to date format 
    node_details_df$creation_time <- as.POSIXct(strptime(node_details_df$creation_time,
                                                         format = "%Y-%m-%dT%H:%M", tz = "UTC"))
    node_details_df$edition_time <- as.POSIXct(strptime(node_details_df$edition_time, 
                                                        format = "%Y-%m-%dT%H:%M", tz = "UTC"))
    review_details_df$review_time <- as.POSIXct(strptime(review_details_df$review_time, 
                                                          format = "%Y-%m-%dT%H:%M",tz = "UTC"))
  
  #Step6: Get details about links..
    if (connectors && !all(is.na(connector_ids))) {
      path=file.path("", pid, "connectors", "links")
      path = paste(path,"?relation_type=", sep ="")
    
      types_connectors = c("presynaptic_to","postsynaptic_to","abutting","gapjunction_with")
      path_multiple = paste(path,types_connectors, sep="")
      
      links_all = list()
      
      for (skididx in seq_along(skids)){
          u = paste("&skeleton_ids%5B0%5D=",skids[skididx], sep="")
          querypath = paste(path_multiple, u, sep="")
          links_temp = list()
          for (linksidx in seq_along(querypath)){
              connectorslist_temp = list(catmaid_fetch(querypath[linksidx], 
                                                       include_headers = F, simplifyVector = T, 
                                                       conn=conn, ...))
              names(connectorslist_temp) <- types_connectors[linksidx]
              links_temp <- c(links_temp,connectorslist_temp)
          }
              links_temp$skid <- skids[skididx]
              links_all[[skididx]] <- links_temp
      }
    
    #Now create a dataframe with each elements..
    links_collec = {}
    for (skididx in seq_along(skids)){
        for (connectypeidx in seq_along(types_connectors)){
            if (length(links_all[[skididx]][[connectypeidx]]$links)) {
            links_collec <- rbind(links_collec, data.frame(links_all[[skididx]][[connectypeidx]]$links, 
                                                     relationtype = types_connectors[connectypeidx], 
                                                     stringsAsFactors=FALSE))
            }
        }
    }
    
    
    colnames(links_collec) <- c("skeleton_id", "connector_id","x","y","z","confidence",
                  "creator_id","treenode_id","creation_time","edition_time","relationtype")
    
    #Convert character to date format
    links_collec$creation_time <- as.POSIXct(strptime(links_collec$creation_time,format = "%Y-%m-%dT%H:%M", 
                                             tz = "UTC"))
    links_collec$edition_time <- as.POSIXct(strptime(links_collec$edition_time, format = "%Y-%m-%dT%H:%M",
                                            tz = "UTC"))
    
    #Choose only those links that have matching connector ids in the neuron???
    links_collec <- links_collec[links_collec$connector_id %in% connector_ids,]
    
    } else{
      #Just initalize with empty here..
      links_collec <- {}
    }
  
    node_details <- node_details_df
    review_details <- review_details_df
    if (!is.null(links_collec)){
      link_details <- links_collec
      colnames(link_details)[colnames(link_details)=="creator_id"] <- "creator"
    }
 
  #Step7: Remove timestamps outside of date range (if provided)
    if(start_date_dt){
      node_details = node_details[node_details$creation_time >= start_date_dt,]
      review_details = review_details[review_details$review_time >= start_date_dt,]
      if (!is.null(links_collec)){
          link_details = link_details[link_details$creation_time >= start_date_dt,]}}
    if(end_date_dt){
      node_details = node_details[node_details$creation_time <= end_date_dt,]
      review_details = review_details[review_details$review_time <= end_date_dt,]
      if (!is.null(links_collec)){
          link_details = link_details[link_details$creation_time <= end_date_dt,]}}
  
  #Step8a:  Create dataframes for the creation of items
    creation_timestamps <- {}
    if (!is.null(links_collec)){
    creation_timestamps <- rbind( link_details[, c('creator', 'creation_time')],
                                  node_details[, c('creator', 'creation_time')])}
    else{
    creation_timestamps <- node_details[, c('creator', 'creation_time')] 
    }
    colnames(creation_timestamps) <- c('user', 'timestamps')

  
  #Step8b:  Create dataframes for edition of items (you can't use links as there is no 
    # editor associated with..)
    edition_timestamps <- node_details[, c('editor', 'edition_time')]
    colnames(edition_timestamps) <- c('user', 'timestamps')
  
  #Step8c:  Create dataframes for review items
   
    review_timestamps <- review_details[, c('reviewer', 'review_time')]
    colnames(review_timestamps) <- c('user', 'timestamps')
    review_timestamps <- review_timestamps[!is.na(review_timestamps$timestamps),]
    
   
  #Step8d:  Merge all the dataframes timestamps
    all_timestamps <- rbind(creation_timestamps, edition_timestamps, review_timestamps)
    all_timestamps <- na.omit(all_timestamps)
  
  #Step9: Choose only users that have performed a minimal number of actions..
    relevant_users <- plyr::count(all_timestamps, vars = "user")
    relevant_users <- relevant_users[relevant_users$freq >=minimum_actions,"user"]
    relevant_users <- relevant_users[!relevant_users == "NA"]
    
  #Step10: Update the stats now..
    #Step10a: Time invested stats for 'SUM'
      if(mode=='SUM'){
        stats_df = data.frame(users=character(),login = character(), total = double(),
                              creation = double(),edition = double(),review = double(),
                              stringsAsFactors = FALSE)
        if (length(relevant_users)>0){
          stats_df[1:length(relevant_users),'users'] <- relevant_users  
          stats_df[,c('total','creation','edition','review')] <- 0
        
          total_agg_df <- aggregate_timestamps(all_timestamps,minimum_actions,timeinterval)
          creation_agg_df <- aggregate_timestamps(creation_timestamps,minimum_actions,timeinterval)
          edition_agg_df <- aggregate_timestamps(edition_timestamps,minimum_actions,timeinterval)
          review_agg_df <- aggregate_timestamps(review_timestamps,minimum_actions,timeinterval)
        
        
          for (useridx in seq_along(stats_df$users)){
              matchidx <- stats_df[useridx,'users']  == user_list$id
              if (any(matchidx)){stats_df[useridx,'login'] <- user_list[matchidx,'login']}
          
              matchidx <- stats_df[useridx,'users']  == total_agg_df$users
              if (any(matchidx)){stats_df[useridx,'total'] <- timeinterval*total_agg_df[matchidx,'x']} 
          
              matchidx <- stats_df[useridx,'users'] == creation_agg_df$users
              if (any(matchidx)){stats_df[useridx,'creation'] <- timeinterval*creation_agg_df[matchidx,'x']}
          
              matchidx <- stats_df[useridx,'users'] == edition_agg_df$users
              if (any(matchidx)){stats_df[useridx,'edition'] <- timeinterval*edition_agg_df[matchidx,'x']}
          
              matchidx <- stats_df[useridx,'users'] == review_agg_df$users
              if (any(matchidx)){stats_df[useridx,'review'] <- timeinterval*review_agg_df[matchidx,'x']}
            }
        
          stats_df <- stats_df[sort(stats_df$total,decreasing = TRUE, index.return = TRUE)$ix,]
          }
        
        }
    #Step10b: Time invested stats for 'OVER_TIME'
      if(mode=='OVER_TIME'){
        
        #minutes invested/day over time.
        
        # Count only those minutes with the minimum number of actions
        agg_timestamps <- all_timestamps %>% 
          dplyr::mutate(interval = lubridate::floor_date(.data$timestamps, unit="hour")+
                          lubridate::minutes(floor(lubridate::minute(.data$timestamps)/timeinterval)
                                             *timeinterval)) %>% 
          dplyr::group_by(.data$interval, .data$user) %>% 
          dplyr::summarise(count=dplyr::n()) %>%
          dplyr::arrange(dplyr::desc(.data$count))
        
        agg_timestamps$status <- 0
        agg_timestamps[agg_timestamps$count>=minimum_actions,'status'] = 1
        temp_agg_df <- agg_timestamps[agg_timestamps$status == 1,]
        
        # Now group per day (number of minutes per day)
        
        overtime_daystamps <- temp_agg_df %>% 
                  dplyr::mutate(dayinterval = lubridate::floor_date(.data$interval, unit="day")) %>% 
                  dplyr::group_by(.data$dayinterval, .data$user) %>%
                  dplyr::summarise(count2=sum(.data$status))
        
       #Now find out the user names..
        stats_df = overtime_daystamps
       #Now rename the columns..
        names(stats_df)[names(stats_df) == 'dayinterval'] <- 'day'
        names(stats_df)[names(stats_df) == 'count2'] <- 'minutes'
        names(stats_df)[names(stats_df) == 'user'] <- 'users'
        if (nrow(stats_df)>0){
            stats_df$login <- NA
        
            for (useridx in seq_along(stats_df$users)){
                matchidx <- as.numeric(stats_df[useridx,'users'])  == user_list$id
                if (any(matchidx)){stats_df[useridx,'login'] <- user_list[matchidx,'login']}
            }
        
        
            statsrecast_df <- reshape2::dcast(stats_df, login ~ day, value.var = "minutes")
        
            statsrecast_df[is.na(statsrecast_df)] <- 0
            if (nrow(statsrecast_df) > 1){
                statsrecast_df[nrow(statsrecast_df)+1,2:ncol(statsrecast_df)]  <- colSums(statsrecast_df[,2:ncol(statsrecast_df)])}
            else {
                statsrecast_df[nrow(statsrecast_df)+1,2:ncol(statsrecast_df)] <- statsrecast_df[,2:ncol(statsrecast_df)]
            }
            statsrecast_df[nrow(statsrecast_df),1] <- 'all_users'
        
            stats_df <- statsrecast_df
        }
        
      }
    
    #Step10c: Time invested stats for 'ACTIONS'
      if(mode=='ACTIONS'){
        
        actions_timestamps <- all_timestamps %>% 
          dplyr::mutate(interval = lubridate::floor_date(.data$timestamps, unit="day")) %>% 
          dplyr::group_by(.data$interval, .data$user) %>% 
          dplyr::summarise(count=dplyr::n()) %>%
          dplyr::arrange(dplyr::desc(.data$count))
        
        #recast it into columns with date
        stats_df <- reshape2::dcast(actions_timestamps, user ~ interval, value.var = "count")
        
        for (useridx in seq_along(stats_df$user)){
          matchidx <- stats_df[useridx,'user']  == user_list$id
          if (any(matchidx)){stats_df[useridx,'login'] <- user_list[matchidx,'login']}
        }
        
        #summarise the data in a neat format..
        stats_df[is.na(stats_df)] <- 0
        stats_df$user <- NULL
        stats_df <- stats_df %>%
                    dplyr::select(.data$login,dplyr::everything())
        if (nrow(stats_df) > 1){
            stats_df[nrow(stats_df)+1,2:ncol(stats_df)] <- colSums(stats_df[,2:ncol(stats_df)])}
        else{
            stats_df[nrow(stats_df)+1,2:ncol(stats_df)] <- stats_df[,2:ncol(stats_df)]}
          
        stats_df[nrow(stats_df),1] <- 'all_users'
        
      }
    
    stats_df$users <- NULL
    return(stats_df)
    
}

extract_nodeconnectorids <- function(skids,pid,connectors, conn){
  list_cf_df = {}
  for (skididx in seq_along(skids)){
      skid = skids[skididx]
      path=file.path("", pid, "skeletons", skid, "compact-detail")
      u = sprintf("?with_history=false&with_tags=true&with_merge_history=false")
      u = paste(u,'&with_connectors=', tolower(connectors),sep = "")
      path = paste(path,u, sep ="")
    
      #rename them accordingly
      cf=catmaid_fetch(path,simplifyVector = T, conn=conn)
    
      if ("error" %in% names(cf)){
          cat('Skipping skid: ',skid, '\n')
      }
      else{
          cat('Processing skid: ',skid, '\n')
          names(cf)=c("nodes", "connectors", "tags")
          colnames(cf[["nodes"]]) <- c("id", "parent_id", "user_id", "x", "y", "z",
                                   "radius", "confidence")
          if (length(cf[["connectors"]]) == 0)
          {
            cf[["connectors"]] <- matrix(ncol = 6, nrow = 1)
          }
          colnames(cf[["connectors"]]) <- c("treenode_id", "connector_id", "relation", 
                                        "x", "y", "z")
      
          #convert them as tibbles for ease of manipulation
          cf_df = {}
          cf_df$nodes = dplyr::as_tibble(as.matrix(cf[["nodes"]]))
          cf_df$connectors = dplyr::as_tibble(as.matrix(cf[["connectors"]]))
          cf_df$tags = cf[["tags"]]
      
          #Get the neuron name
          cf_df$neuron_name <- catmaid_get_neuronnames(skid, conn=conn)
      
          list_cf_df[[skididx]] <- cf_df
    }
  }
  
  return(list_cf_df)
}

aggregate_timestamps <- function(timestamps_collec,minimum_actions,timeinterval){
  
  timestamps_collec <- timestamps_collec[stats::complete.cases(timestamps_collec), ]
  if(nrow(timestamps_collec) >0){
          agg_timestamps <- timestamps_collec %>% 
                            dplyr::mutate(interval = lubridate::floor_date(.data$timestamps, unit="hour")+
                            lubridate::minutes(floor(lubridate::minute(.data$timestamps)/timeinterval)
                                                      *timeinterval)) %>% 
                            dplyr::group_by(.data$interval, .data$user) %>% 
                            dplyr::summarise(count=dplyr::n()) %>%
                            dplyr::arrange(dplyr::desc(.data$count))
          agg_timestamps$status <- 0
          agg_timestamps[agg_timestamps$count>=minimum_actions,'status'] = 1
          temp_agg_df <- stats::aggregate(x = agg_timestamps$status, 
                                          by=list(users=agg_timestamps$user), FUN=sum)
  } else{
    temp_agg_df <- NULL
  }
  
  return(temp_agg_df)
}

replace_emptylist <- function(x) {
  if(length(x) == 0) {return('NA')}
  else if (length(x) == 1) {return(x)}
  else return(x)
}


process_one_user_history <- function(x) {
  slx=sum(sapply(x, length))
  if(slx<1) {
    empytydf=dplyr::tibble(new_treenodes = numeric(0), new_connectors = integer(0), 
           new_reviewed_nodes = integer(0), date = structure(numeric(0), class = "Date"))
    return(empytydf)
  }
  df=bind_rows(lapply(x, as_data_frame))
  dates=as.Date(names(x), format="%Y%m%d")
  df$date=rep(dates, sapply(x, function(x) length(x)>0))
  df
}
