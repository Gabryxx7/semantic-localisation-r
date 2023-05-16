## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sourceCpp("Notebooks/Utils/checking_functions.cpp")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trace.env$check_set <- function(agent_set, agents_in_room){
  if("not" %in% attr(agent_set, "modifiers")){ 
    if(is.null(agent_set) || length(agent_set) <= 0)
      return(TRUE)
    res <- !any(agent_set %in% agents_in_room)
    prefix <- trace.env$OP$negation
  }
  else{
    if(is.null(agent_set) || length(agent_set) > length(agents_in_room))
      return(FALSE)
    prefix <- ""
    res <- all(agent_set %in% agents_in_room)
  }
  # cat("\nCheck Sets: ", prefix,paste0(agent_set, collapse=","), "\t", paste0(agents_in_room, collapse=","), "\t", paste0(res, collapse=","))
  return(res)
}

trace.env$check_sets <- function(agent_set_list, agents_in_room){
  if(is.null(agent_set_list) || length(agent_set_list) <= 0){
    return(TRUE)
  }
  sets_res <- unlist(lapply(agent_set_list, check_set, agents_in_room))
  return(all(sets_res == TRUE))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trace.env$check_group <- function(agent_group, agents_in_room){
  if(is.null(agent_group))
    return(TRUE)
  return(any(agent_group %in% agents_in_room))
}

trace.env$check_groups <- function(agent_group_list, agents_in_room){
  if(is.null(agent_group_list) || length(agent_group_list) <= 0){
    return(TRUE)
  }
  groups_res <- unlist(lapply(agent_group_list, check_group, agents_in_room))
  return(all(groups_res == TRUE))
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trace.env$check_range <- function(agentsList, min=-Inf, max=Inf){
  total <- length(agentsList)
  if(is.null(min)){
    min = -Inf
  }
  
  if(is.null(max)){
    max = Inf
  }
  
  return(min <= total & total <= max)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Get unique cells in a room
trace.env$unique_ids <- function(room_data){
  if(is.null(room_data)) return(c())
  if(is.list(room_data) || is.vector(room_data)){
    return(unlist(room_data))
  }
  separator <- ","
  return(unique(unlist(strsplit(unique(room_data), separator))))
}

## Get the total number of agents in the room 
trace.env$total_ids <- function(room_data){
  ids_list <- unique_ids(room_data)
  total <- length(ids_list)
  return(total)
}



## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trace.env$check_cell <- function(room_data, sets, groups, range, debug=FALSE){
  in_range <- TRUE
  all_in_room <- TRUE
  at_least_one <- TRUE
  
  ids_list <- unique_ids(room_data)
  if(length(range) > 0) in_range <- check_range(ids_list, range[[1]], range[[2]])
  if(length(sets) > 0) all_in_room <- check_sets(sets, ids_list)
  if(length(groups) > 0) at_least_one <- check_groups(groups, ids_list)
  if(debug){
    cat("\nIn room: ", paste0(ids_list, collapse=","), sep="")
    cat("\nSets: ", paste0(sets, collapse=","), "\t", all_in_room, sep="")
    cat("\nGroups: ", paste0(groups, collapse=","), "\t", at_least_one, sep="")
    cat("\nRange: ", paste0(range, collapse=","), "\t", in_range, sep="")
    cat("\n")
  }
  return(in_range & all_in_room & at_least_one)
}

