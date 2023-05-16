## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(data.table)
library(stringr)
library(matrixStats)
library(utils)
library(Rcpp)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
asString <- FALSE
Rcpp::sourceCpp('Notebooks/Utils/utility_functions.cpp', env=environment())
# Retrieving vectorized column_vector_append


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# e.g.
# get_cols_classes(df, type="POSIXct|POSIXt", not.type="numeric")
get_cols_classes <- function(df, type=NULL, not.type=NULL){
  col.types <-utils::stack(lapply(df, function(x) paste(class(x), collapse = ',')))
  colnames(col.types) <- c("class", "name")
  col.types$name <- as.character(col.types$name)
  if(!is.null(type)){
    col.types <- col.types %>% filter(class %like% type)
  }
  if(!is.null(not.type)){
    col.types <- col.types %>% filter(!(class %like% not.type))
  }
  return(col.types)
}

assign("get_cols_classes", get_cols_classes, envir=environment())


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
build_trace_dataset <- function(timeSlots, roomNames, asString=FALSE){
  # Start with creating an empty dataframe with only time slots as rows
  newDT <- data.table(time=timeSlots)
  # Add a new column for each device in the list of devices
  for(x in roomNames){
    roomName <- as.character(x)
    if(asString){
      newDT[, (roomName) := ""]
    }
    else{
      newDT[, (roomName) := list()]
    }
  }
  # Convert the colnames to trimmed string just in case, good for regexes
  colnames(newDT) <- str_trim(colnames(newDT))
  return(newDT)
}

assign("build_trace_dataset", build_trace_dataset, envir=environment())


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combine_datasets <- function(x, DT, beaconsList = NULL, asString = TRUE){
  minor <- x$minor[[1]]
  progress <- ""
  if(!is.null(beaconsList)){
    index <- which(beaconsList==minor)
    total <- length(beaconsList)
    progress <- paste0(index, "/", total)
  }
  cat("\n", progress, "- ID:",minor,"\tRooms Processed: ")
  separator <- ","
  for(i in 2:length(colnames(DT))){
    roomsVisited <- x$nroom
    roomCol <- colnames(DT)[[i]]
    roomColNum <- as.numeric(roomCol)
    cat(roomCol, " ")
    if(asString){
      x$minor <- ifelse(x$nroom!=roomColNum, "", paste(separator,minor,sep=""))
      DT[, (roomCol) := paste(DT[[roomCol]], x$minor, sep="")]
    }
    else{
      # x$minor <- ifelse(x$nroom!=roomColNum, NULL, minor)
      DT[[roomCol]] <- column_vector_append(DT[[roomCol]], x$nroom, as.character(roomColNum), as.character(minor))
      # DT[, (roomCol) := column_vector_append(DT[[roomCol]], x$nroom, as.character(roomColNum), as.character(minor))]
      # tripsMatrix[rowRoom,] <<- list(tripsMatrix[rowRoom,], beaconRoomData$minor)
    }
  }
  return(DT);
}
assign("combine_datasets", combine_datasets, envir=environment())

# Fit an irregular time series into regular time slots interval based on the first and last timestamps
regularise <- function(.data, fromCol, toCol, areasCol, agentsCol, granularity="30 secs"){
  fit_irregular_data <- function(.data, agentsCol, regularTS){
    fill_cells <- function(regularTS, from, to, area, agent){
      # cat(from, " ", to[[1]], " ", area[[1]], " ", agent[[1]], "\n")
      # cat(paste0('regularTS[timeSlot >= from & timeSlot <= to, "',area[[1]],'" := paste0(.SD[["',area[[1]],'"]],agent[[1]],sep=",")]'))
      eval(parse(text=paste0('regularTS[timeSlot >= from & timeSlot <= to, "',area,'" := paste(.SD[["',area,'"]],agent,sep=",")]')))
      #testConcat[ID=="C30", Building := paste(testConcat[ID=="C30"]$Building, "yo", sep=",")]
      return()
    }
    .data[, fill_cells(regularTS, from, to, Building, ID), by = 1:nrow(.data)]
    return(regularTS)
  }
  minTime <- min(.data[[fromCol]])
  maxTime <- max(.data[[toCol]])
  rts <- seq(minTime, maxTime, granularity)
  areas <- unique(.data[[areasCol]])
  
  newData <- data.table(timeSlot = rts)
  
  for (j in areas){
    set(newData, j, value = "")
  }
  newData <- fit_irregular_data(.data, agentsCol, newData)
  return(newData)
}
assign("regularise", regularise, envir=environment())


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
read.trace.csv <- function(file.name, time.col=NULL, room.cols=NULL){
  cols.classes <- c()
  if(!is.null(time.col)){
    cols.classes[time.col] <- "POSIXct"
  }
  trace <- fread(file.name, colClasses=cols.classes)
  cols.to.remove <- get_cols_classes(trace, not.type = "character|POSIXct|POSIXt|list")
  set(trace, , cols.to.remove$name, NULL)
  if(is.null(room.cols)){
    room.cols <- get_cols_classes(trace, type = "character")$name
  }
  trace[, (room.cols)] <- lapply(trace[, ..room.cols], function(cells) lapply(cells, function(cell) c(str_split(cell, ",")[[1]])))
  return(trace)
}
assign("read.trace.csv", read.trace.csv, envir=environment())


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.trace.csv <- function(trace, file.name, room.cols=NULL, non.room.cols=NULL){
  cols <- colnames(trace)
  if(!is.null(room.cols)){
    cols <- room.cols
  }
  if(!is.null(non.room.cols)){
    cols <- cols[!(cols %in% non.room.cols)]
  }
  .GlobalEnv[["cols"]] <- cols
  trace[, (cols)] <- lapply(trace[, ..cols], function(cells) unlist(lapply(cells, function(cell) paste0(cell, collapse=","))))
  write.csv(trace, file.name)
}
assign("write.trace.csv", write.trace.csv, envir=environment())


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## extract_agent_rows <- function(.data, agent, fromTime, toTime, areas, keep_columns=TRUE){
##   if(keep_columns){
##     return(.data[.data[, Reduce(`|`, lapply(.SD, data.table::like, agent, fixed=TRUE)), .SDcols = areas]])
##   }else{
##     filtered <- which(mapply(function(x) data.table::like(x, "C30\\D"), data), arr.ind=TRUE)
##     filteredRes <- .data[filtered[,1], area := .SD[filtered[,1],filtered[,2]]][filtered[,1]]
##   }
##   # testReduce <- extract_agent_rows(test, "C31", NA, NA, 2:20)
## }
## 
## which_room <- function(.data, agent){
##   return(which(apply(.data, 2, data.table::like, paste0(agent,"\\D")),arr.ind=TRUE))
## }
## 
## # Contact tracing: Which other agents did "agent" meet? and when?
## extract_encounters <- function(.data, agent, agentsEncountered=NULL, fromTime, toTime, areas, minEncounterTime){
##   encounters_all <- NULL
##   fromTime <- ifelse(is.null(fromTime), min(.data$timeSlot), fromTime)
##   toTime <- ifelse(is.null(toTime), max(.data$timeSlot), toTime)
##   data <- .data[timeSlot >= fromTime & timeSlot <= toTime, areas]
##   agent_in_area <- data[data[, Reduce(`|`, lapply(.SD, data.table::like, agent, fixed=TRUE)), .SDcols = areas]]
##   agentsEncountered <- ifelse(is.null(agentsEncountered), unique(unlist(apply(agent_in_area[,-1], c(1,2), strsplit, ","))), agentsEncountered)
##   agentsEncountered <- agentsEncountered[agentsEncountered != agent]
##   for(agentEncountered in agentsEncountered){
##     encounters <- agent_in_area[agent_in_area[, Reduce(`|`, lapply(.SD, data.table::like, agent, fixed=TRUE)), .SDcols = areas]]
##   }
##   # Together RegExp
##   # ^(?=.*\bC30\b)(?=.*\bC258\b).*$
## 
## }
## 
## filter_areas <- function(.data){
## 
## }
## 
## filter_trips <- function(.data, agents, fromAreas, toAreas, together=TRUE, fuzzy=TRUE){
##   Rcpp::cppFunction(plugins = "unwindProtect", code='
##    NumericVector group_transitions(LogicalVector fromRooms, LogicalVector toRooms) {
##      NumericVector groups(fromRooms.size());
##      int lastGroupIndex = 0;
##      for(int i=0; i < fromRooms.size(); ++i){
##         groups[i] = 0;
##         if(fromRooms[i]){
##           lastGroupIndex++;
##           i++;
##           while(!toRooms[i] && i < fromRooms.size()){
##             groups[i] = lastGroupIndex;
##             i++;
##           }
##           while(toRooms[i] && i < fromRooms.size()){
##             groups[i] = lastGroupIndex;
##             i++;
##           }
##         }
##      }
##      return groups;
##    }')
## 
##   data <- .data
##   logicOp <- ifelse(together==TRUE, ",", "|")
##   pattern <- paste(agents, collapse=logicOp)
##   arrow <- ifelse(fuzzy==TRUE, "->", "|->")
##   expression <- paste0("({",pattern, "},", "{",paste(fromAreas, collapse="|"), "}) ", arrow, " ({",paste(agents, collapse=","), "},", "{",paste(toAreas, collapse="|"), "})" )
## 
##   fromAreas <- c(fromAreas)
##   toAreas <- c(toAreas)
##   groups <- NA
##   resData <- NA
##   fromAreasRes <- rep(FALSE, nrow(data))
##   toAreasRes <- rep(FALSE, nrow(data))
##   fromAreaTemp <- NULL
##   toAreaTemp <- NULL
## 
##   for(fromArea in fromAreas){
##     for(agent in agents){
##       if(is.null(fromAreaTemp)){
##         fromRoomTemp <- data[,get(fromRoom)] %like% beacon
##       }
##       else{
##         if(together)
##           fromAreaTemp <- fromAreaTemp & data[,get(fromArea)] %like% agent
##         else
##           fromAreaTemp <- fromAreaTemp | data[,get(fromArea)] %like% agent
##       }
##       #cat("\nfromRoomTemp: ", unique(fromRoomTemp))
##     }
##     fromAreasRes <- fromAreasRes | fromAreaTemp
##     #cat("\nfromRoomRes: ", unique(fromRoomRes))
##   }
## 
##   for(toRoom in toRooms){
##     for(beacon in beacons){
##       if(is.null(toRoomTemp)){
##         toRoomTemp <- data[,get(toRoom)] %like% beacon
##       }
##       else{
##         if(together)
##           toRoomTemp <- toRoomTemp & data[,get(toRoom)] %like% beacon
##         else
##           toRoomTemp <- toRoomTemp | data[,get(toRoom)] %like% beacon
##       }
##       #cat("\ntoRoomTemp: ", unique(toRoomTemp))
##     }
##     toRoomRes <- toRoomRes | toRoomTemp
##     #cat("\ntoRoomRes: ", unique(toRoomRes))
##   }
##   data[ , groupid := group_transitions(fromRoomRes, toRoomRes)]
##   data$fromRoomRes <- fromRoomRes
##   data$toRoomRes <- toRoomRes
##   groups <- data[,.(fromTime=min(time), toTime=max(time), beacons=pattern), by=groupid]
## 
##   return(list(data = data, groups=groups[groupid > 0], beacons=beacons, fromRoom=fromRoomRes, toRoom=toRoomRes, pattern=pattern, expression=expression))
## 
## }
## 
## 
## 
## select_beacons <- function(.data, beacons){
##   return(list(data=.data, beacons=beacons))
## }
## 
## from_room <- function(.data, room){
##   .data[["fromRoom"]] <- room
##   return(.data)
## }
## 
## to_room <- function(.data, room){
##   .data[["toRoom"]] <- room
##   return(.data)
## }
## 
## 
## 
## with_beacon <- function(.data, beacon, room=NA){
##   data <- .data$data
##   groups <- .data$groups
##   groups$keep <- FALSE
##   fromRoom <- .data$fromRoom
##   toRoom <- .data$toRoom
##   .data$with <- beacon
##   for(i in 1:nrow(groups)){
##     instance <- data[time >= groups[i]$fromTime & time <= groups[i]$toTime]
##     if(is.na(room)){
##       instance <- instance[get(fromRoom) %like% beacon | get(toRoom) %like% beacon]
##     }
##     else{
##       instance <- instance[get(room) %like% beacon]
##     }
## 
##     if(nrow(instance) > 0){
##       groups[i]$keep <- TRUE
##     }
##     else{
##       groups[i]$keep <- FALSE
##       data <- data[!(time >= groups[i]$fromTime & time <= groups[i]$toTime)]
##       cat("Removed group: \t",groups[i]$groupid, "\n")
##     }
##   }
## 
## 
##   .data$groups <- groups[keep==TRUE]
##   .data$groups$keep <- NULL
## 
##   return(.data)
## 
## }
## 
## 
## visiting_room <- function(.data, room){
##   data <- .data$data
##   groups <- .data$groups
##   groups$keep <- FALSE
##   fromRoom <- .data$fromRoom
##   toRoom <- .data$toRoom
##   pattern <- .data$pattern
##   for(i in 1:nrow(groups)){
##     instance <- data[time >= groups[i]$fromTime & time <= groups[i]$toTime][get(room) %like% pattern]
## 
##     if(nrow(instance) > 0){
##       groups[i]$keep <- TRUE
##     }
##     else{
##       groups[i]$keep <- FALSE
##       data <- data[!(time >= groups[i]$fromTime & time <= groups[i]$toTime)]
##       cat("Removed group: \t",groups[i]$groupid, "\n")
##     }
##   }
##   .data$groups <- groups[keep == TRUE]
##   .data$data <- data
## 
##   return(.data)
## }
## 
## ## SUMMARY FUNCTIONS
## 
## print_beacon_transitions <- function(.data, beacon, groupid=NA){
##   data <- .data$data
##   if(!is.na(groupid)){
##     data <- get_group_data(.data, groupid)
##   }
##   groups <- .data$groups
##   fromRoom <- .data$fromRoom
##   toRoom <- .data$toRoom
##   for(i in 1:nrow(groups)){
##     cat("Trip ",groups$groupid[i], ":")
##     instance <- data[time >= groups[i]$fromTime & time <= groups[i]$toTime]
##     #instance[, room := do.call(testFun, list(.SD, beacon, colnames(.SD))), .SDcols = -1]
##     #cat(paste(instance$room, collapse=" "),"\n")
##     idx <- apply( instance, 1, function(row) which( grepl(beacon, row) ) )
##     cat(paste(colnames(instance)[unlist(idx)], collapse=" "),"\n")
##   }
##   return(.data)
## }
## 
## 
## unique_beacons_in_room <- function(.data, room, groupid=NA){
##   data <- .data$data
##   if(!is.na(groupid)){
##     data <- get_group_data(.data, groupid)
##   }
##   return(sort(as.numeric(unique(unlist(strsplit(unique(data[[room]]), ","))))))
## }
## 
## get_group_data <- function(.data, pgroupid){
##   group <- .data$groups[groupid == pgroupid][1]
##   return(.data$data[time >= group$fromTime & time <= group$toTime])
## }
## 
## calculate_groups_starting_room <- function(.data){
##   .data$groups$startingRoom <- ""
##   for(i in 1:nrow(.data$groups)){
##     group <- .data$groups[i]
##     data <- get_group_data(.data, group$groupid)[1]
##     room <- colnames(.data$data)[which(grepl(group$beacons, data))]
##     room <- ifelse(length(room) <= 0, "NA", room)
##     .data$groups[i, startingRoom := room]
##   }
## 
##   return(.data)
## }
## 
## 
## rooms_visited_by_beacons <- function(.data){
##   roomsVisited <- data.frame(list("beacon"=as.character(), "roomsVisited"=as.character()))
##   for(i in 2:length(colnames(.data$data))){
##     roomCol <- colnames(.data$data)[[i]]
##     tryCatch({
##       beaconsInRoom <- unique(unlist(strsplit(unique(.data$data[[roomCol]]), ",")))
##       #cat("Room: ", roomCol, " ")
##       #cat(beaconsInRoom)
##       #cat("\n")
##       # if(beacon == "*"){
##       #   if(beacon %in% beaconsInRoom){
##       #     roomsVisited <- c(roomCol, roomsVisited)
##       #   }
##       # }
## 
##       roomsVisited <- rbind(roomsVisited, data.frame(list("beacon"=beaconsInRoom, "roomsVisited"=roomCol)))
## 
##     }, error = function(error_condition) {
##       cat("Skipping column: ", roomCol, "\n")
##     })
##   }
## 
##   roomsVisited <- roomsVisited %>%
##     group_by(beacon) %>%
##     summarise(roomsVisited=paste(unique(roomsVisited),collapse=","))
## 
##   .data$roomsVisited <- roomsVisited
##   return(.data)
## }
## 
## 
## calculate_beacon_hops <- function(.data, beacon){
##   data <- .data$data
##   data$idx <- apply( .data$data, 1, function(row) which( grepl(beacon, row) ) )
##   data$room <- colnames(data)[unlist(data$idx)]
##   data$beacon <- beacon
## 
##   str(data)
## 
##   return(data[,c("time", "room", "beacon", "groupid")])
## }
## 
## 
## convertToTransitions <- function(.data, beacon, groupid=NA){
##   data <- .data$data
##   if(!is.na(groupid)){
##     data <- get_group_data(.data, groupid)
##   }
##   groups <- .data$groups
##   fromRoom <- .data$fromRoom
##   toRoom <- .data$toRoom
##   res <- NULL
##   for(i in 1:nrow(groups)){
##     cat("\nTrip ",groups$groupid[i], ":")
##     instance <- data[time >= groups[i]$fromTime & time <= groups[i]$toTime]
##     toId <- function(beacon, row){
##       id <- which(grepl(beacon, row))
##       if(length(id) <= 0){
##         id <- NA
##       }
##       return(id)
##     }
##     idx <- apply( instance, 1, function(row) toId(beacon, row))
##     idx.dt <- as.data.table(list("beacon"=beacon,"room"=colnames(instance)[as.numeric(idx)]))
##     idx.dt[is.na(room), room := 0]
##     cat(instance$time)
##     idx.dt[, startTime:=instance$time]
##     if(is.null(res)){
##       res <- idx.dt
##     }
##     else if(nrow(idx.dt) > 0){
## 
##       res <- rbind(res, idx.dt)
##     }
##   }
##   return(res)
## 
## }
## 
## 

