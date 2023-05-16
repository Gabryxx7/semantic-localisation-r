## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(lubridate)
library(ambient) # noise
library(ggridges)
library(ggplot2)
library(data.table)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
generate_regular_time_series <- function(startTime=NULL, endTime=NULL, interval="00:00:10", n=100){
  if(is.null(startTime)) startTime <- Sys.time()
  time_interval <- hms::parse_hms(interval)
  seconds_interval <- (hour(time_interval) *60*60) + (minute(time_interval) * 60) + second(time_interval)
  if(is.null(endTime)) endTime <- startTime+(seconds_interval*(n-1))
  time_series_dt <- data.table(time=seq(from=startTime, to=endTime, by=seconds_interval))
  time_series_dt <- time_series_dt[, timeSlotID := .I][, c("timeSlotID", "time")]
  return(time_series_dt)
}

generate_noise_matrix <- function(nrows=100, ncols=2, frequency=0.01, octaves=3, lacunarity=2, gain=0.5, thresholds=NULL, fractal="fbm"){
  library(ambient)
  noise <- noise_perlin(c(nrows, ncols), frequency=frequency, octaves=octaves, lacunarity=lacunarity, gain=gain, fractal=fractal,pertubation="fractal")
  noise <- apply(noise, 2, function(x) abs(x* 1000))
  if(is.null(thresholds)){
  }
  # noise <- apply(noise, 2, function(x) x > threshold)
  noise <- data.table(noise)
  colnames(noise) <- paste0("r",seq(1,ncols))
  return(noise)
}

apply_thresholds <- function(noise, bins){
  thresholds_binning <- function(val, thresholds){
    n <- length(thresholds)
    prevT <- thresholds[1]
    for(i in 2:n){
      t <- thresholds[i]
      if(val >= prevT && val < t){
        val <- i-1
      }
      prevT <- t
    }
    return(val)
  }
  thresholds <- c()
  q <- 0
  bins <- bins
  for(i in 1:(bins-1)){
    thresholds <- c(thresholds, quantile(noise, (1/bins)*i)[[1]])
  }
  thresholds <- c(-Inf, thresholds, Inf)
  print(thresholds)
  # thresholds <- c(thresholds, Inf)
  return(unlist(lapply(noise, thresholds_binning, thresholds)))
}

plot_noise <- function(noiseDT, col=1){
  colName <- colnames(noiseDT)[[col]]
  return(ggplot(noiseDT, aes_string(x=colName)) + geom_density())
}

plot_noise_matrix <- function(noiseDT){
  rows <- nrow(noiseDT)
  cols <- ncol(noiseDT)
  noiseDT <- melt(noiseDT, variable.name="room", value.name="value")
  plot <- ggplot(noiseDT, aes(x=value, y=room, fill=room))
  for(c in 1:cols){
    plot <- plot + geom_density_ridges(alpha=0.6)
  }
  plot <- plot +
    theme_ridges() + 
    theme(legend.position = "none")
  return(plot)
}


generate_repeated_bools <- function(n=100, rep_ratio=0.4){
    # Set the total number of elements in the list
  total_elements <- n
  # Set the ratio of repeated elements
  repeated_ratio <- rep_ratio
  # Calculate the number of repeated elements
  num_repeated <- floor(total_elements * repeated_ratio)
  
  # Generate the repeated elements
  repeated_bools <- rep(TRUE, num_repeated)
  
  # Generate the non-repeated elements
  non_repeated_bools <- map(1:(total_elements - num_repeated), ~{
    bool <- sample(c(TRUE, FALSE), 1)
    while (bool %in% repeated_bools) {
      bool <- sample(c(TRUE, FALSE), 1)
    }
    bool
  })
  
  # Combine the repeated and non-repeated elements
  bool_list <- c(repeated_bools, non_repeated_bools)
  
  # Shuffle the list to ensure randomness
  bool_list <- sample(bool_list)
  
  # Print the final list
  return(unlist(bool_list))
}


combine_vector_columns <- function(colA, colB){
  combine_agents <- function(x, y){
    return(append(x, y))
  }
  return(Map(combine_agents, colA, colB))
}

generate_trace <- function(nrooms=5, nagents=10, ntimes=100, interval="00:00:10", concurrency=FALSE, distribution="normal", threshold_step=0.2, start_threshold=NULL, test_sort=FALSE, reps_ratio=0.3){
  cat("\nGenerating Trace with", nagents, "agents,", nrooms, "rooms and ", ntimes, "timeslots\n")
  update_available_slots <- function(is_slot_available, should_place){
    if(!should_place) return(is_slot_available)  # If we are not placing the agent here there's nothing to update
    if(!is_slot_available) return(FALSE) # if we are placing the agent but the slot is unavailable, it should keep being unavailable
    # if we are placing the agent here AND the slot is available, it should now become unavailable
    return(FALSE)
  }
  check_concurrency <- function(is_slot_available, should_place){
    if(!should_place) return(FALSE)
    # If we want to place it in this slot but the slot is unavailable we should return FALSE
    if(!is_slot_available) return(FALSE)
    # if we want to place it here and the slot is available we should return TRUE
    return(TRUE)
  }
  
  middle_sort <- function(dist){
      dist <- sort(dist)
      r_len <- length(dist)
      r_half <- ceiling(r_len/2)
      dist <- append(dist[1:r_half], sort(dist[(r_half+1):r_len], decreasing=TRUE))
      return(dist)
  }
  
  threshold <- start_threshold
  if(distribution=="normal"){
    mean <- 100
    sd <- 50
    start_quantile <- 0.85
    # generate a random distribution with the given parameters and get a 0.95 percentile
    if(is.null(threshold)){
      threshold <- quantile(rnorm(ntimes, mean = mean, sd = sd), start_quantile)
    }
  }
  else if(distribution == "uniform"){
    min <- 0
    max <- 100
    if(is.null(threshold)){
      threshold <- (min+max)/2
    }
  }
  else if(distribution == "chi"){
    df <- ntimes*0.3
    ncp <- ntimes*0.5
    if(is.null(threshold)){
      threshold <- ntimes*0.8
    }
  }
  else if(distribution == "perlin"){
    if(is.null(threshold)){
      threshold <- 250
    }
  }
  else {
    s_size <- ntimes*(1-reps_ratio)
    if(is.null(threshold)){
      threshold <- s_size*0.7
      # threshold <- ntimes*0.5
    }
  }
  ts <- generate_regular_time_series(interval=interval, n=ntimes)
  room_cols <- list()
  agents_list <- list()
  for(r in 1:nrooms){
    rCol <- paste0("r",r)
    room_cols <- append(room_cols, rCol)
    for(a in 1:nagents){
      aName <- paste0("a", a)
      if(!(aName %in% names(agents_list))){
        agents_list[[aName]] <- rep(TRUE, ntimes) # All slots are initially available
      }
      if(distribution == "normal"){
        random_slots <- rnorm(ntimes, mean = mean, sd = sd)
        random_slots <- random_slots > threshold
      }
      else if(distribution == "uniform"){
        random_slots <- runif(n=ntimes, min=min, max=max)
        random_slots <- random_slots > threshold
        if(test_sort){
          random_slots <- middle_sort(random_slots)
        }
      }
      else if(distribution == "chi"){
        random_slots <- rchisq(ntimes, df=df, ncp=ncp)
        random_slots <- random_slots > threshold
        if(test_sort){
          random_slots <- middle_sort(random_slots)
        }
      }
      else if(distribution == "perlin"){
        noise_freq <- runif(1) * 0.3
        oct <- as.integer(runif(1) * 4)
        lac <- runif(1) * 0.4
        gain <- runif(1) * 0.6
        random_slots <- generate_noise_matrix(nrows=ntimes, frequency = noise_freq, octaves = oct, lacunarity = lac, gain=gain, fractal="rigid-multi")[[1]]
        random_slots <- random_slots > threshold
      }
      else{
        # random_slots <- sample(c(1:s_size, sample(1:s_size, ntimes-s_size, replace = TRUE)))
        random_slots <- generate_repeated_bools(ntimes, reps_ratio)
      }
    
      random_slots_cleaned <- random_slots
      if(!concurrency){
        random_slots_cleaned <- Map(check_concurrency, agents_list[[aName]], random_slots)
        agents_list[[aName]] <- Map(update_available_slots, agents_list[[aName]], random_slots)
      }
      aData <- ifelse(random_slots_cleaned, aName, NA)
      if(rCol %in% colnames(ts)){
        combined <- combine_vector_columns(ts[[rCol]], aData)
        # cat("\nts: ", nrow(ts), "\taData: ", length(aData), "\tCombined: ", length(combined), "\n")
        ts[[rCol]] <- combined
      }
      else{
        # cat("\nts: ", nrow(ts), "\taData: ", length(aData), "\taData ", length(aData), "\n")
        ts[[rCol]] <- aData
      }
    }
    ts[[rCol]] <- lapply(ts[[rCol]], function(x) x[!is.na(x)])
    threshold <- threshold*(1- threshold_step)
    if(r == nrooms-1){
      threshold <- 0 # just fill the last room anyway
    }
  }
  return(ts)
}


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## # Sorting test
## randomNorm <- sort(rnorm(100, 100, 50))
## 


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## randNormDist <- generate_trace(distribution = "normal", nrooms=3, start_threshold = 125, threshold_step = 0.15)
## View(randNormDist)
## invisible(get_agent_journey(randNormDist, "a1"))


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## randUniDist <- generate_trace(distribution = "uniform", threshold_step = 0.4, start_threshold = 70, nrooms=3)
## View(randUniDist)
## invisible(get_agent_journey(randUniDist, "a1"))


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## randChiDist <- generate_trace(distribution = "chi", start_threshold = 85, threshold_step = 0.1, nrooms=3)
## View(randChiDist)
## invisible(get_agent_journey(randChiDist, "a1"))


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## randSampleDist <- generate_trace(distribution = "sample",reps_ratio = 0.35, nrooms=3)
## View(randSampleDist)
## invisible(get_agent_journey(randSampleDist, "a1"))


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## # Load required libraries
## library(dplyr)
## library(lubridate)
## 
## # Set seed for reproducibility
## set.seed(123)
## 
## # Define the number of rows and columns
## num_rows <- 100
## num_cols <- 3
## 
## # Generate a dataframe with a date column
## df <- data.frame(date = seq(as.Date("2022-01-01"), by = "day", length.out = num_rows))
## agents_list <- c("a1", "a2", "a3", "a4", "a5")
## n <- length(agents_list)
## 
## # Generate random data for the additional columns
## for (i in 1:num_cols) {
##   col_name <- paste0("r", i)
##   df[[col_name]] <- replicate(num_rows, {
##     vec <- sample(agents_list, sample(1:n, 1), replace = FALSE)
##     # cat("Start while")
##     while (any(sapply(df[[col_name]], function(x) any(vec %in% unlist(x[-1]))))) {
##       vec <- sample(agents_list, sample(1:n, 1), replace = FALSE)
##     }
##     # cat("-End while, ")
##     vec
##   })
## }
## 
## 
## # View the first few rows of the dataframe
## head(df)
## 
## df <- data.table(df)
## df$timeSlotID <- seq(1:nrow(df))
## # colnames(df) <- c("date", "r1" ,      "r2"   ,    "r3" ,      "timeSlotID")
## invisible(get_agent_journey(df, "a1"))


## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------
## randTrace <- generate_trace(distribution = "perlin", start_threshold=800, ntimes=1000)
## View(randTrace)
## invisible(get_agent_journey(randTrace, "a3"))

