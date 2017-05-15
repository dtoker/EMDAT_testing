# computes path_length, the saccade distance between two sucessive coordinates
find_path_length_vector <- function(x_cord_vector, y_cord_vector){
  
  path_length_vector <- c()
  
  for(i in 1:(length(x_cord_vector)-1)){
    
    path_length_vector[i] <- 
      sqrt((x_cord_vector[i+1] - x_cord_vector[i])^2 + (y_cord_vector[i+1] - y_cord_vector[i])^2)
  }
  return(path_length_vector)
}

# Checks whether the value computed from the internal data actually matches EMDAT output value
# First argument: expected value. Second: actual value.  
verify_equivalence <- function(internal_value, output_value, participant, a_scene, error_name){ 
  
  error_specification <- paste("Error: ", error_name, " does not match for participant:")
  try(
    if(internal_value  != output_value)
      stop(paste(error_specification, participant, " and scene: ", a_scene))
  )
}

# computes and returns the abs angles of sucessive fixation points in vector format  
find_abs_angle_vector<- function(x_cord_vector, y_cord_vector){
  
  abs_angle_vector <- c()
  
  for(i in 1:(length(x_cord_vector)-1)){
    
    delta_x <- x_cord_vector[i+1] - x_cord_vector[i]
    delta_y <- y_cord_vector[i+1] - y_cord_vector[i]
    
    if(delta_x == 0 & delta_y == 0){ # no movement case
      abs_angle_vector[i] <- 0
    } 
    else if(delta_x == 0){ # vertical move cases
      abs_angle_vector[i] <- pi/2
    } 
    else if(delta_y == 0){ # horizontal move cases
      abs_angle_vector[i] <-  0
    } 
    else if(delta_x > 0 & delta_y > 0) { # first quadrant case
      abs_angle_vector[i] <- atan(delta_y/delta_x)
    } 
    else if (delta_x < 0 & delta_y > 0) { # second quadrant case
      abs_angle_vector[i] <- pi+atan(delta_y/delta_x)
    } 
    else if (delta_x < 0 & delta_y < 0) { # third quadrant case
      abs_angle_vector[i] <- pi-atan(delta_y/delta_x)
    } 
    else { # fourth quadrant case (delta_x > 0 & delta_y < 0)
      abs_angle_vector[i] <- -atan(delta_y/delta_x)
    } 
  }
  return(abs_angle_vector)
}

# computes and returns the relative angles of sucessive saccade paths in vector format
find_rel_angle_vector<- function(x_cord_vector, y_cord_vector){
  
  rel_angle_vector <- c()
  last_vector <- c()
  next_vector <- c()
  
  for(i in 1:(length(x_cord_vector)-2)){
    
    last_vector[1] <- x_cord_vector[i] - x_cord_vector[i+1]
    last_vector[2] <- y_cord_vector[i] - y_cord_vector[i+1]
    next_vector[1] <- x_cord_vector[i+2] - x_cord_vector[i+1]
    next_vector[2] <- y_cord_vector[i+2] - y_cord_vector[i+1]
    normalized_last_vec <- normalize_vector(last_vector)
    normalized_new_vec <- normalize_vector(next_vector)
    rel_angle_vector[i]<- acos((normalized_last_vec%*%normalized_new_vec)[1,])
  }
  return(rel_angle_vector)
}

# helper function called in find_rel_angle_vector for use of arccosine  
normalize_vector <- function(vector){
  return(vector/ sqrt((vector%*%vector)[1,]))
}

# computes the length as defined in EMDAT for a scene from its segment lengths  
compute_scene_length <- function(segment_names,internal_data_vector){ 
  
  internal_value <- 0

  for(i in 1:length(segment_names)){
  
    time_stamp_vector <- subset(internal_data_vector[[i]], select=timestamp)$timestamp 
    internal_value <- internal_value + (tail(time_stamp_vector, 1) - head(time_stamp_vector, 1))
  }
  return(internal_value)
}

# Computes double and left clicks as defined in EMDAT code. Also, records the respectvie first clicks.
# Return has the format of vector.   
find_double_and_left_clicks <- function(internal_data.df){

  clicks.df <- subset(internal_data.df, event == "LeftMouseClick")
  clicks <- c() # c[1]: double click count, c[2]: left click count,
  clicks[3] <- -1    # c[3]: time of first double click, with default being -1
  clicks[4] <- -1    # c[4]: time of first left click, with default being -1

  if(nrow(clicks.df) == 0){
    clicks[1] <- 0
    clicks[2] <- 0
    return(clicks)

  } else if(nrow(clicks.df) == 1){
    clicks[1] <- 0
    clicks[2] <- 1
    clicks[4] <- subset(clicks.df, select=timestamp)[1,]
    return(clicks)

  } else{
    time_stamps <- subset(clicks.df, select=timestamp)$timestamp
    x_coords <- as.numeric(as.character(subset(clicks.df, select=x_coord)$x_coord))
    y_coords <- as.numeric(as.character(subset(clicks.df, select=y_coord)$y_coord))
    
    # "D for double and "L" for left. Keep track of double and left clicks
    #  Used for determining the first double and left
    #  first element is always not double
    double_left_marker <- c()
    double_left_marker[1] <- "L" 
    
    is_double_click <- TRUE
    double_click_count <- 0
    left_click_count <- 1 # first left click is not counted in the loop
    
    for(i in 1:(length(x_coords)-1)) {

      if(is_double_click &
         (time_stamps[i+1] - time_stamps[i]) <= 700 &
         (x_coords[i+1] - x_coords[i]) <= 10 &
         (y_coords[i+1] - y_coords[i]) <= 10)
      {
        double_click_count <- double_click_count + 1
        left_click_count <- left_click_count - 1
        is_double_click <- FALSE
        double_left_marker[i+1] <- "D"
      }
      else{
        left_click_count <- left_click_count + 1
        is_double_click <- TRUE
        double_left_marker[i+1] <- "L"
      }
    }
    clicks[1] <- double_click_count
    clicks[2] <- left_click_count
    
    first_double <- TRUE
    first_left <- TRUE
    for(j in 1:(length(double_left_marker)-1)){
      
      if(double_left_marker[j+1]=="D" & first_double){
        clicks[3] <- time_stamps[j+1]
        first_double <- FALSE
      }
      if(double_left_marker[j]=="L" & double_left_marker[j+1]!="D" & first_left){
        clicks[4] <- time_stamps[j]
        first_left <- FALSE
      }
    }
    return(clicks)
  }
}

# Helper funciton for computing the numerator of aggregated sd
compute_segsd_with_weight <- function(feature_value_vector, scene_mean){
  
  numerator <- (length(feature_value_vector)-1)*(sd(feature_value_vector)^2)+
                length(feature_value_vector)*(mean(feature_value_vector)-scene_mean)^2
  
  return(numerator)
}

# Helper funciton for computing the numerator of scene mean
compute_segmean_with_weight <- function(feature_value_vector){
  
  numerator <- length(feature_value_vector)*mean(feature_value_vector)
  return(numerator)
} 

# Given an input vector, compute all of sum, mean, and rate (sum over scene_length)
# for pathdistance, abspathangles, and relpathangles. Also returns mean before rounnding 
# and result of applying the given vector function to the input, which are used for 
# subsequent sd computation. 
find_sum_mean_rate <- function(vector_input, vector_function, segs_length, scene_length){
  
  internal_sum <-0
  numerator <- 0
  denominator <- 0
  data_storage <- list()
  results <- list(sum = 0, temp_mean = 0, mean = 0, rate = 0, data_storage = 0)
  
  for(i in 1:segs_length){
    
    result_vector <- vector_function(vector_input[[i]]$mappedfixationpointx, 
                                     vector_input[[i]]$mappedfixationpointy)
    
    internal_sum <- internal_sum + sum(result_vector)
    numerator <- numerator + compute_segmean_with_weight(result_vector)
    denominator <- denominator+length(result_vector)
    data_storage[[i]] <- result_vector
  }
  
  results$rate <- signif(internal_sum / scene_length, digits = 12)
  results$sum <- signif(internal_sum, digits = 12)
  results$temp_mean <- numerator / denominator
  results$mean <- signif(results$temp_mean, digits = 12)
  results$data_storage <- data_storage
  
  return(results)
}

# computes sd for pathdistance, abspathangles, and relpathangles 
find_fixation_sd <- function(data_storage, scene_mean, segs_length){
  numerator <- 0
  denominator <- 0
  
  for(i in 1:segs_length){
    
    numerator <- numerator + compute_segsd_with_weight(data_storage[[i]], scene_mean)
    denominator <- denominator+length(data_storage[[i]])
  }
  internal_value <- signif(sqrt(numerator/(denominator-1)), digits = 12)
  return(internal_value)
}

