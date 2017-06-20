# global variables for keeping track of the number of total executed tests and that of passed tests 
# for each participant
success_counter <- 0
total_counter <- 0

# computes path_length, the saccade distance between two sucessive coordinates
find_path_length_vector <- function(x_cord_vector, y_cord_vector){
  
  path_length_vector <- numeric(length(x_cord_vector)-1)
  
  for(i in 1:(length(x_cord_vector)-1)){
    
    path_length_vector[i] <- 
      sqrt((x_cord_vector[i+1] - x_cord_vector[i])^2 + (y_cord_vector[i+1] - y_cord_vector[i])^2)
  }
  return(path_length_vector)
}

# Checks whether the value computed from the internal data actually matches EMDAT output value
# ,to the given number of significant figures, which is currently set to eight  
# First argument: expected value. Second: actual value.  
verify_equivalence <- function(internal_value, output_value, participant, a_scene, error_name){

  total_counter <<- total_counter + 1

  error_specification <- paste("Error: ", error_name, " does not match for participant:")
  
  #### for debugging ####
  
  print(paste("--------- processing ", participant, " ", a_scene, " ", error_name, " --------", sep = ""))
  
  #######################
  
  try(
    if(signif(internal_value, digits = 8) != signif(output_value, digits = 8)){

      stop(paste(error_specification,
                 participant,
                 " and scene: ",
                 a_scene,
                 " internal_value: ",
                 formatC(internal_value, format="e", digits = 15),
                 " output_value: ",
                 formatC(output_value, format="e", digits = 15),
                 sep = ""))
    } else{

        success_counter <<- success_counter + 1
        
        # To pintout values for confirmation:
        # writeLines(paste(participant,
        #                  " and scene: ",
        #                  a_scene,
        #                  " ",
        #                  error_name,
        #                  " internal_value: ",
        #                  formatC(internal_value, format="e", digits = 15),
        #                  " output_value: ",
        #                  formatC(output_value, format="e", digits = 15),
        #                  sep = ""))
    }
  )
}

# notifies test results for each particpant by printing out  
report_success <- function(participant, cumulative_counter){
  
  writeLines(paste("######## Results for ", participant, " #########\nTotal number of tests: ", 
                   total_counter, sep = ""))
  writeLines(paste("Number of failed tests: ", total_counter - success_counter, sep = ""))
  
  if(success_counter == total_counter){
    
    writeLines(paste("All Tests Passed for P", participant, sep = ""))
  }
  writeLines("###################################")
  writeLines("")
  
  cumulative_counter <<- cumulative_counter + total_counter
  
  # clear the counters
  success_counter <<- 0
  total_counter <<- 0
}

# computes and returns the abs angles of sucessive fixation points in vector format  
find_abs_angle_vector<- function(x_cord_vector, y_cord_vector){
  
  abs_angle_vector <- numeric(length(x_cord_vector)-1)
  
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
  
  result_length <- length(x_cord_vector) - 2
  rel_angle_vector <- numeric(result_length)
  
  if(result_length > 0){
    
    last_vector <- numeric(2)
    next_vector <- numeric(2)
  
    for(i in 1:(length(x_cord_vector)-2)){
    
      last_vector[1] <- x_cord_vector[i] - x_cord_vector[i+1]
      last_vector[2] <- y_cord_vector[i] - y_cord_vector[i+1]
      next_vector[1] <- x_cord_vector[i+2] - x_cord_vector[i+1]
      next_vector[2] <- y_cord_vector[i+2] - y_cord_vector[i+1]
    
      if((last_vector[1]==0 & last_vector[2]==0) | (next_vector[1]==0 & next_vector[2]==0)){
      
        rel_angle_vector[i]<- 0
      } else{
      
          normalized_last_vec <- normalize_vector(last_vector)
          normalized_new_vec <- normalize_vector(next_vector)
          dot_product <- (normalized_last_vec%*%normalized_new_vec)[1,]
          if(dot_product > 1 ){
            dot_product = 1
          } 
          if(dot_product < -1){
            dot_product = -1
          }
          rel_angle_vector[i]<- acos(dot_product)
      }
    }
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
  clicks <- numeric(4) # clicks[1]: double click count, clicks[2]: left click count,
  clicks[3] <- -1      # clicks[3]: time of first double click, with default being -1
  clicks[4] <- -1      # clicks[4]: time of first left click, with default being -1

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
    double_left_marker <- character(length(x_coords)-1)
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
    # this last element can not be checked in the for-loop above
    if(double_left_marker[length(double_left_marker)]=="L" & first_left){
      clicks[4] <- time_stamps[length(double_left_marker)]
    } 
    return(clicks)
  }
}

# Helper funciton for computing the numerator of aggregated sd
compute_segsd_with_weight <- function(feature_value_vector, scene_mean){
  
  vector_length <- length(feature_value_vector)
  numerator <- 0
  
  if(vector_length > 0){
    stddev <- sd(feature_value_vector)
    if(is.na(stddev)){
      stddev <- 0
    } 
  
    numerator <- (vector_length-1)*(stddev^2) + vector_length*(mean(feature_value_vector)-scene_mean)^2
  }
  return(numerator)
}

# Helper funciton for computing the numerator of scene mean (weighted)
compute_segmean_with_weight <- function(feature_value_vector){
  
  numerator <- length(feature_value_vector)*mean(feature_value_vector)
  return(numerator)
} 

# Given an input vector, compute all of sum, mean, and rate (sum over scene_length)
# for pathdistance, abspathangles, and relpathangles. 
find_sum_mean_rate <- function(vector_input, vector_function, segs_length, scene_length){
  
  internal_sum <-0
  numerator <- 0
  denominator <- 0
  
  # this variable stores computed vectors to avoid recomputation  
  data_storage <- list()
  
  results <- list(sum = 0, mean = 0, rate = 0, data_storage = 0)
  
  for(i in 1:segs_length){
    
    result_vector <- vector_function(vector_input[[i]]$mappedfixationpointx, 
                                     vector_input[[i]]$mappedfixationpointy)
    
    internal_sum <- internal_sum + sum(result_vector)
    numerator <- numerator + compute_segmean_with_weight(result_vector)
    denominator <- denominator+length(result_vector)
    data_storage[[i]] <- result_vector
  }
  
  results$rate <- internal_sum / scene_length
  results$sum <- internal_sum
  
  if(denominator != 0){
    results$mean <- numerator / denominator
  }
  
  results$data_storage <- data_storage
  
  return(results)
}

# computes sd for pathdistance, abspathangles, and relpathangles 
find_fixation_sd <- function(data_storage, scene_mean, segs_length){
  numerator <- 0
  denominator <- 0
  internal_value <- 0
  
  for(i in 1:segs_length){
    
    numerator <- numerator + compute_segsd_with_weight(data_storage[[i]], scene_mean)
    denominator <- denominator+length(data_storage[[i]])
  }
  if(denominator > 1){
    internal_value <- sqrt(numerator/(denominator-1))
  }
  return(internal_value)
}

# computes the mean for saccadedistance, saccadeduration, and saccadespeed
# the argument coloumn takes a column name in String 
find_saccade_mean <- function(input_vector, coloumn, segs_length){
  
  numerator <- 0
  denominator <- 0
  mean <- 0
  
  for(i in 1:segs_length){
    
    data <- subset(
      input_vector[[i]], select=coloumn)[,1]
    numerator <- numerator + compute_segmean_with_weight(data)
    denominator <- denominator+length(data)
  }
  if(denominator != 0){
    mean <- numerator/denominator
  }
  return(mean)
}

# computes the sd for saccadedistance, saccadeduration, and saccadespeed
# the argument coloumn takes a column name in String 
find_saccade_sd <- function(input_vector, coloumn, segs_length, scene_mean){
  
  numerator <- 0
  denominator <- 0
  internal_value <- 0
  
  for(i in 1:segs_length){
    
    data <- subset(
      input_vector[[i]], select=coloumn)[,1]
    
    numerator <- numerator + compute_segsd_with_weight(data, scene_mean)
    denominator <- denominator+length(data)
  }
  
  if(denominator > 1) {
    
    internal_value <- sqrt(numerator/(denominator-1))
  }
  return(internal_value)
}

# computes the mean for headdistance, pupilsize, and pupilvelocity
# coloumn, criterion_name, criterion_condition, and criterion_value in String
find_gaze_mean <- function(input_vector, coloumn, criterion_name, criterion_condition, criterion_value, 
                           segs_length){
  
  numerator <- 0
  denominator <- 0
  
  for(i in 1:segs_length){
    # input_vector[[i]][input_vector[[i]][[criterion_name]]==criterion_value,][[coloumn]]    
    #   == input_vector[[i]][input_vector[[i]]$criterion_name==criterion_value,]$coloumn
    #   == subset(input_vector[[i]], criterion_name==criterion_value)$coloumn
    # Note: in the above "equalities", actually needs to strip away the quotation marks from 
    #       the Strings criterion_name and coloumn, when preceded by '$' or used in subset 
    switch(criterion_condition, 
           eql = (valid_data <- input_vector[[i]][input_vector[[i]][[criterion_name]]==criterion_value,][[coloumn]]),
           not_eql = (valid_data <- input_vector[[i]][input_vector[[i]][[criterion_name]]!=criterion_value,][[coloumn]])
    )  
    numerator <- numerator + compute_segmean_with_weight(valid_data)
    denominator <- denominator+length(valid_data)
  }
  
  if(denominator != 0){
    mean <- numerator / denominator
  } else {
    mean <- -1
  }
  
  return(mean)
}

# computes the sd for headdistance, pupilsize, and pupilvelocity
# coloumn, criterion_name, criterion_condition, and criterion_value in String
find_gaze_sd <- function(input_vector, coloumn, criterion_name, criterion_condition,
                         criterion_value, segs_length, scene_mean){
  
  numerator <- 0
  denominator <- 0
  internal_value <- 0
  
  for(i in 1:segs_length){
    # input_vector[[i]][input_vector[[i]][[criterion_name]]==criterion_value,][[coloumn]]    
    #   == input_vector[[i]][input_vector[[i]]$criterion_name==criterion_value,]$coloumn
    #   == subset(input_vector[[i]], criterion_name==criterion_value)$coloumn
    # Note: in the above "equalities", actually needs to strip away the quotation marks from 
    #       the Strings criterion_name and coloumn, when preceded by '$' or used in subset
    switch(criterion_condition, 
           eql = (valid_data <- input_vector[[i]][input_vector[[i]][[criterion_name]]==criterion_value,][[coloumn]]),
           not_eql = (valid_data <- input_vector[[i]][input_vector[[i]][[criterion_name]]!=criterion_value,][[coloumn]])
    )  
    numerator <- numerator + compute_segsd_with_weight(valid_data, scene_mean)
    denominator <- denominator+length(valid_data)
  }
  
  # no valid data case 
  if(denominator == 0) {
    internal_value <- -1
  }
  
  if(denominator > 1){
    
    internal_value <- sqrt(numerator/(denominator-1))
  }
  
  return(internal_value)
}

# Given all particapnts data, returns only the scenes belonging to the given participant
# This idea of row retrieval is from 
# http://stackoverflow.com/questions/5553802/get-row-number-for-r-data-frame   
get_features_df_for_participant <- function(emdat_export_all.df, participant, Sc_ids, last_participant){
  
  start_row <- which(Sc_ids==paste(participant, "_allsc", sep = "")) + 1
  
  if(participant != last_participant){
    
    number_char <- nchar(participant)
    last_char <- substr(participant, number_char, number_char)
    
    if(last_char == "a"){
      
      end_row <- which(Sc_ids==paste(substr(participant, 1, number_char-1), "b_allsc", sep = "")) - 1
    } else {
      
      numerical_part <- as.numeric(substr(participant, 1, number_char - 1))
      
      # P143 missing; needs to skip 
      if(numerical_part != 142){
        numerical_part <- numerical_part + 1
      } else{
        numerical_part <- numerical_part + 2
      }
      
      end_row <- which(Sc_ids==paste(numerical_part, "a_allsc", sep = "")) - 1
    }
  } else{
    
    end_row <- length(Sc_ids)
  }
  
  return(emdat_export_all.df[start_row : end_row, ])
}

# generate a list of participants in the givne range in string    
generate_participant_list <- function(p_range){
  
  participants <- list()
  last_index = 0
  
  for(i in p_range){
    
    participant_a = paste(as.character(i), "a", sep = "")
    participant_b = paste(as.character(i), "b", sep = "")
    
    last_index = last_index + 1
    participants[last_index] = participant_a
    
    last_index = last_index + 1
    participants[last_index] = participant_b
  }
  
  return(participants)
} 


# converts an element, specified by numeral 1 or 2, of a given string tuple 
# of the form "number1, number2" into the corresponding number 
get_tuple_element <- function(tuple, tuples) {
  
  tuples <- as.character(tuples)
  comma_index <- gregexpr(pattern = paste(",", sep = ""), tuples)[[1]][1]
  
  if(tuple == 1){
    return(as.numeric(substr(tuples, 1, comma_index - 1)))
  } 
  else{
    return(as.numeric(substr(tuples, comma_index + 1, nchar(tuples))))
  }
}

get_seg_start_and_end_times <- function(seg) {
  
  start_and_end <- list(start = 0, end = 0)
  
  timestamps <- seg$timestamp
  start_and_end$start <- timestamps[1]
  start_and_end$end <- timestamps[length(timestamps)]
  
  return(start_and_end)
}

# tells which elements of fix_data_set is inside the rectangle specified by the
# values of the reamining arguments 
is_inside <-  function(fix_data_set, x_left, x_right, y_bottom, y_top) {
  
  fix_data_set$mappedfixationpointx > x_left &
  fix_data_set$mappedfixationpointx <= x_right &
  fix_data_set$mappedfixationpointy <= y_bottom &   
  fix_data_set$mappedfixationpointy > y_top 
}

# computes the number of incoming saccades from inside aoi2 to those points inside aoi1
trans_from <- function(fix_data_set, aoi1, aoi2){
  
  inside_indices <- which(is_inside(fix_data_set, aoi1$x_left, aoi1$x_right, aoi1$y_bottom, aoi1$y_top))
  
  count_per_aoi <- 0
  
  # disregard first element in inside_indices
  for(i in setdiff(inside_indices, 1)){
    
    if(is_inside(fix_data_set[i-1,], aoi2$x_left, aoi2$x_right, aoi2$y_bottom, aoi2$y_top)){
      
      count_per_aoi <- count_per_aoi + 1
    }
  }
  return(count_per_aoi) 
}

# extracts the boundaries of a rectangular aoi with the given name
# from df of the form col.names = c("aoi_name","TL","TR","BR", "BL")
extract_aoi_coordinate <- function(aoi_file.df, aoi_name){
  
  aoi <- aoi_file.df[aoi_file.df$aoi_name == aoi_name,]
  aoi_element <- list(aoi_name = aoi_name, left = 0, right = 0, bottom = 0, top = 0)
  aoi_element$left <- get_tuple_element(1, aoi$TL)
  aoi_element$right <- get_tuple_element(1, aoi$TR)
  aoi_element$bottom <- get_tuple_element(2, aoi$BR)
  aoi_element$top <-  get_tuple_element(2, aoi$TR)
  
  return(aoi_element) 
}

# Given all particapnts data, returns only the scenes belonging to the given participant
# Used for three participants study
# This idea of row retrieval is from 
# http://stackoverflow.com/questions/5553802/get-row-number-for-r-data-frame   
get_features_df_for_participant_for_3 <- function(emdat_export_all.df, participant, Sc_ids, last_participant){
  
  start_row <- which(Sc_ids==paste(participant, "_allsc", sep = "")) + 1
  
  if(participant != last_participant){
    
    participant <- as.numeric(participant) + 1
    participant <- as.character(participant)
    end_row <- which(Sc_ids==paste(participant, "_allsc", sep = "")) - 1
  } else{
    
    end_row <- length(Sc_ids)
  }
  return(emdat_export_all.df[start_row : end_row, ])
}

set_root_name <- function(root){
  
  return(paste(root, "_", sep = ""))
}

