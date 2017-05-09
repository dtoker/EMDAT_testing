# path_length is the saccade distance between two sucessive coordinates
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

# Computes the length as defined in EMDAT for a scene from its segment lengths  
compute_scene_length <- function(segment_names,internal_data_vector){ 
  
  internal_value <- 0

  for(i in 1:length(segment_names)){
  
    time_stamp_vector <- subset(internal_data_vector[[i]], select=timestamp)$timestamp 
    internal_value <- internal_value + (tail(time_stamp_vector, 1) - head(time_stamp_vector, 1))
  }
  return(internal_value)
}

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
    
    is_double_click <- TRUE
    double_click_count <- 0
    left_click_count <- 1 # first left click is not counted in the loop
    double_click_index <- 1000000
    left_click_index <- 1000000
    
    for(i in 1:(length(x_coords)-1)) {
      
      if(is_double_click & 
         (time_stamps[i+1] - time_stamps[i]) <= 700 & 
         (x_coords[i+1] - x_coords[i]) <= 10 &
         (y_coords[i+1] - y_coords[i]) <= 10
      ){
        double_click_count <- double_click_count + 1
        left_click_count <- left_click_count - 1
        is_double_click <- FALSE
        if(i < double_click_index){
          clicks[3] <- time_stamps[i+1]
          double_click_index <- i
        }
      } else{
        left_click_count <- left_click_count + 1
        is_double_click <- TRUE
        if(i < left_click_index){
          clicks[4] <- time_stamps[i]
          left_click_index <- i
        }
      }
    } 
    clicks[1] <- double_click_count
    clicks[2] <- left_click_count
    return(clicks)
  }
}

# find_double_and_left_click_rates <- function(internal_data.df, length){
#   
#   click_rate.df <- subset(internal_data.df, event == "LeftMouseClick")
#   click_rates <- c() # c[1]: double click rate, c[2]: left click rate
#   
#   if(nrow(click_rate.df) == 0){
#     click_rates[1] <- 0
#     click_rates[2] <- 0
#     return(click_rates)
#     
#   } else if(nrow(click_rate.df) == 1){
#     click_rates[1] <- 0
#     click_rates[2] <- 1
#     return(click_rates)
#     
#   } else{
#     time_stamps <- subset(click_rate.df, select=timestamp)$timestamp
#     x_coords <- as.numeric(as.character(subset(click_rate.df, select=x_coord)$x_coord))
#     y_coords <- as.numeric(as.character(subset(click_rate.df, select=y_coord)$y_coord))
#     
#     is_double_click <- TRUE
#     double_click_count <- 0
#     left_click_count <- 1 # first left click is not counted in the loop
#     
#     for(i in 1:(length(x_coords)-1)) {
#       
#       if(is_double_click & 
#          (time_stamps[i+1] - time_stamps[i]) <= 700 & 
#          (x_coords[i+1] - x_coords[i]) <= 10 &
#          (y_coords[i+1] - y_coords[i]) <= 10
#       ){
#         double_click_count <- double_click_count + 1
#         left_click_count <- left_click_count - 1
#         is_double_click <- FALSE
#       } else{
#         left_click_count <- left_click_count + 1
#         is_double_click <- TRUE
#       }
#     } 
#     click_rates[1] <- double_click_count/length
#     click_rates[2] <- left_click_count/length
#     return(click_rates)
#   }
# }


