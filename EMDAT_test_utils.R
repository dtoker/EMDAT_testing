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




