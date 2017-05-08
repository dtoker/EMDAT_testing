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