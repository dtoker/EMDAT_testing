source("EMDAT_testUtils.R")

### set up the tests by initializing varibales in the global scope ###
root <- "Part2_EMDATInternal_EMDATOutput/three_parts_study_data/"
internal_data_path <- paste(root, "EMDAT_internal_data/", sep = "")
seg_file_path <- paste(root, "seg_files/", sep = "")
feature_files_path <- paste(root, "features/", sep = "")
aoi_file_path <- "Part1_TobiiV3Output_EMDATInternal/three_parts_study_data/"

emdat_export_all.df <- read.csv(paste(feature_files_path, 
                                      "tobiiv3_sample_features_single",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")

Sc_ids <- as.character(emdat_export_all.df[,1])

cumulative_counter <- 0

### test scripts ###
readfiles_aoi <- function(participant, seg_file, aoi_file, last_participant){
  
  # reads the pertinent part of the features file for the given participant (*)
  emdat_export.df <- get_features_df_for_participant_for_3(emdat_export_all.df, participant, Sc_ids, last_participant)
  
  # reads aoi file and extracts aoi boundaries 
  aoi_file.df <- read.csv(aoi_file, sep="\t", header = FALSE, col.names = c("aoi_name","TL","TR","BR", "BL"))
  aoi <- extract_aoi_coordinate(aoi_file.df, "single")
  
  # extracts scene names
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  scene.names <- unique(seg_file.df[,"scene"])
  
  # reads in the internal EMDAT data files necessary for computing expecetd values, 
  # once for the given participant 
  fixation_data.df <- read.csv(paste(internal_data_path,"EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
  events_data.df <- read.csv(paste(internal_data_path, "EMDATdata_eve_P", participant, ".tsv", sep=""), sep="\t")
  gazesample_data.df <- read.csv(paste(internal_data_path, "EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  
  # replaces None vlaue in x_ and y_coords with NA to elegantly handle inequality comparisons later
  # in check_aoi_eve
  events_data.df$x_coord <- replace(events_data.df$x_coord, which(events_data.df$x_coord == 'None'), NA)
  events_data.df$y_coord <- replace(events_data.df$y_coord, which(events_data.df$y_coord == 'None'), NA)
  
  # loops over the scenes
  for (a_scene in scene.names) {
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    # reads the pertinent part of the file from (*) above for the given scene 
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    # if-statement below guards against missing scenes in the files   
    if(nrow(emdat_export.df.scene) != 0) {
      
      # keeps all segments belonging to the scene in data frame format
      gazesample_data_scene.df <- subset(gazesample_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
      fixation_data_scene.df <- subset(fixation_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
      events_data_scene.df <- subset(events_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
      
      if(nrow(fixation_data_scene.df) != 0 &
         nrow(gazesample_data_scene.df) != 0){
        
        check_aoi_fix(emdat_export.df.scene,
                      participant,
                      a_scene,
                      segment.names,
                      aoi,
                      gazesample_data_scene.df,
                      fixation_data_scene.df)
      }
      
      if(nrow(gazesample_data_scene.df) != 0){
        
        check_aoi_eve(emdat_export.df.scene,
                      participant,
                      a_scene,
                      segment.names,
                      aoi,
                      events_data_scene.df,
                      gazesample_data_scene.df)
      }
    }
  }
  report_success(participant, cumulative_counter)
}

# This function checks the correctness of fixations
# LIST OF COLUMS TO TEST:

# single_numfixations
# single_proportionnum
# single_fixationrate
# single_totaltimespent
# single_proportiontime
# single_meanfixationduration
# single_stddevfixationduration (Update the definition if the output file is regenerated)    
# single_longestfixation
# single_timetofirstfixation
# single_timetolastfixation
# single_numtransfrom_single
# single_proptransfrom_single

check_aoi_fix <- function(emdat_output.df, 
                          participant, 
                          a_scene, 
                          segment.names,
                          aoi,
                          gazesample_data_scene.df,
                          fixation_data_scene.df){
  
  ### set up the tests ###
  internal_data.df <- subset(fixation_data_scene.df,
                             is_inside(fixation_data_scene.df,aoi$left, aoi$right, aoi$bottom, aoi$top))
  
  # stores the data by segment into vectors
  internal_data_vector <- c()
  fixation_data_vector <- c()
  gazesample_data_vector <- c()
  segs_length <- length(segment.names)
  for(i in 1:segs_length) {

    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    fixation_data_vector[[i]] <- subset(fixation_data_scene.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data_scene.df, grepl(segment.names[i], scene))
  }
  
  # get start and end times of all_data for the scene
  length <- 0
  for(i in 1:segs_length){
    
    start_and_end_times <- get_seg_start_and_end_times(gazesample_data_vector[[i]])
    length <- length + start_and_end_times$end - start_and_end_times$start
  }
  
  ### single_numfixations ###
  output_value <- subset(emdat_output.df, select = single_numfixations)[1,]
  numfixs <- nrow(internal_data.df)
  
  verify_equivalence(numfixs ,output_value, participant, a_scene, "single_numfixations")
  
  ### single_proportionnum ###
  output_value <- subset(emdat_output.df, select = single_proportionnum)[1,]
  internal_value <- numfixs / nrow(fixation_data_scene.df)
  
  verify_equivalence(internal_value ,output_value, participant, a_scene, "single_proportionnum")
  
  ### single_fixationrate ###
  output_value <- subset(emdat_output.df, select = single_fixationrate)[1,]
  
  fix_duration <- sum(internal_data.df$fixationduration)
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- numfixs / fix_duration
  } else {
    
    if(segs_length > 1){
      
      internal_value <- -1
    } else{
      
      internal_value <- 0
    }
  }
  
  verify_equivalence(internal_value ,output_value, participant, a_scene, "single_fixationrate")
  
  ### single_totaltimespent ###
  output_value <- subset(emdat_output.df, select = single_totaltimespent)[1,]
  
  verify_equivalence(fix_duration, output_value, participant, a_scene, "single_totaltimespent")
  
  ### single_proportiontime ###
  output_value <- subset(emdat_output.df, select = single_proportiontime)[1,]
  internal_value <- fix_duration / length
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_proportiontime")
  
  ### single_meanfixationduration ###
  output_value <- subset(emdat_output.df, select = single_meanfixationduration)[1,]
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- mean(internal_data.df$fixationduration)
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_meanfixationduration")
  
  ### single_stddevfixationduration ###
  output_value <- subset(emdat_output.df, select = single_stddevfixationduration)[1,]
  
  if(nrow(internal_data_vector[[1]]) > 1){
    
    internal_value <- sd(internal_data_vector[[1]]$fixationduration)
  } else if(nrow(internal_data_vector[[1]]) == 1){
    
    if(is.nan(output_value)){
      
      # sd evaluate to NaN in EMDAT while to NA in R if argument length is one
      # but cannot pass these values directly to verify_equivalence  
      internal_value <- 0.0
      output_value <- 0.0
    } else {
      internal_value <- NA
    }
    
  } else {
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_stddevfixationduration")
  
  ### single_longestfixation ###
  output_value <- subset(emdat_output.df, select = single_longestfixation)[1,]
  
  internal_value <- -1
  
  for(i in 1:segs_length){
    
    if(nrow(internal_data_vector[[i]]) != 0){
      
      internal_value_temp <- max(internal_data_vector[[i]]$fixationduration)
      
      if(internal_value_temp > internal_value){
        
        internal_value <- internal_value_temp
      }
    }
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_longestfixation")
  
  ### single_timetofirstfixation ###
  output_value <- subset(emdat_output.df, select = single_timetofirstfixation)[1,]
  
  internal_values <- numeric(segs_length)
  
  for(i in 1:segs_length){
    
    if(nrow(internal_data_vector[[i]]) != 0){
      
      internal_values[i] <- internal_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- min(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    i <- i+1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetofirstfixation")
  
  ### single_timetolastfixation ###
  output_value <- subset(emdat_output.df, select = single_timetolastfixation)[1,]
  
  for(i in 1:segs_length){
    
    column_length <- nrow(internal_data_vector[[i]])
    if(column_length != 0){
      
      internal_values[i] <- 
        internal_data_vector[[i]][column_length,]$timestamp - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- max(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetolastfixation")
  
  ### single_numtransfrom_single ###
  total_count <- 0
  internal_value <- 0
  output_value <- subset(emdat_output.df, select = single_numtransfrom_single)[1,]
  
  for(i in 1:segs_length){
    
    aoi1 <- list(x_left = aoi$left , x_right = aoi$right, y_bottom = aoi$bottom, y_top = aoi$top)
    aoi2 <- list(x_left = aoi$left , x_right = aoi$right, y_bottom = aoi$bottom, y_top = aoi$top)
    
    trans_from_counts <- trans_from(fixation_data_vector[[i]], aoi1, aoi2)
    internal_value <- internal_value + trans_from_counts
    total_count <- total_count + trans_from_counts
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numtransfrom_single")
  
  ### single_proptransfrom_single ###
  output_value <- subset(emdat_output.df, select = single_proptransfrom_single)[1,]
  
  if(total_count != 0){
    
    internal_value <- internal_value / total_count
  } else{
    internal_value <- 0
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_proptransfrom_single")
}

# This function checks the correctness of events
# LIST OF COLUMS TO TEST:

# single_numevents
# single_numrightclic
# single_rightclicrate
# single_numdoubleclic
# single_doubleclicrate
# single_numleftclic
# single_leftclicrate
# single_timetofirstdoubleclic
# single_timetofirstleftclic
# single_timetofirstrightclic

# Not tested; these are set to -1 in the emdat code:	
# single_timetolastdoubleclic	
# single_timetolastleftclic	
# single_timetolastrightclic	

check_aoi_eve <- function(emdat_output.df, 
                          participant, 
                          a_scene, 
                          segment.names,
                          aoi,
                          events_data_scene.df,
                          gazesample_data_scene.df){
  
  ### set up the tests ###
  internal_data.df <- subset(events_data_scene.df,
                             grepl('MouseClick', event) &
                             as.numeric(as.character(x_coord)) > aoi$left &
                             as.numeric(as.character(x_coord)) <= aoi$right &
                             as.numeric(as.character(y_coord)) <= aoi$bottom &
                             as.numeric(as.character(y_coord)) > aoi$top)

  # stores the data by segment into vectors
  internal_data_vector <- c()
  gazesample_data_vector <- c()
  segs_length <- length(segment.names)
  for(i in 1:segs_length) {

    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data_scene.df, grepl(segment.names[i], scene))
  }
  
  # get start and end times of all_data for the scene
  length <- 0
  for(i in 1:segs_length){
    
    start_and_end_times <- get_seg_start_and_end_times(gazesample_data_vector[[i]])
    length <- length + start_and_end_times$end - start_and_end_times$start
  }
  
  ### single_numevents ###
  output_value <- subset(emdat_output.df, select = single_numevents)[1,]
  
  internal_value <- nrow(internal_data.df)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numevents")
  
  ### single_numrightclic ###
  output_value <- subset(emdat_output.df, select = single_numrightclic)[1,]
  
  rightclicks.df <- subset(internal_data.df, event=="RightMouseClick")
  internal_value <- nrow(rightclicks.df)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numrightclic")
  
  ### single_rightclicrate ###
  output_value <- subset(emdat_output.df, select = single_rightclicrate)[1,]
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_rightclicrate")
  
  ### single_numdoubleclic ###
  output_value <- subset(emdat_output.df, select = single_numdoubleclic)[1,]
  
  double_clicks <- 0
  left_clicks <- 0
  first_double_clicks <- c()
  first_left_clicks <- c()
  
  for(i in 1:segs_length){
    
    clicks <- find_double_and_left_clicks(internal_data_vector[[i]])
    double_clicks <- double_clicks + clicks[1]
    left_clicks <- left_clicks + clicks[2]
    first_double_clicks[i] <- clicks[3]
    first_left_clicks[i] <- clicks[4]
  }
  internal_value <- double_clicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numdoubleclic")
  
  ### single_doubleclicrate ###
  output_value <- subset(emdat_output.df, select = single_doubleclicrate)[1,] 
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_doubleclicrate")
  
  ### single_numleftclic ###
  output_value <- subset(emdat_output.df, select = single_numleftclic)[1,]
  
  internal_value <- left_clicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numleftclic")
  
  ### single_leftclicrate ###
  output_value <- subset(emdat_output.df, select = single_leftclicrate)[1,]
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_leftclicrate")
  
  ### single_timetofirstdoubleclic ###
  output_value <- subset(emdat_output.df, select = single_timetofirstdoubleclic)[1,]
  internal_values <- c()
  
  for(i in 1:segs_length){
    
    if(first_double_clicks[i] != -1){
      
      internal_values[i] <- first_double_clicks[i] - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- min(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetofirstdoubleclic")
  
  ### single_timetofirstleftclic ###
  output_value <- subset(emdat_output.df, select = single_timetofirstleftclic)[1,]
  
  for(i in 1:segs_length){
    
    if(first_left_clicks[i] != -1){
      
      internal_values[i] <- first_left_clicks[i] - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- min(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetofirstleftclic")
  
  ### single_timetofirstrightclic ###
  output_value <- subset(emdat_output.df, select = single_timetofirstrightclic)[1,]
  
  for(i in 1:segs_length){
    
    rightclicks.df <- subset(internal_data_vector[[i]], event=="RightMouseClick")
    if(nrow(rightclicks.df) != 0){

      internal_values[i] <- rightclicks.df[1,]$timestamp - gazesample_data_vector[[i]][1,]$timestamp
    } else{

      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- min(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetofirstrightclic")
}

##########################################################################################

# When called, commences the tests for the given list of participants
run_part2Test <- function(participants, aoi_file_name, last_participant){
  
  aoi_file <- paste(aoi_file_path, aoi_file_name, ".aoi", sep = "")
  
  for(i in 1:length(participants)){
    
    participant <- participants[i]
    readfiles_aoi(participant,
                  paste(seg_file_path, "P", participant, ".seg", sep = ""),
                  aoi_file, 
                  last_participant)
  }
  writeLines(paste("####### cumulative total number of tests run: ", cumulative_counter, " #######"))
  cumulative_counter <<- 0
}

##### To Run #####

# Set up the tests: choose the range of particpants to run the tests on
participants <- list("16", "17", "18")

# Run
# Note: last_participant refers to the last in the EMDAT output file used, not necessarily that
#       in the list of participants
run_part2Test(participants, "single_aoi", "18")









