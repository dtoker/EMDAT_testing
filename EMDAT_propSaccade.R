### Set up tests ###
source("EMDAT_testUtils.R")

### for 3P study data ###
root <- "Part2_EMDATInternal_EMDATOutput/three_parts_study_data/"
export_files_root <- "Part1_TobiiV3Output_EMDATInternal/three_parts_study_data/seg_and_export/P"

### for intervention study data  ###
# root <- "Part2_EMDATInternal_EMDATOutput/intervention_study_data/"
# export_files_root <- "Part1_TobiiV3Output_EMDATInternal/intervention_study_data/seg_and_export/P"

internal_data_files_path <- paste(root, "EMDAT_int_data_saccade_param/", sep = "")
seg_files_path <- paste(root, "seg_files/P", sep = "")

cumulative_counter <- 0

# sets the values of the parameter to be tested
valid_samples_prop_saccade <- 0.5  

### Tests ### 
test_param <- function(participant, seg_file){
  
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  # extracts scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  # reads in the needed internal EMDAT data file once for the given participant 
  saccade_data.df <- read.csv(paste(internal_data_files_path, "EMDATinternaldata_saccades_", participant, ".csv", sep=""), sep=",")
  
  # reads in tobii_export file for the participant and process it for all saccade data     
  tobii_export.df <- read.csv(paste(export_files_root, participant, "_Data_Export.tsv", sep = ""), sep="\t")
  
  ## extract all saccaade data and then compare with the actual values ##
  in_saccade <- FALSE
  in_fixation <- FALSE
  last_gaze_time <- 0
  last_valid <- FALSE
  next_index <- 1
  nb_invalid_temp <- 0
  nb_valid_sample <- 0
  nb_sample <- 0
  
  tobii_all.df <- subset(tobii_export.df, 
                         MediaName == 'ScreenRec'&
                         !is.na(RecordingTimestamp))
  
  # initalize the numeric vector, time_stamps, into which valid sacccde times are collected
  tobii_saccades_bound <- nrow(subset(tobii_all.df, GazeEventType == "Saccade"))
  time_stamps <- rep(-1, tobii_saccades_bound)
  
  for(i in 1:nrow(tobii_all.df)){
    
    current_row <- tobii_all.df[i,]
    
    if(in_fixation){
      
      if(current_row$GazeEventType == "Fixation"){
        
        nb_invalid_temp <- 0
      } else if(current_row$GazeEventType == "Saccade"){
        
        # switch the boolean values
        in_fixation <- !in_fixation
        in_saccade <- !in_saccade
        
        last_gaze_time <- last_gaze_time_temp
        nb_valid_sample <- 0
        
        if(is_valid_gazesample(current_row)){
          
          nb_valid_sample <- nb_valid_sample + 1
        }
        
        if(last_valid){
          
          nb_valid_sample <- nb_valid_sample + 1
        }
        
        nb_sample <- 2 + nb_invalid_temp
        nb_invalid_temp <- 0
      } else{
        
        nb_invalid_temp <- nb_invalid_temp + 1
      } 
    } else if(in_saccade){
      
      if(current_row$GazeEventType == "Fixation"){
        
        # switch the boolean values
        in_fixation <- !in_fixation
        in_saccade <- !in_saccade
        
        if(is_valid_gazesample(current_row)){
          
          nb_valid_sample <- nb_valid_sample + 1
        } else if(exist_points(current_row, "FixationPointX..MCSpx.", "FixationPointY..MCSpx.")){
          
          nb_valid_sample <- nb_valid_sample + 1
        }
        nb_sample <- nb_sample + 1
        
        if((nb_valid_sample / nb_sample) >= valid_samples_prop_saccade){
          
          time_stamps[next_index] <- last_gaze_time
          next_index <- next_index + 1
          nb_valid_sample <- 0
          nb_sample <- 0
        }
      } else if(current_row$GazeEventType == "Saccade"){
        
            if(is_valid_gazesample(current_row)){
              
              nb_valid_sample <- nb_valid_sample + 1
            }
            nb_sample <- nb_sample + 1
      } else{
        nb_sample <- nb_sample + 1 
      }
      
      nb_invalid_temp <- 0
    } else{
      
      if(current_row$GazeEventType == "Fixation"){
        
        in_fixation <- !in_fixation
      }
    }
    
    if(exist_points(current_row,"GazePointX..ADCSpx.", "GazePointY..ADCSpx.")){
      
      last_gaze_time_temp <- current_row$RecordingTimestamp
      last_valid <- current_row$ValidityLeft < 2 | current_row$ValidityRight < 2 
    } else if(current_row$GazeEventType == "Fixation" & 
              exist_points(current_row, "FixationPointX..MCSpx.", "FixationPointY..MCSpx.")
              ){
      
      last_gaze_time_temp <- current_row$RecordingTimestamp
      last_valid <- TRUE
    }
  }
  
  # test matchs 
  for (a_scene in scene.names) {
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    #saccade_data_scene.df <- subset(saccade_data.df, grepl(a_scene, scene))
    
    saccade_data_scene.df <- subset(saccade_data.df, scene == a_scene)
    
    for(seg in segment.names){
      
      # extract timestamps of the valid saccades belonging to the seg
      start_time <- subset(seg_file.df, segment == seg)$start
      end_time <- subset(seg_file.df, segment == seg)$end
      time_stamps_seg <- subset(time_stamps,
                             (time_stamps >= start_time) & (time_stamps <= end_time))
      
      seg_saccade <- subset(saccade_data_scene.df, grepl(seg, scene))
      
      verify_equivalence(length(time_stamps_seg), nrow(seg_saccade), participant, seg, "valid saccades", " and seg:")
    }
  }
  
  report_success(participant, cumulative_counter)
}

##########################################################################################

# helper function to check validity of points
is_valid_gazesample <- function(current_row){
  
  (current_row$ValidityLeft < 2 | current_row$ValidityRight < 2) &
  exist_points(current_row, "GazePointX..ADCSpx.", "GazePointY..ADCSpx.")
}

# helper function to check that the cells in the file are not NA
exist_points <- function(current_row, x_point, y_point) {
  
  !is.na(current_row[,x_point]) & !is.na(current_row[,y_point])
}   

# When called, commences the tests for the given list of participants
run_parameterTest <- function(participants){
  
  for(i in 1:length(participants)){
    
    participant <- participants[i]
    test_param(participant,
               paste(seg_files_path, participant, ".seg", sep = ""))
  }
  writeLines(paste("####### cumulative total number of tests run: ", cumulative_counter, " #######"))
  cumulative_counter <<- 0
}

##### To Run #####

# Set up the tests: choose the range of particpants to run the tests on
participants <- list("16", "17")
#participants <- generate_participant_list(162:162)

# Run
run_parameterTest(participants)



