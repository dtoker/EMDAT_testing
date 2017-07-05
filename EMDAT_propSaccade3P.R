### Set up tests ###
source("EMDAT_testUtils.R")

root <- "Part2_EMDATInternal_EMDATOutput/three_parts_study_data/"
export_files_root <- "Part1_TobiiV3Output_EMDATInternal/three_parts_study_data/seg_and_export/P"
internal_data_files_path <- paste(root, "EMDAT_internal_data/", sep = "")
seg_files_path <- paste(root, "seg_files/P", sep = "")

cumulative_counter <- 0

# sets the values of the tested parameters
valid_samples_prop_saccade <- 1.0  # for now 

### Tests ### 
test_param <- function(participant, seg_file, last_participant){
  
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  # extracts scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  # reads in the needed internal EMDAT data file once for the given participant 
  saccade_data.df <- read.csv(paste(internal_data_files_path,"EMDATdata_sac_P", participant, ".tsv", sep=""), sep="\t")
  
  # reads in tobii_export file for the participant and process it for all saccade data     
  tobii_export.df <- read.csv(paste(export_files_root, participant, "_Data_Export.tsv", sep = ""), sep="\t")
  ######## TODO1: put logic to extract all saccaade data here #############
  in_saccade <- FALSE
  in_fixation <- FALSE
  last_gaze_time <- 0
  last_valid <- FALSE
  time_stamps <- numeric()
  next_index <- 1
    
  nb_invalid_temp <- 0
  nb_valid_sample <- 0
  nb_sample <- 0
  
  tobii_all.df <- subset(tobii_export.df, 
                         MediaName == 'ScreenRec'&
                         !is.na(RecordingTimestamp))
  
  # needs only the rows after the first fixation  
  
  for(i in 1:nrow(tobii_all.df)){
    
    current_row <- tobii_all.df[i,]
    
    if(in_fixation){
      
      if(current_row$GazeEventType == "Fixation"){
        
        nb_invalid_temp <- 0
      } else if(current_row$GazeEventType == "Saccade"){
        
        in_fixation <- FALSE
        in_saccade <- TRUE
        last_gaze_time <- last_gaze_time_temp
        nb_valid_sample <- 0
        
        if((current_row$ValidityLeft < 2 | current_row$ValidityRight < 2) &
           !is.na(current_row$GazePointX..ADCSpx.) &
           !is.na(current_row$GazePointY..ADCSpx.)){
          
          nb_valid_sample <- nb_valid_sample + 1
        }
        
        if(last_valid){
          
          nb_valid_sample <- nb_valid_sample + 1
        }
        
        nb_sample <- 2 + nb_invalid_temp
        nb_invalid_temp <- 0
      } else {
        
        nb_invalid_temp <- nb_invalid_temp + 1
      } 
    } else if(in_saccade){
      
      if(current_row$GazeEventType == "Fixation"){
        
        in_fixation <- TRUE
        in_saccade <- FALSE
        
        if((current_row$ValidityLeft < 2 | current_row$ValidityRight < 2) &
           !is.na(current_row$GazePointX..ADCSpx.) &
           !is.na(current_row$GazePointY..ADCSpx.)){
          
          nb_valid_sample <- nb_valid_sample + 1
        } else if(!is.na(current_row$FixationPointX..MCSpx.) &
                  !is.na(current_row$FixationPointY..MCSpx.)){
          
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
        
            if((current_row$ValidityLeft < 2 | current_row$ValidityRight < 2) &
               !is.na(current_row$GazePointX..ADCSpx.) &
               !is.na(current_row$GazePointY..ADCSpx.)){
              
              nb_valid_sample <- nb_valid_sample + 1
            }
            nb_sample <- nb_sample + 1
      } else{
        nb_sample <- nb_sample + 1 
      }
      
      nb_invalid_temp <- 0
    } else{
      
      if(current_row$GazeEventType == "Fixation"){
        
        in_fixation <- TRUE
      }
    }
    
    if(!is.na(current_row$GazePointX..ADCSpx.) &
       !is.na(current_row$GazePointY..ADCSpx.)){
      
      if(current_row$RecordingTimestamp == 100743) {
        print(current_row$RecordingTimestamp)
      }
      last_gaze_time_temp <- current_row$RecordingTimestamp
      last_valid <- current_row$ValidityLeft < 2 | current_row$ValidityRight < 2 
    } else if(current_row$GazeEventType == "Fixation" &
              !is.na(current_row$FixationPointX..MCSpx.) &
              !is.na(current_row$FixationPointY..MCSpx.)) {
      
      last_gaze_time_temp <- current_row$RecordingTimestamp
      last_valid <- TRUE
    }
  }
  
  for (a_scene in scene.names) {
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    saccade_data_scene.df <- subset(saccade_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
    
    for(seg in segment.names){
      
      ########## TODO2: 
      #########       1. extract seg saccade data from all saccade data
      #########       2. get the corresponding saccade data from the internal state file
      #########       3. compare to show they are the same in size
      start_time <- subset(seg_file.df, segment == seg)$start
      end_time <- subset(seg_file.df, segment == seg)$end
      time_stamps_seg <- subset(time_stamps,
                             (time_stamps >= start_time) & (time_stamps <= end_time))
      
      seg_saccade <- subset(saccade_data_scene.df, grepl(seg, scene))
      
      # for(i in 1: length(time_stamps_seg)){
      # 
      #   if(time_stamps_seg[i] != seg_saccade$timestamp[i]){
      #     print(paste(i, " ", time_stamps_seg[i]))
      #   }
      # }
      
      verify_equivalence(length(time_stamps_seg), nrow(seg_saccade), participant, seg, "saccade numbers", " and seg:")
    }
  }
  
  report_success(participant, cumulative_counter)
}

##########################################################################################

# When called, commences the tests for the given list of participants
# last_participant refers to the last in the given study, not necessarily that
# in the list of participants
run_parameterTest <- function(participants, last_participant){
  
  for(i in 1:length(participants)){
    
    participant <- participants[i]
    test_param(participant,
               paste(seg_files_path, participant, ".seg", sep = ""),
               last_participant)
  }
  writeLines(paste("####### cumulative total number of tests run: ", cumulative_counter, " #######"))
  cumulative_counter <<- 0
}

##### To Run #####

# Set up the tests: choose the range of particpants to run the tests on

participants <- list("16")

# Run
# Note: second argument takes the last participant of the study, not necessarily the
#       last element in the list of participants given to the first argument

run_parameterTest(participants, "18")



