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
  
  for (a_scene in scene.names) {
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    for(seg in segment.names){
      
      ########## TODO2: 
      #########       1. extract seg saccade data from all saccade data
      #########       2. get the corresponding saccade data from the internal state file
      #########       3. compare to show they are the same in size 
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

participants <- list("16", "17", "18")

# Run
# Note: second argument takes the last participant of the study, not necessarily the
#       last element in the list of participants given to the first argument

run_parameterTest(participants, "18")



