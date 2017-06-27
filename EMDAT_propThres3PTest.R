### TEST SCRIPT ###

source("EMDAT_testUtils.R")

root_path <- "Part2_EMDATInternal_EMDATOutput/old_data/"
export_files_root <- "Part1_TobiiV3Output_EMDATInternal/old_data/P"
emdat_export_all.df <- read.csv(paste(root_path, 
                                      "tobiiv3_sample_features",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")
Sc_ids <- as.character(emdat_export_all.df[,1])
cumulative_counter <- 0

valid_prop_threshold <- 0.8

test_param <- function(participant, seg_file, last_participant){
  
  # reads the pertinent part of the features file for the given participant (*)
  #emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
  emdat_export.df <- get_features_df_for_participant_for_3(emdat_export_all.df, participant, Sc_ids, last_participant)
  
  # reads in the needed internal EMDAT data files once for the given participant 
  fixation_data.df <- read.csv(paste(root_path,"EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
  
  # reads in tobii_export file for the participant and subset it for computation of validity     
  tobii_export.df <- read.csv(paste(export_files_root, participant, "_Data_Export.tsv", sep = ""), sep="\t")
  tobii_all.df <- subset(tobii_export.df, 
                         MediaName == 'ScreenRec'&
                           ValidityLeft != ''&
                           ValidityRight != '')
  #tobii_valid.df <- subset(tobii_all.df, (ValidityLeft < 2 || ValidityRight < 2) || FixationIndex != '')
  
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  # extracts scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  # loops over the scenes
  for (a_scene in scene.names) {
    
    total_data_size <- 0
    valid_data_size <- 0
    internal_value <- 0
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    # reads the pertinent part of the file from (*) above for the given scene 
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    for(seg in segment.names){
      
      start_time <- subset(seg_file.df, segment == seg)$start
      end_time <- subset(seg_file.df, segment == seg)$end
      tobii_seg.df <- subset(tobii_all.df,
                             (RecordingTimestamp >= start_time) & (RecordingTimestamp <= end_time))
      
      if(nrow(tobii_seg.df) != 0){
        
        total_data_size <- total_data_size + nrow(tobii_seg.df)
        
        tobii_seg_valid.df <- subset(tobii_seg.df, (ValidityLeft < 2 | ValidityRight < 2) | FixationIndex != '')
        valid_data_size <- valid_data_size + nrow(tobii_seg_valid.df)
      } 
    }
    
    if(total_data_size != 0) {
      
      internal_value <- valid_data_size / total_data_size
    }
    
    if(internal_value > valid_prop_threshold) {
      
      assert_true(nrow(emdat_export.df.scene) > 0, participant, a_scene, as.character(internal_value))
    }else {
      
      assert_true(nrow(emdat_export.df.scene) == 0, participant, a_scene, as.character(internal_value))
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
               paste(root_path, "SegFiles/P", participant, ".seg", sep = ""),
               last_participant)
  }
  writeLines(paste("####### cumulative total number of tests run: ", cumulative_counter, " #######"))
  cumulative_counter <<- 0
}

##### To Run #####

# Set up the tests: choose the range of particpants to run the tests on

#participants <- generate_participant_list(101:101)
participants <- list("16", "17", "18")

# Run
# Note: second argument takes the last participant of the study, not necessarily the
#       last element in the list of participants given to the first argument

run_parameterTest(participants, "18")



