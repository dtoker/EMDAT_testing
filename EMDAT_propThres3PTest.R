### Set up tests ###
source("EMDAT_testUtils.R")

root <- "Part2_EMDATInternal_EMDATOutput/three_parts_study_data/"
feature_files_path <- paste(root, "features/", sep = "")
export_files_root <- "Part1_TobiiV3Output_EMDATInternal/three_parts_study_data/seg_and_export/P"
internal_data_files_path <- paste(root, "EMDAT_internal_data/", sep = "")
seg_files_path <- paste(root, "seg_files/P", sep = "")

emdat_export_all.df <- read.csv(paste(feature_files_path, 
                                      "tobiiv3_sample_features_prop_thres",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")
Sc_ids <- as.character(emdat_export_all.df[,1])

cumulative_counter <- 0

# sets the values of the tested parameters
valid_prop_threshold <- 0.8
p_threshold <- 0.8 # for aggregation done for all segs belonging to a given participant    

### Tests ### 
test_param <- function(participant, seg_file, last_participant){
  
  # reads in the needed internal EMDAT data file once for the given participant 
  fixation_data.df <- read.csv(paste(internal_data_files_path,"EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
  
  # reads in tobii_export file for the participant and subset it for computation of validity     
  tobii_export.df <- read.csv(paste(export_files_root, participant, "_Data_Export.tsv", sep = ""), sep="\t")
  tobii_all.df <- subset(tobii_export.df, 
                         MediaName == 'ScreenRec'&
                         ValidityLeft != ''&
                         ValidityRight != '')
  
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  # extracts scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  # loops over the scenes while keeping track of validity both at participant and scene levels   
  validity_seq <- list() # stores scene level valdity values
  p_total_data_size <- 0 # these three variabble prepended by p_ for the participant level values    
  p_valid_data_size <- 0
  p_validity <- 0
  
  for (a_scene in scene.names) {
    
    total_data_size <- 0
    valid_data_size <- 0
    internal_value <- 0
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    # extracts scene fixation data
    fixation_data_scene.df <- subset(fixation_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
    
    # collects values needed for the scene level validity computation 
    for(seg in segment.names){
      
      start_time <- subset(seg_file.df, segment == seg)$start
      end_time <- subset(seg_file.df, segment == seg)$end
      tobii_seg.df <- subset(tobii_all.df,
                             (RecordingTimestamp >= start_time) & (RecordingTimestamp <= end_time))
      
      seg_fixation <- subset(fixation_data_scene.df, grepl(seg, scene))
      
      if(nrow(tobii_seg.df) != 0 & nrow(seg_fixation) != 0){
        
        total_data_size <- total_data_size + nrow(tobii_seg.df)
        
        tobii_seg_valid.df <- subset(tobii_seg.df, (ValidityLeft < 2 | ValidityRight < 2) | FixationIndex != '')
        valid_data_size <- valid_data_size + nrow(tobii_seg_valid.df)
      } 
    }
    
    # stores the values later used for computation of the participant level validity value    
    p_total_data_size <- p_total_data_size + total_data_size
    p_valid_data_size <- p_valid_data_size + valid_data_size
    
    # computes and stores the scene level validity value 
    if(total_data_size != 0) {
      
      internal_value <- valid_data_size / total_data_size
    }
    validity_seq[[a_scene]] <- internal_value
  }
  
  # computes the participant level validity value     
  if(p_total_data_size != 0) {
    
    p_validity <- p_valid_data_size / p_total_data_size
  }
  
  if(p_validity > p_threshold){
    
    # reads the pertinent part of the features file for the given participant (*)
    emdat_export.df <- get_features_df_for_participant_for_3(emdat_export_all.df, participant, Sc_ids, last_participant)
    
    # checks that the valid participant was in fact not dropped      
    assert_true(nrow(emdat_export.df) > 0, participant, "allsc", as.character(p_validity))
    
    # proceeeds to scene level check  
    for(a_scene in scene.names){
      
      # reads the pertinent part of the file from (*) above for the given scene 
      emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
      
      internal_value <- validity_seq[[a_scene]]
      
      if(internal_value > valid_prop_threshold) {
        
        # above the threshold; the scene is valid and should be in the generated file
        assert_true(nrow(emdat_export.df.scene) > 0, participant, a_scene, as.character(internal_value))
      }else {
        
        # below the threshold; the scene is invalid and should not be in the generated file
        assert_true(nrow(emdat_export.df.scene) == 0, participant, a_scene, as.character(internal_value), " dropped")
      }
    }
  } else{
    
    # checks that the invalid participant was in fact dropped
    assert_true(!Reduce("|", grepl(participant, Sc_ids)), participant, "allsc", as.character(p_validity), " dropped") 
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



