### Set up tests ###
source("EMDAT_testUtils.R")

root <- "Part2_EMDATInternal_EMDATOutput/intervention_study_data/"
feature_files_path <- paste(root, "features/", sep = "")
export_files_root <- "Part1_TobiiV3Output_EMDATInternal/intervention_study_data/seg_and_export/P"
internal_data_files_path <- paste(root, "EMDAT_internal_data/", sep = "")
seg_files_path <- paste(root, "seg_files/P", sep = "")

emdat_export_all.df <- read.csv(paste(feature_files_path, 
                                      "tobiiv3_sample_features_prop_thres_intr",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")
Sc_ids <- as.character(emdat_export_all.df[,1])

# reads in the features file generated with VALID_PROP_THRESH = 0
# for finding missing scenes for some other reasons  
emdat_export_all_null_prop.df <- read.csv(paste(feature_files_path, 
                                      "tobiiv3_sample_features",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")
Sc_ids_null_prop <- as.character(emdat_export_all_null_prop.df[,1])

cumulative_counter <- 0

# sets the values of the tested parameters
valid_prop_threshold <- 0.8
p_threshold <- 0.8

### Tests ###
test_param <- function(participant, seg_file, last_participant){
  
  # reads in the needed internal EMDAT data file once for the given participant 
  fixation_data.df <- read.csv(paste(internal_data_files_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
  
  # reads in tobii_export file for the participant and subset it for computation of validity     
  tobii_export.df <- read.csv(paste(export_files_root, participant, "_Data_Export.tsv", sep = ""), sep="\t")
  tobii_all.df <- subset(tobii_export.df, 
                         MediaName == 'ScreenRec'&
                         ValidityLeft != ''&
                         ValidityRight != '')
  
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  # extracts scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  # find any missing scenes for the participant even when VALID_PROP_THRESH = 0   
  missing_scenes <- find_missing(participant, scene.names, last_participant)
    
  # loops over the extant scenes while keeping track of validity both at participant and scene levels   
  validity_seq <- list() # stores scene level valdity values
  p_total_data_size <- 0 # these three variabble prepended by p_ for the participant level values
  p_valid_data_size <- 0
  p_validity <- 0
  
  for (a_scene in setdiff(scene.names, missing_scenes)) {
    
    total_data_size <- 0
    valid_data_size <- 0
    internal_value <- 0
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    # extracts fixation data for the scene (and segment since these are single-segment scenes)     
    fixation_data <- subset(fixation_data.df, grepl(a_scene, scene))
    
    # collects values needed for the scene level validity computation                                 
    for(seg in segment.names){
      
      start_time <- subset(seg_file.df, segment == seg)$start
      end_time <- subset(seg_file.df, segment == seg)$end
      tobii_seg.df <- subset(tobii_all.df,
                             (RecordingTimestamp >= start_time) & (RecordingTimestamp <= end_time))
      
      if(nrow(tobii_seg.df) != 0 & nrow(fixation_data) != 0){
        
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
    emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
    
    # checks that the valid participant was in fact not dropped
    assert_true(nrow(emdat_export.df) > 0, participant, "allsc", as.character(p_validity))
    
    # proceeeds to scene level check
    for(a_scene in setdiff(scene.names, missing_scenes)){
      
      # reads the pertinent part of the file from (*) above for the given scene 
      emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
      
      internal_value <- validity_seq[[a_scene]]
      
      if(internal_value > valid_prop_threshold) {
        
        # above the threshold; the scene is valid and should be in the generated file
        assert_true(nrow(emdat_export.df.scene) > 0, participant, a_scene, as.character(internal_value))
      }else {
        
        # below the threshold; the scene is invalid and should not be in the generated file
        assert_true(nrow(emdat_export.df.scene) == 0, participant, a_scene, as.character(internal_value))
      }
    }
  } else{
    
    # checks that the invalid participant was in fact dropped
    assert_true(!Reduce("|", grepl(participant, Sc_ids)), participant, "allsc", as.character(p_validity)) 
  }
  
  report_success(participant, cumulative_counter)
}

get_features_df_for_participant <- function(emdat_export_all.df, participant, Sc_ids, last_participant){
  
  start_row <- which(Sc_ids==paste(participant, "_allsc", sep = "")) + 1
  
  if(participant != last_participant){
    
    emdat_export.df <- emdat_export_all.df[start_row: nrow(emdat_export_all.df),]
    # subtract 2 to offset the extra rows, whcih are simply an artifcat of the addition    
    end_row <- start_row + which(grepl("_allsc", emdat_export.df$Sc_id))[1] - 2    
  } else{
    
    end_row <- length(Sc_ids)
  }
  
  return(emdat_export_all.df[start_row : end_row, ])
}

find_missing <- function(participant, scene.names, last_participant){
  
  missing_scenes <- list()
  emdat_export_null_prop.df <- get_features_df_for_participant(emdat_export_all_null_prop.df, participant, Sc_ids_null_prop, last_participant)
  
  # loop over the scenes to find missing ones
  for(a_scene in scene.names){
    
    emdat_export_null_prop.df.scene <- subset(emdat_export_null_prop.df, Sc_id == a_scene)
    
    if(nrow(emdat_export_null_prop.df.scene) == 0){
      
      missing_scenes[[a_scene]] <- a_scene
    } 
  }
  return(missing_scenes)
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

participants <- generate_participant_list(105:105)

# Run
# Note: second argument takes the last participant of the study, not necessarily the
#       last element in the list of participants given to the first argument

run_parameterTest(participants, "105b")