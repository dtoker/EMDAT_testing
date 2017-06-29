########### Set Up Tests ######################
source("EMDAT_testUtils.R")

root <- "Part2_EMDATInternal_EMDATOutput/intervention_study_data/"
feature_files_path <- paste(root, "features/", sep = "")
internal_data_files_path <- paste(root, "EMDAT_internal_data/", sep = "")
seg_files_path <- paste(root, "seg_files/P", sep = "")

emdat_export_all.df <- read.csv(paste(feature_files_path, 
                                      "tobiiv3_sample_features",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")
Sc_ids <- as.character(emdat_export_all.df[,1])

#################### Tests ##########################

find_missing <- function(participant, seg_file, last_participant){
  
  emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  #extract scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  # reads in the needed internal EMDAT data files
  # fixation_data.df <- read.csv(paste(internal_data_files_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
  # gazesample_data.df <- read.csv(paste(internal_data_files_path, "EMDATinternaldata_gazesamples_", participant, ".csv", sep=""), sep=",")
  # saccade_data.df <- read.csv(paste(internal_data_files_path, "EMDATinternaldata_saccades_", participant, ".csv", sep=""), sep=",")
  # events_data.df <- read.csv(paste(internal_data_files_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
  
  space_marker <- FALSE
  
  # loop over the scenes
  for(a_scene in scene.names){
    
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    if(nrow(emdat_export.df.scene) == 0){
      
      print(paste(participant, " ",  a_scene,
                  " has no data in output file",
                  sep = ""))
      space_marker <- TRUE
    } 
    
    # keeps all segments belonging to the scene in data frame format 
    # fixation_data_sub.df <- subset(fixation_data.df, scene == a_scene)
    # gazesample_data_sub.df <- subset(gazesample_data.df, scene == a_scene)
    # saccade_data_sub.df <- subset(saccade_data.df, scene == a_scene)
    # events_data_sub.df <- subset(events_data.df, scene == a_scene)
    # 
    # if(nrow(fixation_data_sub.df) == 0){
    #   print(paste(participant, " ",  a_scene,
    #               " has no data in fixation file",
    #               sep = ""))
    #   space_marker <- TRUE
    # }
    # if(nrow(gazesample_data_sub.df) == 0){
    #   print(paste(participant, " ",  a_scene,
    #               " has no data in gaze_sample file",
    #               sep = ""))
    #   space_marker <- TRUE
    # }  
    # if(nrow(saccade_data_sub.df) == 0){
    #   print(paste(participant, " ",  a_scene,
    #               " has no data in saccade file",
    #               sep = ""))
    #   space_marker <- TRUE
    # }
    # if(nrow(events_data_sub.df) == 0){
    #   print(paste(participant, " ",  a_scene,
    #               " has no data in events file",
    #               sep = ""))
    #   space_marker <- TRUE
    # }
    # 
    if(space_marker){
      
      writeLines("")
      space_marker <- FALSE
    }
  }
}

participants <- generate_participant_list(101:105)

run_tests <- function(participants, last_participant){
  
  for(i in 1:length(participants)){
    
    participant <- participants[i]
    find_missing(participant, 
                 paste(seg_files_path, participant, ".seg", sep = ""),
                 last_participant)
  }
}

run_tests(participants, "105b")

