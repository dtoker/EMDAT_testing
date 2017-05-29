########### Set Up Tests ######################

root_path <- "Part2_EMDATInternal_EMDATOutput/new_data/"
emdat_export_all.df <- read.csv(paste(root_path, 
                                      "tobiiv3_sample_features",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")
Sc_ids <- as.character(emdat_export_all.df[,1])

############ Helper Functions #################

# Given all particapnts data, returns only the scenes belonging to the given participant
# This idea of row retrieval is from 
# http://stackoverflow.com/questions/5553802/get-row-number-for-r-data-frame   
get_features_df_for_participant <- function(emdat_export_all.df, participant, Sc_ids, last_participant){
  
  start_row <- which(Sc_ids==paste(participant, "_allsc", sep = "")) + 1
  
  if(participant != last_participant){
    
    number_char <- nchar(participant)
    last_char <- substr(participant, number_char, number_char)
    
    if(last_char == "a"){
      
      end_row <- which(Sc_ids==paste(substr(participant, 1, number_char-1), "b_allsc", sep = "")) - 1
    } else {
      
      numerical_part <- as.numeric(substr(participant, 1, number_char - 1))
      
      # no seg file for participant 143 
      if(numerical_part != 142){
        numerical_part <- numerical_part + 1
      } else{
        numerical_part <- numerical_part + 2
      }
      
      end_row <- which(Sc_ids==paste(numerical_part, "a_allsc", sep = "")) - 1
    }
  } else{
    
    end_row <- length(Sc_ids)
  }
  
  return(emdat_export_all.df[start_row : end_row, ])
}

generate_participant_list <- function(p_range){
  
  participants <- list()
  last_index = 0
  
  for(i in p_range){
    
    participant_a = paste(as.character(i), "a", sep = "")
    participant_b = paste(as.character(i), "b", sep = "")
    
    last_index = last_index + 1
    participants[last_index] = participant_a
    
    last_index = last_index + 1
    participants[last_index] = participant_b
  }
  
  return(participants)
}

#################### Tests ##########################

find_missing <- function(participant, seg_file, last_participant){
  
  emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  
  #extract scene names
  scene.names <- unique(seg_file.df[,"scene"])
  
  # reads in the needed internal EMDAT data files
  fixation_data.df <- read.csv(paste(root_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
  gazesample_data.df <- read.csv(paste(root_path, "EMDATinternaldata_gazesamples_", participant, ".csv", sep=""), sep=",")
  saccade_data.df <- read.csv(paste(root_path, "EMDATinternaldata_saccades_", participant, ".csv", sep=""), sep=",")
  events_data.df <- read.csv(paste(root_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
  
  # loop over the scenes
  for(a_scene in scene.names){
    
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    if(nrow(emdat_export.df.scene) == 0){
      
      print(paste(participant, " ",  a_scene,
                  " has no data in output file",
                  sep = ""))                                                
    } 
    
    # keeps all segments belonging to the scene in data frame format 
    fixation_data_sub.df <- subset(fixation_data.df, scene == a_scene)
    gazesample_data_sub.df <- subset(gazesample_data.df, scene == a_scene)
    saccade_data_sub.df <- subset(saccade_data.df, scene == a_scene)
    events_data_sub.df <- subset(events_data.df, scene == a_scene)
    
    if(nrow(fixation_data_sub.df) == 0){
      print(paste(participant, " ",  a_scene,
                  " has no data in fixation file",
                  sep = ""))
    }
    if(nrow(gazesample_data_sub.df) == 0){
      print(paste(participant, " ",  a_scene,
                  " has no data in gaze_sample file",
                  sep = ""))
    }  
    if(nrow(saccade_data_sub.df) == 0){
      print(paste(participant, " ",  a_scene,
                  " has no data in saccade file",
                  sep = ""))
    }
    if(nrow(events_data_sub.df) == 0){
      print(paste(participant, " ",  a_scene,
                  " has no data in events file",
                  sep = ""))
    }
  }
}

participants <- generate_participant_list(144:162)

run_tests <- function(participants, last_participant){
  
  for(i in 1:length(participants)){
    
    participant <- participants[i]
    find_missing(participant, 
                 paste(root_path, "SegFiles/P", participant, ".seg", sep = ""),
                 last_participant)
  }
}

run_tests(participants, "162b")

