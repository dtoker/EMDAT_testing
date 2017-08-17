source("EMDAT_testUtils.R")

### set up the tests by initializing varibales in the global scope ###
root <- "Part2_EMDATInternal_EMDATOutput/intervention_study_data/"
internal_data_path <- paste(root, "EMDAT_internal_data/", sep = "")
seg_file_path <- paste(root, "seg_files/", sep = "")
feature_files_path <- paste(root, "features/", sep = "")
aoi_file_path <- "Part1_TobiiV3Output_EMDATInternal/intervention_study_data/"

# choose aois here
aoi_file_name <- "viz-specific" 
#aoi_file_name <- "grid2x2"

emdat_export_all.df <- read.csv(paste(feature_files_path, 
                                      "tobiiv3_sample_features_",
                                      aoi_file_name,
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")

Sc_ids <- as.character(emdat_export_all.df[,1])

cumulative_counter <- 0

### test scripts ###
readfiles_aoi <- function(participant, seg_file, aoi_file, last_participant){
  
  # reads the pertinent part of the features file for the given participant (*)
  emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
  
  # extracts scene names
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  scene.names <- unique(seg_file.df[,"scene"])
  
  # extracts aoi names
  aoi_file.df <- read.csv(aoi_file, sep="\t", 
                          header = FALSE, 
                          col.names = c("aoi_name","TL","TR","BR", "BL"), 
                          stringsAsFactors = FALSE)
  aoi.names <- aoi_file.df[, "aoi_name"]
  
  # stores aoi names and boundaries in df 
  aois <- lapply(aoi.names, extract_aoi_coordinate, aoi_file.df= aoi_file.df)
  aois.data <- Reduce(rbind2, aois)
  
  # reads in the internal EMDAT data files necessary for computing expecetd values, 
  # once for the given participant 
  fixation_data.df <- read.csv(paste(internal_data_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
  events_data.df <- read.csv(paste(internal_data_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
  gazesample_data.df <- read.csv(paste(internal_data_path, "EMDATinternaldata_gazesamples_", participant, ".csv", sep=""), sep=",")
  
  # replaces None vlaue in x_ and y_coords with NA to elegantly handle inequality comparisons later
  # in check_aoi_eve    
  if(participant == "147b"){
    events_data.df$x_coord <- replace(events_data.df$x_coord, which(events_data.df$x_coord == 'None'), NA)
    events_data.df$y_coord <- replace(events_data.df$y_coord, which(events_data.df$y_coord == 'None'), NA)
  }
  
  # loops over the scenes
  for (a_scene in scene.names) {
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    # reads the pertinent part of the file from (*) above for the given scene 
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    # if-statement below guards aginst missing scenes in the files   
    if(nrow(emdat_export.df.scene) != 0) {
      
      # keeps all segments belonging to the scene in data frame format
      gazesample_data_scene.df <- subset(gazesample_data.df, scene == a_scene)
      fixation_data_scene.df <- subset(fixation_data.df, scene == a_scene)
      events_data_scene.df <- subset(events_data.df, scene == a_scene)
      
      if(nrow(fixation_data_scene.df) != 0 &
         nrow(gazesample_data_scene.df) != 0){

        for(aoi_name in aoi.names){

          check_aoi_fix(emdat_export.df.scene,
                        participant,
                        a_scene,
                        aoi_name,
                        aois.data,
                        segment.names,
                        gazesample_data_scene.df,
                        fixation_data_scene.df)
        }

      }
      
      if(nrow(gazesample_data_scene.df) != 0){

        for(aoi_name in aoi.names){

          check_aoi_eve(emdat_export.df.scene,
                        participant,
                        a_scene,
                        aoi_name,
                        aois.data,
                        segment.names,
                        events_data_scene.df,
                        gazesample_data_scene.df)
        }
      }
    }
  }
  report_success(participant, cumulative_counter)
}

# This function checks the correctness of fixations
# LIST OF COLUMS TO TEST:

# numfixations
# proportionnum
# fixationrate
# totaltimespent
# proportiontime
# meanfixationduration
# stddevfixationduration (Update the definition if the output file is regenerated)
# longestfixation
# timetofirstfixation
# timetolastfixation
# numtransfrom
# proptransfrom

check_aoi_fix <- function(emdat_output.df, 
                          participant, 
                          a_scene,
                          aoi_name,
                          aois.data,
                          segment.names,
                          gazesample_data_scene.df,
                          fixation_data_scene.df){
  
  ### set up the tests ###
  aoi <- aois.data[aois.data[,"aoi_name"] == aoi_name,]
  
  if(class(aoi_name) == "integer"){
    
    aoi_feature_name_root <- set_root_name(paste("X", aoi_name, sep = ""))
  } else {
    
    aoi_feature_name_root <- set_root_name(aoi_name)
  }
  
  # get data inside aoi
  internal_data.df <- subset(fixation_data_scene.df, 
                             is_inside(fixation_data_scene.df, aoi$left, aoi$right, aoi$bottom, aoi$top))
  
  # get start and end times of all_data for the scene
  start_and_end_times <- get_seg_start_and_end_times(gazesample_data_scene.df)
  length <- start_and_end_times$end - start_and_end_times$start
  
  # stores the data by segment into vectors
  # internal_data_vector <- c()
  # fixation_data_vector <- c()
  # gazesample_data_vector <- c()
  # segs_length <- length(segment.names)
  # for(i in 1:segs_length) {
  # 
  #   internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
  #   fixation_data_vector[[i]] <- subset(fixation_data_scene.df, grepl(segment.names[i], scene))
  #   gazesample_data_vector[[i]] <- subset(gazesample_data_scene.df, grepl(segment.names[i], scene))
  # }
  
  ### numfixations ###
  feature_name <- paste(aoi_feature_name_root, "numfixations", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  numfixs <- nrow(internal_data.df)
  
  verify_equivalence(numfixs ,output_value, participant, a_scene, feature_name)
  
  ### proportionnum ###
  feature_name <- paste(aoi_feature_name_root, "proportionnum", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  internal_value <- numfixs / nrow(fixation_data_scene.df)
  
  verify_equivalence(internal_value ,output_value, participant, a_scene, feature_name)
  
  ### fixationrate ###
  feature_name <- paste(aoi_feature_name_root, "fixationrate", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  fix_duration <- sum(internal_data.df$fixationduration)
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- numfixs / fix_duration
  } else {
    
    internal_value <- 0
  }
  
  verify_equivalence(internal_value ,output_value, participant, a_scene, feature_name)
  
  ### totaltimespent ###
  feature_name <- paste(aoi_feature_name_root, "totaltimespent", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  verify_equivalence(fix_duration, output_value, participant, a_scene, feature_name)
  
  ### proportiontime ###
  feature_name <- paste(aoi_feature_name_root, "proportiontime", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  internal_value <- fix_duration / length
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### meanfixationduration ###
  feature_name <- paste(aoi_feature_name_root, "meanfixationduration", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- mean(internal_data.df$fixationduration)
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### stddevfixationduration ###
  feature_name <- paste(aoi_feature_name_root, "stddevfixationduration", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  if(nrow(internal_data.df) > 1){
    
    internal_value <- sd(internal_data.df$fixationduration)
  } else if(nrow(internal_data.df) == 1){
    
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
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### longestfixation ###
  feature_name <- paste(aoi_feature_name_root, "longestfixation", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- max(internal_data.df$fixationduration)
  } else{
    
    internal_value <- -1
  } 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetofirstfixation ###
  feature_name <- paste(aoi_feature_name_root, "timetofirstfixation", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- internal_data.df[1,]$timestamp - gazesample_data_scene.df[1,]$timestamp 
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetolastfixation ###
  feature_name <- paste(aoi_feature_name_root, "timetolastfixation", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  column_length <- nrow(internal_data.df)
  
  if(column_length != 0){
    
    internal_value <- 
      internal_data.df[column_length,]$timestamp - gazesample_data_scene.df[1,]$timestamp 
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### numtransfrom_ ###
  aoi1 <- list(x_left = aoi$left , x_right = aoi$right, y_bottom = aoi$bottom, y_top = aoi$top)
  total_count <- 0
  internal_values <- list()
  
  for(name in aois.data[,1]){
    
    aoi2 <- aois.data[aois.data[,1] == name,]
    feature_name <- paste(aoi_feature_name_root, "numtransfrom_", name, sep = "")
    output_value <- subset(emdat_output.df, select = feature_name)[1,]
    
    aoi2 <- list(x_left = aoi2$left , x_right = aoi2$right, y_bottom = aoi2$bottom, y_top = aoi2$top)
    
    internal_value <- trans_from(fixation_data_scene.df, aoi1, aoi2)
    internal_values[[name]] <- internal_value
    total_count <- total_count + internal_value
    
    verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  }
  
  ### proptransfrom_ ###
  for(name in aois.data[,1]){
    
    feature_name <- paste(aoi_feature_name_root, "proptransfrom_", name, sep = "")
    output_value <- subset(emdat_output.df, select = feature_name)[1,]
    
    if(total_count != 0){
      
      internal_value <- internal_values[[name]] / total_count
    } else{
      
      internal_value <- 0
    }
    
    verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  }
  
}

# This function checks the correctness of events
# LIST OF COLUMS TO TEST:

# numevents
# numrightclic
# rightclicrate
# numdoubleclic
# doubleclicrate
# numleftclic
# leftclicrate
# timetofirstdoubleclic
# timetofirstleftclic
# timetofirstrightclic

# Not tested; these are set to -1 in the emdat code:	
# timetolastdoubleclic	
# timetolastleftclic	
# timetolastrightclic	

check_aoi_eve <- function(emdat_output.df, 
                          participant, 
                          a_scene,
                          aoi_name,
                          aois.data,
                          segment.names,
                          events_data_scene.df,
                          gazesample_data_scene.df){
  
  ### set up the tests ###
  aoi <- aois.data[aois.data[,"aoi_name"] == aoi_name,]
  
  # preprends X for the aoi name, which is how R represents aoi names of grid2X2
  if(class(aoi_name) == "integer"){
    
    aoi_feature_name_root <- set_root_name(paste("X", aoi_name, sep = ""))
  } else {
    
    aoi_feature_name_root <- set_root_name(aoi_name)
  }
  
  # subsets for data points inside aoi 
  internal_data.df <- subset(events_data_scene.df,
                             grepl('MouseClick', event) &
                             as.numeric(as.character(x_coord)) > aoi$left &
                             as.numeric(as.character(x_coord)) <= aoi$right &
                             as.numeric(as.character(y_coord)) <= aoi$bottom &
                             as.numeric(as.character(y_coord)) > aoi$top)
  
  
  # get start and end times of all_data for the scene
  start_and_end_times <- get_seg_start_and_end_times(gazesample_data_scene.df)
  length <- start_and_end_times$end - start_and_end_times$start
  
  # stores the data by segment into vectors
  # internal_data_vector <- c()
  # events_data_vector <- c()
  # gazesample_data_vector <- c()
  # segs_length <- length(segment.names)
  # for(i in 1:segs_length) {
  # 
  #   internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
  #   gazesample_data_vector[[i]] <- subset(gazesample_data_scene.df, grepl(segment.names[i], scene))
  # }
  
  ### numevents ###
  feature_name <- paste(aoi_feature_name_root, "numevents", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  internal_value <- nrow(internal_data.df)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### numrightclic ###
  feature_name <- paste(aoi_feature_name_root, "numrightclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  rightclicks.df <- subset(internal_data.df, event=="RightMouseClick")
  internal_value <- nrow(rightclicks.df)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### rightclicrate ###
  feature_name <- paste(aoi_feature_name_root, "rightclicrate", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### numdoubleclic ###
  feature_name <- paste(aoi_feature_name_root, "numdoubleclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  clicks <- find_double_and_left_clicks(internal_data.df)
  
  internal_value <- clicks[1]
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### doubleclicrate ###
  feature_name <- paste(aoi_feature_name_root, "doubleclicrate", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,] 
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### numleftclic ###
  feature_name <- paste(aoi_feature_name_root, "numleftclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  internal_value <- clicks[2]
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### leftclicrate ###
  feature_name <- paste(aoi_feature_name_root, "leftclicrate", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetofirstdoubleclic ###
  feature_name <- paste(aoi_feature_name_root, "timetofirstdoubleclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  internal_value <- clicks[3]
  
  if(internal_value != -1){
    
    internal_value <- internal_value - start_and_end_times$start
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetofirstleftclic ###
  feature_name <- paste(aoi_feature_name_root, "timetofirstleftclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  internal_value <- clicks[4]
  
  if(internal_value != -1){
    
    internal_value <- internal_value - start_and_end_times$start
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetofirstrightclic ###
  feature_name <- paste(aoi_feature_name_root, "timetofirstrightclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  if(nrow(rightclicks.df) != 0){
    
    internal_value <- rightclicks.df[1,]$timestamp - start_and_end_times$start
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  # ### timetolastdoubleclic ###
  # feature_name <- paste(aoi_feature_name_root, "timetolastdoubleclic", sep = "")
  # output_value <- subset(emdat_output.df, select = feature_name)[1,]
  # 
  # verify_equivalence(-1, output_value, participant, a_scene, feature_name)
  # 
  # ### timetolastleftclic ###
  # feature_name <- paste(aoi_feature_name_root, "timetolastleftclic", sep = "")
  # output_value <- subset(emdat_output.df, select = feature_name)[1,]
  # 
  # verify_equivalence(-1, output_value, participant, a_scene, feature_name)
  # 
  # ### timetolastrightclic ###
  # feature_name <- paste(aoi_feature_name_root, "timetolastrightclic", sep = "")
  # output_value <- subset(emdat_output.df, select = feature_name)[1,]
  # 
  # verify_equivalence(-1, output_value, participant, a_scene, feature_name)
  
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
participants <- generate_participant_list(144:162)

# Run
# Note: last_participant refers to the last in the EMDAT output file used, not necessarily that
#       in the list of participants
run_part2Test(participants, aoi_file_name, "162b")


# #### To debug #####
# 
# # Runs tests on a given individual participant and scene
# 
# ##############
# part <- "147b"
# test_scene <- "Event_79"
# ##############
# 
# seg_file <- paste(seg_file_path, "P", part, ".seg", sep = "")
# aoi_file <- paste(aoi_file_path, aoi_file_name, ".aoi", sep = "")
# 
# readfiles_aoi_debug <- function(participant, seg_file, aoi_file, last_participant, a_scene){
# 
#   # reads the pertinent part of the features file for the given participant (*)
#   emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
# 
#   seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
# 
#   # extract aoi names
#   aoi_file.df <- read.csv(aoi_file, sep="\t", header = FALSE, col.names = c("aoi_name","TL","TR","BR", "BL"), stringsAsFactors = FALSE)
#   aoi.names <- aoi_file.df[, "aoi_name"]
# 
#   # store aoi names and boundaries in df
#   aois <- lapply(aoi.names, extract_aoi_coordinate, aoi_file.df= aoi_file.df)
#   aois.data <- Reduce(rbind2, aois)
# 
#   # reads in the internal EMDAT data files necessary for computing expecetd values,
#   # once for the given participant
#   fixation_data.df <- read.csv(paste(internal_data_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
#   events_data.df <- read.csv(paste(internal_data_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
#   gazesample_data.df <- read.csv(paste(internal_data_path, "EMDATinternaldata_gazesamples_", participant, ".csv", sep=""), sep=",")
# 
#   # this participant has None vlaues in x_ and y_coords; replace them with NA
#   # to handle those rows together with the other numerical entries without a warning message later
#   if(participant == "147b"){
#     events_data.df$x_coord <- replace(events_data.df$x_coord, which(events_data.df$x_coord == 'None'), NA)
#     events_data.df$y_coord <- replace(events_data.df$y_coord, which(events_data.df$y_coord == 'None'), NA)
#   }
# 
#   # extracts segments within a given scene
#   segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
# 
#   # reads the pertinent part of the file from (*) above for the given scene
#   emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
# 
#   # if-statement below guards aginst missing scenes in the files
#   if(nrow(emdat_export.df.scene) != 0) {
# 
#     # keeps all segments belonging to the scene in data frame format
#     gazesample_data_scene.df <- subset(gazesample_data.df, scene == a_scene)
#     fixation_data_scene.df <- subset(fixation_data.df, scene == a_scene)
#     events_data_scene.df <- subset(events_data.df, scene == a_scene)
# 
# 
#     if(nrow(fixation_data_scene.df) != 0 &
#        nrow(gazesample_data_scene.df) != 0){
# 
#       for(aoi_name in aoi.names){
# 
#         check_aoi_fix(emdat_export.df.scene,
#                       participant,
#                       a_scene,
#                       aoi_name,
#                       aois.data,
#                       segment.names,
#                       gazesample_data_scene.df,
#                       fixation_data_scene.df)
#       }
#     }
# 
#     if(nrow(gazesample_data_scene.df) != 0){
# 
#       for(aoi_name in aoi.names){
# 
#         check_aoi_eve(emdat_export.df.scene,
#                       participant,
#                       a_scene,
#                       aoi_name,
#                       aois.data,
#                       segment.names,
#                       events_data_scene.df,
#                       gazesample_data_scene.df)
#       }
#     }
#   }
#   report_success(participant, cumulative_counter)
# }
# 
# readfiles_aoi_debug(part, seg_file, aoi_file, "162b", test_scene)