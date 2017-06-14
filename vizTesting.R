### TEST SCRIPT ###

source("EMDAT_testUtils.R")

root_path <- "Part2_EMDATInternal_EMDATOutput/new_data/"
aoi_file_path <- "Part1_TobiiV3Output_EMDATInternal/new_data/"

emdat_export_all.df <- read.csv(paste(root_path, 
                                      "tobiiv3_sample_features",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")

Sc_ids <- as.character(emdat_export_all.df[,1])

cumulative_counter <- 0

readfiles_aoi <- function(participant, seg_file, aoi_file, last_participant){
  
  # reads the pertinent part of the features file for the given participant (*)
  emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
  
  # extracts scene names
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  scene.names <- unique(seg_file.df[,"scene"])
  
  # extract aoi names
  aoi_file.df <- read.csv(aoi_file, sep="\t", header = FALSE, col.names = c("aoi_name","TL","TR","BR", "BL"), stringsAsFactors = FALSE)
  aoi.names <- aoi_file.df[, "aoi_name"]
  
  # store aoi names and boundaries in df 
  aois <- lapply(aoi.names, extract_aoi_coordinate, aoi_file.df= aoi_file.df)
  aois.data <- Reduce(rbind2, aois)
  
  # reads in the internal EMDAT data files necessary for computing expecetd values, 
  # once for the given participant 
  fixation_data.df <- read.csv(paste(root_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
  events_data.df <- read.csv(paste(root_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
  gazesample_data.df <- read.csv(paste(root_path, "EMDATinternaldata_gazesamples_", participant, ".csv", sep=""), sep=",")
  
  # this participant has None vlaues in x_ and y_coords; replace them with NA
  # to handle those rows together with the other numerical entries without a warning message later    
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

# single_numfixations
# single_proportionnum
# single_fixationrate
# single_totaltimespent
# single_proportiontime
# single_meanfixationduration
# single_stddevfixationduration
# single_longestfixation
# single_timetofirstfixation
# single_timetolastfixation
# single_numtransfrom_single
# single_proptransfrom_single

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
  aoi_feature_name_root <- paste(aoi_name, "_", sep = "")
  
  # get data inside aoi
  # internal_data.df <- subset(fixation_data_scene.df, 
  #                            mappedfixationpointx > aoi$left &
  #                            mappedfixationpointx <= aoi$right &
  #                            mappedfixationpointy <= aoi$bottom &   
  #                            mappedfixationpointy > aoi$top)
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
  internal_values <- list(nrow(aois.data))
  
  for(name in aois.data[,1]){
    
    aoi2 <- aois.data[aois.data[,"aoi_name"] == name,]
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
                          aoi_name,
                          aois.data,
                          segment.names,
                          events_data_scene.df,
                          gazesample_data_scene.df){
  
  ### set up the tests ###
  
  aoi <- aois.data[aois.data[,"aoi_name"] == aoi_name,]
  aoi_feature_name_root <- paste(aoi_name, "_", sep = "")
  
  
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
}
##########################################################################################

# When called, commences the part2 tests for the given list of participants
# last_participant refers to the last in the given study, not necessarily that
# in the list of participants
run_part2Test <- function(participants, aoi_file_name, last_participant){
  
  aoi_file <- paste(aoi_file_path, aoi_file_name, ".aoi", sep = "")
  
  for(i in 1:length(participants)){
    
    participant <- participants[i]
    readfiles_aoi(participant,
                  paste(root_path, "SegFiles/P", participant, ".seg", sep = ""),
                  aoi_file, 
                  last_participant)
  }
  writeLines(paste("####### cumulative total number of tests run: ", cumulative_counter, " #######"))
  cumulative_counter <<- 0
}

# ##### To Run #####
# 
# # Set up the tests: choose the range of particpants to run the tests on
# 
participants <- generate_participant_list(101:101)
# 
# # Run
# # Note: second argument takes the last participant of the study, not necessarily the
# #       last element in the list of participants given to the first argument
# 
run_part2Test(participants, "viz-specific" , "101b")


#### To debug #####

# Runs tests on a given individual participant and scene

# ##############
# part <- "147b"
# test_scene <- "Event_79"
# ##############
# 
# seg_file <- paste(root_path, "SegFiles/P", part, ".seg", sep = "")
# aoi_file <- paste(aoi_file_path, "single_aoi", ".aoi", sep = "")
# 
# readfiles_aoi_debug <- function(participant, seg_file, aoi_file, last_participant, a_scene){
# 
#   # reads the pertinent part of the features file for the given participant (*)
#   emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
# 
#   aoi_file.df <- read.csv(aoi_file, sep="\t", header = FALSE, col.names = c("aoi_name","TL","TR","BR", "BL"))
# 
#   top_left <- aoi_file.df$TL
#   top_right <- aoi_file.df$TR
#   bottom_right <- aoi_file.df$BR
#   bottom_left <-  aoi_file.df$BL
# 
#   # extracts scene names
#   seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
#   scene.names <- unique(seg_file.df[,"scene"])
# 
#   # reads in the internal EMDAT data files necessary for computing expecetd values,
#   # once for the given participant
#   fixation_data.df <- read.csv(paste(root_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
#   events_data.df <- read.csv(paste(root_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
#   gazesample_data.df <- read.csv(paste(root_path, "EMDATinternaldata_gazesamples_", participant, ".csv", sep=""), sep=",")
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
#     # if(nrow(fixation_data_scene.df) != 0 &
#     #    nrow(gazesample_data_scene.df) != 0){
#     #
#     #   checked_result1 <- check_aoi_fix(emdat_export.df.scene,
#     #                                    participant,
#     #                                    a_scene,
#     #                                    segment.names,
#     #                                    aoi_file.df,
#     #                                    gazesample_data_scene.df,
#     #                                    fixation_data_scene.df)
#     #
#     # }
# 
#     if(nrow(events_data_scene.df) != 0 &
#        nrow(gazesample_data_scene.df) != 0){
# 
#       checked_result2 <- check_aoi_eve(emdat_export.df.scene,
#                                        participant,
#                                        a_scene,
#                                        segment.names,
#                                        aoi_file.df,
#                                        events_data_scene.df,
#                                        gazesample_data_scene.df)
#     }
#   }
#   report_success(participant, cumulative_counter)
# }
# 
# readfiles_aoi_debug(part, seg_file, aoi_file, "162b", test_scene)