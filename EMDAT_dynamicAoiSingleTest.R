source("EMDAT_testUtils.R")

### set up the tests by initializing varibales in the global scope ###
root <- "Part2_EMDATInternal_EMDATOutput/intervention_study_data/"
internal_data_path <- paste(root, "EMDAT_internal_data/", sep = "")
seg_file_path <- paste(root, "seg_files/", sep = "")
feature_files_path <- paste(root, "features/", sep = "")
aoi_file_path <- "Part1_TobiiV3Output_EMDATInternal/intervention_study_data/"

emdat_export_all.df <- read.csv(paste(feature_files_path, 
                                      "tobiiv3_sample_features_single_dynamic",  
                                      ".tsv", 
                                      sep=""), 
                                sep="\t")

Sc_ids <- as.character(emdat_export_all.df[,1])

cumulative_counter <- 0

### test scripts ###
readfiles_aoi <- function(participant, seg_file, aoi_file, last_participant){
  
  # reads the pertinent part of the features file for the given participant (*)
  emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
  
  # reads the aoi file with active time intervals indicated by '#' 
  aoi_file.df_temp <- read.csv(aoi_file, sep="\t", header = FALSE, col.names = c("aoi_name","TL","TR","BR", "BL"))
  
  # removes active time interval rows  
  aoi_file.df <- aoi_file.df_temp[aoi_file.df_temp$aoi_name != "#",]
  
  # stores aoi name and boundaries in list
  aoi <- extract_aoi_coordinate(aoi_file.df, "single")
  
  # extracts # <active intevals> 
  interval_vector <- extract_active_time_intervals(aoi_file.df_temp, aoi_file.df)
  
  # extracts scene names
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  scene.names <- unique(seg_file.df[,"scene"])
  
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

        checked_result1 <- check_aoi_fix(emdat_export.df.scene,
                                         participant,
                                         a_scene,
                                         segment.names,
                                         aoi_file.df,
                                         aoi,
                                         interval_vector,
                                         gazesample_data_scene.df,
                                         fixation_data_scene.df)

      }
      
      if(nrow(gazesample_data_scene.df) != 0){

        checked_result2 <- check_aoi_eve(emdat_export.df.scene,
                                         participant,
                                         a_scene,
                                         segment.names,
                                         aoi_file.df,
                                         aoi,
                                         interval_vector,
                                         gazesample_data_scene.df,
                                         events_data_scene.df)
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
# single_longestfixation
# single_timetofirstfixation
# single_timetolastfixation
# single_numtransfrom_single
# single_proptransfrom_single

### TODO ###
# single_stddevfixationduration

check_aoi_fix <- function(emdat_output.df, 
                          participant, 
                          a_scene, 
                          segment.names,
                          aoi_file.df,
                          aoi,
                          interval_vector,
                          gazesample_data_scene.df,
                          fixation_data_scene.df){
  
  ### set up the tests ###
  
  # gets start and end times of all_data for the scene (same as for seg in this case) 
  start_and_end_times <- get_seg_start_and_end_times(gazesample_data_scene.df)
  seg_start <- start_and_end_times$start
  seg_end <- start_and_end_times$end
  length <- seg_end - seg_start
  
  inactive <- TRUE
  
  # initializes a dataframe with the correct column names and types  
  fixation_data_scene.df_cumulative <- fixation_data_scene.df[0,]
  
  # this loop gets data inside the intersection of seg/scene and active intervals
  for(i in 1:length(interval_vector[[1]])){
      
    interval_start <- get_tuple_element(1, interval_vector[[1]][i])
    interval_end <- get_tuple_element(2, interval_vector[[1]][i])
    
    # seg time interval is subset of the active interval; keeps all points  
    if(interval_start <= seg_start && seg_end <= interval_end) {
      
      inactive <- FALSE
      fixation_data_scene.df_cumulative <- fixation_data_scene.df
      break   
    }
    
    # some intersection; keeps only those points inside  
    if(seg_start <= interval_end && interval_start <= seg_end){
        
      start <- max(seg_start, interval_start)
      end <- min(seg_end, interval_end)
        
      fixation_data_scene.df_temp <- fixation_data_scene.df[
                                       which(start <= fixation_data_scene.df$timestamp &
                                             fixation_data_scene.df$timestamp + 
                                             fixation_data_scene.df$fixationduration <= end),]
        
      fixation_data_scene.df_cumulative <- rbind(fixation_data_scene.df_cumulative, 
                                                 fixation_data_scene.df_temp) 
          
      inactive <- FALSE
    }
  }
  
  fixation_data_scene.df <- fixation_data_scene.df_cumulative
  
  # aoi inactive; yields default feature values
  if(inactive){
    
    test_dynamic_aoi_default(emdat_output.df,participant, a_scene, "fix")
    return()
  }
  
  # gets data inside aoi
  left <- aoi$left
  right <- aoi$right
  bottom <- aoi$bottom
  top <- aoi$top
  internal_data.df <- subset(fixation_data_scene.df, 
                             is_inside(fixation_data_scene.df, left, right, bottom, top))
  
  ### single_numfixations ###
  output_value <- subset(emdat_output.df, select = single_numfixations)[1,]
  numfixs <- nrow(internal_data.df)
  
  verify_equivalence(numfixs ,output_value, participant, a_scene, "single_numfixations")
  
  ### single_proportionnum ###
  output_value <- subset(emdat_output.df, select = single_proportionnum)[1,]
  
  if(nrow(fixation_data_scene.df) != 0){
    
    internal_value <- numfixs / nrow(fixation_data_scene.df)
  } else{
    
    internal_value <- 0
  }
  verify_equivalence(internal_value ,output_value, participant, a_scene, "single_proportionnum")
  
  ### single_fixationrate ###
  output_value <- subset(emdat_output.df, select = single_fixationrate)[1,]
  
  fix_duration <- sum(internal_data.df$fixationduration)
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- numfixs / fix_duration
  } else {
    
    internal_value <- 0
  }
  
  verify_equivalence(internal_value ,output_value, participant, a_scene, "single_fixationrate")
  
  ### single_totaltimespent ###
  output_value <- subset(emdat_output.df, select = single_totaltimespent)[1,]
  
  verify_equivalence(fix_duration, output_value, participant, a_scene, "single_totaltimespent")
  
  ### single_proportiontime ###
  output_value <- subset(emdat_output.df, select = single_proportiontime)[1,]
  internal_value <- fix_duration / length
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_proportiontime")
  
  ### single_meanfixationduration ###
  output_value <- subset(emdat_output.df, select = single_meanfixationduration)[1,]
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- mean(internal_data.df$fixationduration)
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_meanfixationduration")
  
  ## single_stddevfixationduration ###
  output_value <- subset(emdat_output.df, select = single_stddevfixationduration)[1,]

  if(nrow(internal_data.df) > 1){
    
    internal_value <- sd(internal_data.df$fixationduration)
  } else {
    
    internal_value <- 0.0
  }

  verify_equivalence(internal_value, output_value, participant, a_scene, "single_stddevfixationduration")
  
  ### single_longestfixation ###
  output_value <- subset(emdat_output.df, select = single_longestfixation)[1,]
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- max(internal_data.df$fixationduration)
  } else{
    
    internal_value <- -1
  } 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_longestfixation")
  
  ### single_timetofirstfixation ###
  output_value <- subset(emdat_output.df, select = single_timetofirstfixation)[1,]
  
  if(nrow(internal_data.df) != 0){
    
    internal_value <- internal_data.df[1,]$timestamp - gazesample_data_scene.df[1,]$timestamp 
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetofirstfixation")
  
  ### single_timetolastfixation ###
  output_value <- subset(emdat_output.df, select = single_timetolastfixation)[1,]
  
  column_length <- nrow(internal_data.df)
  
  if(column_length != 0){
    
    internal_value <- 
      internal_data.df[column_length,]$timestamp - gazesample_data_scene.df[1,]$timestamp 
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetolastfixation")
  
  ### single_numtransfrom_single ###
  output_value <- subset(emdat_output.df, select = single_numtransfrom_single)[1,]
  
  aoi1 <- list(x_left = left , x_right = right, y_bottom = bottom, y_top = top)
  aoi2 <- list(x_left = left , x_right = right, y_bottom = bottom, y_top = top)
  
  internal_value <- trans_from(fixation_data_scene.df, aoi1, aoi2)
  total_count <- internal_value
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numtransfrom_single")
  
  ### single_proptransfrom_single ###
  output_value <- subset(emdat_output.df, select = single_proptransfrom_single)[1,]
  
  if(total_count != 0){
    
    internal_value <- internal_value / total_count
  } else{
    internal_value <- 0
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_proptransfrom_single")
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
# single_timetolastdoubleclic	
# single_timetolastleftclic	
# single_timetolastrightclic	

check_aoi_eve <- function(emdat_output.df, 
                          participant, 
                          a_scene, 
                          segment.names,
                          aoi_file.df,
                          aoi,
                          interval_vector,
                          gazesample_data_scene.df,
                          events_data_scene.df){
  
  ### sets up the tests ###
  
  # gets start and end times of all_data for the scene (same as for seg in this case) 
  start_and_end_times <- get_seg_start_and_end_times(gazesample_data_scene.df)
  seg_start <- start_and_end_times$start
  seg_end <- start_and_end_times$end
  length <- seg_end - seg_start
  
  inactive <- TRUE
  
  # initializes a dataframe with the correct column names and types  
  events_data_scene.df_cumulative <- events_data_scene.df[0,]
  
  # this loop gets data inside the intersection of seg/scene and active intervals
  for(i in 1:length(interval_vector[[1]])){
    
    interval_start <- get_tuple_element(1, interval_vector[[1]][i])
    interval_end <- get_tuple_element(2, interval_vector[[1]][i])
    
    # seg time interval is subset of the active interval; keeps all points
    if(interval_start <= seg_start && seg_end <= interval_end) {
      
      inactive <- FALSE
      events_data_scene.df_cumulative <- events_data_scene.df
      break   
    }
    
    # some intersection; keeps only those points inside
    if(seg_start <= interval_end && interval_start <= seg_end){
      
      start <- max(seg_start, interval_start)
      end <- min(seg_end, interval_end)
      
      events_data_scene.df_temp <- events_data_scene.df[
        which(start <= events_data_scene.df$timestamp &
              events_data_scene.df$timestamp <= end),]
      
      events_data_scene.df_cumulative <- rbind(events_data_scene.df_cumulative, events_data_scene.df_temp) 
      
      inactive <- FALSE
    }
  }
  
  events_data_scene.df <- events_data_scene.df_cumulative
  
  # aoi inactive; yields default feature values   
  if(inactive){
    
    test_dynamic_aoi_default(emdat_output.df,participant, a_scene, "event")
    return()
  }
  
  # gets data inside aoi
  internal_data.df <- subset(events_data_scene.df,
                             grepl('MouseClick', event) &
                             as.numeric(as.character(x_coord)) > aoi$left &
                             as.numeric(as.character(x_coord)) <= aoi$right &
                             as.numeric(as.character(y_coord)) <= aoi$bottom &
                             as.numeric(as.character(y_coord)) > aoi$top)
  
  ### single_numevents ###
  output_value <- subset(emdat_output.df, select = single_numevents)[1,]
  
  internal_value <- nrow(internal_data.df)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numevents")
  
  ### single_numrightclic ###
  output_value <- subset(emdat_output.df, select = single_numrightclic)[1,]
  
  rightclicks.df <- subset(internal_data.df, event=="RightMouseClick")
  internal_value <- nrow(rightclicks.df)
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numrightclic")
  
  ### single_rightclicrate ###
  output_value <- subset(emdat_output.df, select = single_rightclicrate)[1,]
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_rightclicrate")
  
  ### single_numdoubleclic ###
  output_value <- subset(emdat_output.df, select = single_numdoubleclic)[1,]
  
  clicks <- find_double_and_left_clicks(internal_data.df)
  
  internal_value <- clicks[1]
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numdoubleclic")
  
  ### single_doubleclicrate ###
  output_value <- subset(emdat_output.df, select = single_doubleclicrate)[1,] 
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_doubleclicrate")
  
  ### single_numleftclic ###
  output_value <- subset(emdat_output.df, select = single_numleftclic)[1,]
  
  internal_value <- clicks[2]
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_numleftclic")
  
  ### single_leftclicrate ###
  output_value <- subset(emdat_output.df, select = single_leftclicrate)[1,]
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_leftclicrate")
  
  ### single_timetofirstdoubleclic ###
  output_value <- subset(emdat_output.df, select = single_timetofirstdoubleclic)[1,]
  
  internal_value <- clicks[3]
  
  if(internal_value != -1){
    
    internal_value <- internal_value - start_and_end_times$start
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetofirstdoubleclic")
  
  ### single_timetofirstleftclic ###
  output_value <- subset(emdat_output.df, select = single_timetofirstleftclic)[1,]
  
  internal_value <- clicks[4]
  
  if(internal_value != -1){
    
    internal_value <- internal_value - start_and_end_times$start
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetofirstleftclic")
  
  ### single_timetofirstrightclic ###
  output_value <- subset(emdat_output.df, select = single_timetofirstrightclic)[1,]
  
  if(nrow(rightclicks.df) != 0){
    
    internal_value <- rightclicks.df[1,]$timestamp - start_and_end_times$start
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetofirstrightclic")
  
  ### single_timetolastdoubleclic ###
  output_value <- subset(emdat_output.df, select = single_timetolastdoubleclic)[1,]
  
  internal_value <- clicks[5]
  
  if(internal_value != -1){
    
    internal_value <- internal_value - start_and_end_times$start
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetolastdoubleclic")
  
  ### single_timetolastleftclic ###
  output_value <- subset(emdat_output.df, select = single_timetolastleftclic)[1,]
  
  internal_value <- clicks[6]
  
  if(internal_value != -1){
    
    internal_value <- internal_value - start_and_end_times$start
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetolastleftclic")
  
  ### single_timetolastrightclic ###
  output_value <- subset(emdat_output.df, select = single_timetolastrightclic)[1,]
  rightclick_counts <- nrow(rightclicks.df)
  
  if(rightclick_counts != 0){
    
    internal_value <- rightclicks.df[rightclick_counts,]$timestamp - start_and_end_times$start
  } else{
    
    internal_value <- -1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "single_timetolastrightclic")
}
##########################################################################################

# When called, commences the tests for the given list of participants
run_aoiTest <- function(participants, aoi_file_name, last_participant){
  
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
run_aoiTest(participants, "single_aoi_dynamic" , "162b")  

#### To debug #####

# Runs tests on a given individual participant and scene

# ##############
# part <- "105a"
# test_scene <- "Event_54"
# ##############
# 
# seg_file <- paste(seg_file_path, "P", part, ".seg", sep = "")
# aoi_file <- paste(aoi_file_path, "single_aoi_dynamic", ".aoi", sep = "")
# 
# readfiles_aoi_debug <- function(participant, seg_file, aoi_file, last_participant, a_scene){
# 
#   # reads the pertinent part of the features file for the given participant (*)
#   emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
# 
#   # reads the aoi file with active time intervals indicated by '#'
#   aoi_file.df_temp <- read.csv(aoi_file, sep="\t", header = FALSE, col.names = c("aoi_name","TL","TR","BR", "BL"))
# 
#   # removes active time interval rows
#   aoi_file.df <- aoi_file.df_temp[aoi_file.df_temp$aoi_name != "#",]
# 
#   # The loop below extracts active time intervals for each row (aoi) in aoi_file_df and also mark
#   # always active aoi with an empty string, "". The information is stored in the list interval_vector
#   names_temp <- aoi_file.df_temp$aoi_name
#   names <- aoi_file.df$aoi_name
#   interval_vector <- list()
# 
#   for(name in names) {
# 
#     if(names_temp[which(names_temp == name) + 1] == "#"){
# 
#       row.df <- aoi_file.df_temp[which(names_temp == name) +1,]
#       row_vector <- as.vector(t(row.df))[c(2:5)]
#       interval_vector[[which(names == name)]] <- row_vector[which(row_vector != "")]
#     } else {
# 
#       interval_vector[[which(names == name)]] <- ""
#     }
#   }
# 
#   seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
# 
#   # reads in the internal EMDAT data files necessary for computing expecetd values,
#   # once for the given participant
#   fixation_data.df <- read.csv(paste(internal_data_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
#   events_data.df <- read.csv(paste(internal_data_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
#   gazesample_data.df <- read.csv(paste(internal_data_path, "EMDATinternaldata_gazesamples_", participant, ".csv", sep=""), sep=",")
# 
#   # replaces None vlaue in x_ and y_coords with NA to elegantly handle inequality comparisons later
#   # in check_aoi_eve
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
#       nrow(gazesample_data_scene.df) != 0){
# 
#       check_aoi_fix(emdat_export.df.scene,
#                     participant,
#                     a_scene,
#                     segment.names,
#                     aoi_file.df,
#                     interval_vector,
#                     gazesample_data_scene.df,
#                     fixation_data_scene.df)
#     }
# 
#     # if(nrow(events_data_scene.df) != 0 &
#     #    nrow(gazesample_data_scene.df) != 0){
#     #
#     #    check_aoi_eve(emdat_export.df.scene,
#     #                  participant,
#     #                  a_scene,
#     #                  segment.names,
#     #                  aoi_file.df,
#     #                  events_data_scene.df,
#     #                  gazesample_data_scene.df)
#     # }
#   }
#   report_success(participant, cumulative_counter)
# }
# 
# readfiles_aoi_debug(part, seg_file, aoi_file, "162b", test_scene)