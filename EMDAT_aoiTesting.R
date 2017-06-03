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
  
  aoi_file.df <- read.csv(aoi_file, sep="\t", header = FALSE, col.names = c("aoi_name","TL","TR","BR", "BL"))
  
  top_left <- aoi_file.df$TL
  top_right <- aoi_file.df$TR
  bottom_right <- aoi_file.df$BR
  bottom_left <-  aoi_file.df$BL
  
  # extracts scene names
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  scene.names <- unique(seg_file.df[,"scene"])
  
  # reads in the needed internal EMDAT data files once for the given participant 
  fixation_data.df <- read.csv(paste(root_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
  events_data.df <- read.csv(paste(root_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
  
  # loops over the scenes
  for (a_scene in scene.names) {
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    # reads the pertinent part of the file from (*) above for the given scene 
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    # if-statements below guards aginst missing scenes in the files   
    if(nrow(emdat_export.df.scene) != 0) {
      
      # keeps all segments belonging to the scene in data frame format 
      fixation_data_scene.df <- subset(fixation_data.df, scene == a_scene)
      aoi_fixation_data_scene.df <- subset(fixation_data_scene.df, 
                                             mappedfixationpointx > get_tuple_element(1, top_left) &
                                             mappedfixationpointx < get_tuple_element(1, top_right) &
                                             mappedfixationpointy > get_tuple_element(2, bottom_right) &   
                                             mappedfixationpointy < get_tuple_element(2, top_right))
                           
      events_data_scene.df <- subset(events_data.df, scene == a_scene)
      
      if(nrow(fixation_data_scene.df) != 0){
        
        checked_result1 <- check_aoi_fix(emdat_export.df.scene,
                                                 participant,
                                                 a_scene,
                                                 segment.names,
                                                 fixation_data_scene.df,
                                                 aoi_fixation_data_scene.df)
                                                 
      }
      
      # if(nrow(events_data_scene.df) != 0 &
      #    nrow(gazesample_data_scene.df) != 0){
      #   
      #   checked_result2 <- check_aoi_eve(emdat_export.df.scene,
      #                                            participant,
      #                                            a_scene,
      #                                            segment.names,
      #                                            events_data_scene.df,
      #                                            gazesample_data_scene.df)
      # }
      
    }
  }
  report_success(participant, cumulative_counter)
}



# This function checks the correctness of fixations
# LIST OF COLUMS TO TEST:


# TODO:
# single_fixationrate	
# single_numfixations
# single_longestfixation	
# single_meanfixationduration
# single_stddevfixationduration
# single_timetofirstfixation
# single_timetolastfixation	

check_aoi_fix <- function(emdat_output.df, 
                                  participant, 
                                  a_scene, 
                                  segment.names,
                                  fixation_data_scene.df,
                                  aoi_fixation_data_scene.df){
  
  # ### set up the tests ###
  internal_data.df <- aoi_fixation_data_scene.df
  
  segs_length <- length(segment.names)
  
  # stores the data by segment into vectors 
  aoi_fixation_data_vector <- c()
  fixation_data_vector <- c()
  for(i in 1:segs_length) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    fixation_data_vector[[i]] <- subset(fixation_data_scene.df, grepl(segment.names[i], scene))
  }
  
  ### single_numfixations ###
  output_value <- subset(emdat_output.df, select=single_numfixations)[1,]
  verify_equivalence(nrow(internal_data.df),output_value, participant, a_scene, "single_numfixations")
  
  # ### sumfixationduration ###
  # internal_value <- sum(subset(internal_data.df, select=fixationduration))
  # output_value <- subset(emdat_output.df, select=sumfixationduration)[1,]
  # verify_equivalence(internal_value, output_value, participant, a_scene, "sumfixationduration")
  # 
  # ### stddevfixationduration ###
  # internal_value <- sd(subset(internal_data.df, select=fixationduration)$fixationduration)
  # output_value <- subset(emdat_output.df, select=stddevfixationduration)[1,]
  # verify_equivalence(internal_value, output_value, participant, a_scene, "stddevfixationduration")
  # 
  # ### meanfixationduration ###
  # internal_value <- mean(subset(internal_data.df, select=fixationduration)$fixationduration)
  # output_value <- subset(emdat_output.df, select=meanfixationduration)[1,]
  # verify_equivalence(internal_value, output_value, participant, a_scene, "meanfixationduration")
  # 
  # ### fixationrate ###
  # output_value <- subset(emdat_output.df, select=fixationrate)[1,]
  # scene_length <- compute_scene_length(segment.names, gazesample_data_vector)
  # internal_value <- nrow(internal_data.df) / scene_length
  # 
  # verify_equivalence(internal_value, output_value, participant, a_scene, "fixationrate")
  # 
  # ### numsegments ###
  # output_value <- subset(emdat_output.df, select=numsegments)[1,]
  # internal_value <- length(segment.names)
  # verify_equivalence(internal_value, output_value, participant, a_scene, "numsegments")
  # 
  # ### sumpathdistance ###
  # ### meanpathdistance ###
  # ### eyemovementvelocity ###  
  # output_sum <- subset(emdat_output.df, select=sumpathdistance)[1,]
  # output_mean <- subset(emdat_output.df, select = meanpathdistance)[1,]
  # output_velocity <- subset(emdat_output.df, select=eyemovementvelocity)[1,]
  # 
  # results <- find_sum_mean_rate(internal_data_vector, 
  #                               find_path_length_vector, segs_length, scene_length)
  # 
  # verify_equivalence(results$sum, output_sum, participant, a_scene, "sumpathdistance")
  # verify_equivalence(results$mean, output_mean, participant, a_scene, "meanpathdistance")
  # verify_equivalence(results$rate, output_velocity, participant, a_scene, "eyemovementvelocity")
  # 
  # ### stddevpathdistance ###
  # output_value <- subset(emdat_output.df, select = stddevpathdistance)[1,]
  # internal_value <- find_fixation_sd(results$data_storage, results$mean, segs_length)
  # 
  # verify_equivalence(internal_value, output_value, participant, a_scene, "stddevpathdistance")
  # 
  # ### sumabspathangles ###
  # ### meanabspathangles ###
  # ### abspathanglesrate ###  
  # output_sum <- subset(emdat_output.df, select = sumabspathangles)[1,]
  # output_mean <- subset(emdat_output.df, select = meanabspathangles)[1,]
  # output_rate <- subset(emdat_output.df, select = abspathanglesrate)[1,]
  # 
  # results <- find_sum_mean_rate(internal_data_vector, 
  #                               find_abs_angle_vector, segs_length, scene_length)
  # 
  # verify_equivalence(results$sum, output_sum, participant, a_scene, "sumabspathangles")
  # verify_equivalence(results$mean, output_mean, participant, a_scene, "meanabspathangles")
  # internal_value <- results$rate
  # verify_equivalence(internal_value, output_rate, participant, a_scene, "abspathanglesrate")
  # 
  # ### stddevabspathangles ###
  # output_value <- subset(emdat_output.df, select = stddevabspathangles)[1,]
  # internal_value <- find_fixation_sd(results$data_storage, results$mean, segs_length)
  # 
  # verify_equivalence(internal_value, output_value, participant, a_scene, "stddevabspathangles")
  # 
  # ### fixationsaccadetimeratio ###
  # output_value <- subset(emdat_output.df, select=fixationsaccadetimeratio)[1,]
  # numerator <- 0
  # 
  # for(i in 1:segs_length){
  #   
  #   fix_sum <- sum(subset(internal_data_vector[[i]], select=fixationduration))
  #   sac_sum <- sum(subset(saccade_data_vector[[i]], select=saccadeduration))
  #   numerator <- numerator + fix_sum/sac_sum
  # }
  # internal_value <- numerator/segs_length 
  # 
  # verify_equivalence(internal_value, output_value, participant, a_scene, "fixationsaccadetimeratio")
  # 
  # ### sumrelpathangles ###
  # ### meanrelpathangles ###
  # ### relpathanglesrate ###  
  # output_sum <- subset(emdat_output.df, select = sumrelpathangles)[1,]
  # output_mean <- subset(emdat_output.df, select = meanrelpathangles)[1,]
  # output_rate <- subset(emdat_output.df, select = relpathanglesrate)[1,]
  # 
  # results <- find_sum_mean_rate(internal_data_vector, 
  #                               find_rel_angle_vector, segs_length, scene_length)
  # 
  # verify_equivalence(results$sum, output_sum, participant, a_scene, "sumrelpathangles")
  # verify_equivalence(results$mean, output_mean, participant, a_scene, "meanrelpathangles")
  # verify_equivalence(results$rate, output_rate, participant, a_scene, "relpathanglesrate")
  # 
  # ### stddevrelpathangles ###
  # output_value <- subset(emdat_output.df, select = stddevrelpathangles)[1,]
  # internal_value <- find_fixation_sd(results$data_storage, results$mean, segs_length)
  # 
  # verify_equivalence(internal_value, output_value, participant, a_scene, "stddevrelpathangles")
}


# This function checks the correctness of events
# LIST OF COLUMS TO TEST:


check_aoi_eve <- function(emdat_output.df, 
                                  participant, 
                                  a_scene, 
                                  segment.names,
                                  events_data_scene.df,
                                  gazesample_data_scene.df){
  
  ### set up the tests ###
  internal_data.df <- events_data_scene.df
  gazesample_data.df <- gazesample_data_scene.df
  segs_length <- length(segment.names)
  
  # stores the data by segment into vectors
  internal_data_vector <- c()
  gazesample_data_vector <- c()
  for(i in 1:segs_length) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data.df, grepl(segment.names[i], scene))
  }
  
  scene_length <- compute_scene_length(segment.names, gazesample_data_vector)
  
  ### numdoubleclic ###
  output_value <- subset(emdat_output.df, select=numdoubleclic)[1,]
  double_clicks <- 0
  left_clicks <- 0
  
  for(i in 1:segs_length){
    
    clicks <- find_double_and_left_clicks(internal_data_vector[[i]])
    double_clicks <- double_clicks + clicks[1]
    left_clicks <- left_clicks + clicks[2]
    if(i == 1){
      first_double_click <- clicks[3]
      first_left_click <- clicks[4]
    }
  }
  internal_value <- double_clicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numdoubleclic")
  
  ### timetofirstdoubleclic ###
  output_value <- subset(emdat_output.df, select=timetofirstdoubleclic)[1,]
  internal_value <- first_double_click
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "timetofirstdoubleclic")
  
  ### doubleclicrate ###
  output_value <- subset(emdat_output.df, select=doubleclicrate)[1,]
  internal_value <- double_clicks / scene_length
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "doubleclicrate")
  
  ### numleftclic ###
  output_value <- subset(emdat_output.df, select=numleftclic)[1,]
  internal_value <- left_clicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numleftclic")
  
  ### timetofirstleftclic ###
  output_value <- subset(emdat_output.df, select=timetofirstleftclic)[1,]
  internal_value <- first_left_click
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "timetofirstleftclic")
  
  ### leftclicrate ###
  output_value <- subset(emdat_output.df, select=leftclicrate)[1,]
  internal_value <- left_clicks / scene_length
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "leftclicrate")  
  
  ### numkeypressed ###  
  output_value <- subset(emdat_output.df, select=numkeypressed)[1,]
  keypress.df <- subset(internal_data.df, event=="KeyPress")
  
  keypress <- nrow(keypress.df)
  
  internal_value <- keypress
  verify_equivalence(internal_value, output_value, participant, a_scene, "numkeypressed")
  
  ### timetofirstkeypressed ###
  output_value <- subset(emdat_output.df, select=timetofirstkeypressed)[1,]
  first_keypress.df <- subset(internal_data_vector[[1]], event=="KeyPress")
  
  if(nrow(first_keypress.df) == 0){
    first_keypressed <- -1
  }else{
    first_keypressed <- subset(first_keypress.df, select=timestamp)[1,]
  }
  internal_value <- first_keypressed
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "timetofirstkeypressed")
  
  ### keypressedrate ###
  output_value <- subset(emdat_output.df, select=keypressedrate)[1,]
  internal_value <- keypress / scene_length 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "keypressedrate")
  
  ### numrightclic ###  
  output_value <- subset(emdat_output.df, select=numrightclic)[1,]
  rightclicks.df <- subset(internal_data.df, event=="RightMouseClick")
  
  rightclicks <- nrow(rightclicks.df)
  
  internal_value <- rightclicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numrightclic")
  
  ### timetofirstrightclic ###
  output_value <- subset(emdat_output.df, select=timetofirstrightclic)[1,]
  first_rightclick.df <- subset(internal_data_vector[[1]], event=="RightMouseClick")
  
  if(nrow(first_rightclick.df) == 0){
    first_rightclick <- -1
  }else{
    first_rightclick <- subset(first_rightclick.df, select=timestamp)[1,]
  }
  internal_value <- first_rightclick
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "timetofirstrightclic")
  
  ### rightclicrate ###
  output_value <- subset(emdat_output.df, select=rightclicrate)[1,]
  internal_value <- rightclicks / scene_length 
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "rightclicrate")
  
  ### numevents ###
  output_value <- subset(emdat_output.df, select=numevents)[1,]
  internal_value <- double_clicks + left_clicks + keypress + rightclicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, "numevents")
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

##### To Run #####

# Set up the tests: choose the range of particpants to run the tests on

participants <- generate_participant_list(101:101)

# Run
# Note: second argument takes the last participant of the study, not necessarily the
#       last element in the list of participants given to the first argument

run_part2Test(participants, "single_aoi" , "101b")


#### To debug #####

# Runs tests on a given individual participant and scene

# path <- paste(root_path, "SegFiles/P", "140a", ".seg", sep = "")
# 
# readfiles_part2_debug <- function(participant, seg_file, last_participant, a_scene){
# 
#   emdat_export.df <- get_features_df_for_participant(emdat_export_all.df, participant, Sc_ids, last_participant)
#   seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
# 
#   segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
#   emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
# 
#   fixation_data.df <- read.csv(paste(root_path,"EMDATinternaldata_fixations_", participant, ".csv", sep=""), sep=",")
#   gazesample_data.df <- read.csv(paste(root_path, "EMDATinternaldata_gazesamples_", participant, ".csv", sep=""), sep=",")
#   saccade_data.df <- read.csv(paste(root_path, "EMDATinternaldata_saccades_", participant, ".csv", sep=""), sep=",")
#   events_data.df <- read.csv(paste(root_path, "EMDATinternaldata_events_", participant, ".csv", sep=""), sep=",")
# 
#   fixation_data_scene.df <- subset(fixation_data.df, scene == a_scene)
#   gazesample_data_scene.df <- subset(gazesample_data.df, scene == a_scene)
#   saccade_data_scene.df <- subset(saccade_data.df, scene == a_scene)
#   events_data_scene.df <- subset(events_data.df, scene == a_scene)
# 
#   checked_result1 <- check_correctness_fix(emdat_export.df.scene,
#                                            participant,
#                                            a_scene,
#                                            segment.names,
#                                            fixation_data_scene.df,
#                                            gazesample_data_scene.df,
#                                            saccade_data_scene.df)
# 
#   # checked_result2 <- check_correctness_sac(emdat_export.df.scene,
#   #                                          participant,
#   #                                          a_scene,
#   #                                          segment.names,
#   #                                          saccade_data_scene.df)
# 
#   # checked_result3 <- check_correctness_eve(emdat_export.df.scene,
#   #                                          participant,
#   #                                          a_scene,
#   #                                          segment.names,
#   #                                          events_data_scene.df,
#   #                                          gazesample_data_scene.df)
#   #
#   # checked_result4 <- check_correctness_gazesample(emdat_export.df.scene,
#   #                                                 participant,
#   #                                                 a_scene,
#   #                                                 segment.names,
#   #                                                 gazesample_data_scene.df)
# }

#readfiles_part2_debug("140a", path, "162b", "Event_38")