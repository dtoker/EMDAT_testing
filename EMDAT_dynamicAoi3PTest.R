source("EMDAT_testUtils.R")

### set up the tests by initializing varibales in the global scope ###
root <- "Part2_EMDATInternal_EMDATOutput/three_parts_study_data/"
internal_data_path <- paste(root, "EMDAT_internal_data/", sep = "")
seg_file_path <- paste(root, "seg_files/", sep = "")
feature_files_path <- paste(root, "features/", sep = "")
aoi_file_path <- "Part1_TobiiV3Output_EMDATInternal/three_parts_study_data/"

emdat_export_all.df <- read.csv(paste(feature_files_path, 
                                      "tobiiv3_sample_features_dynamic_aoi_3P",
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
  seg_file.df <- read.csv(seg_file, sep="\t", 
                          header = FALSE, 
                          col.names = c("scene","segment","start","end"))
  scene.names <- unique(seg_file.df[,"scene"])
  
  # reads the aoi file with active time intervals indicated by '#'
  aoi_file.df_temp <- read.csv(aoi_file, sep="\t", 
                               header = FALSE, 
                               col.names = c("aoi_name","TL","TR","BR", "BL"), 
                               stringsAsFactors = FALSE)
  
  # removes active time interval rows  
  aoi_file.df <- aoi_file.df_temp[aoi_file.df_temp$aoi_name != "#",]
  
  # stores aoi names and boundaries in df
  aoi.names <- aoi_file.df[, "aoi_name"]
  aois <- lapply(aoi.names, extract_aoi_coordinate, aoi_file.df= aoi_file.df)
  aois.data <- Reduce(rbind2, aois)
  
  # extracts # <active intevals> 
  interval_vector <- extract_active_time_intervals(aoi_file.df_temp, aoi_file.df)
  
  # reads in the internal EMDAT data files necessary for computing expecetd values, 
  # once for the given participant 
  fixation_data.df <- read.csv(paste(internal_data_path,"EMDATdata_fix_P", participant, ".tsv", sep=""), sep="\t")
  events_data.df <- read.csv(paste(internal_data_path, "EMDATdata_eve_P", participant, ".tsv", sep=""), sep="\t")
  gazesample_data.df <- read.csv(paste(internal_data_path, "EMDATdata_gazesample_P", participant, ".tsv", sep=""), sep="\t")
  
  # replaces None vlaue in x_ and y_coords with NA to elegantly handle inequality comparisons later
  # in check_aoi_eve 
  events_data.df$x_coord <- replace(events_data.df$x_coord, which(events_data.df$x_coord == 'None'), NA)
  events_data.df$y_coord <- replace(events_data.df$y_coord, which(events_data.df$y_coord == 'None'), NA)
  
  # loops over the scenes
  for (a_scene in scene.names) {
    
    # extracts segments within a given scene
    segment.names <- unique(subset(seg_file.df, scene==a_scene)[,"segment"])
    
    # reads the pertinent part of the file from (*) above for the given scene 
    emdat_export.df.scene <- subset(emdat_export.df, Sc_id == a_scene)
    
    # if-statement below guards aginst missing scenes in the files   
    if(nrow(emdat_export.df.scene) != 0) {
      
      # keeps all segments belonging to the scene in data frame format
      gazesample_data_scene.df <- subset(gazesample_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
      fixation_data_scene.df <- subset(fixation_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
      events_data_scene.df <- subset(events_data.df, grepl(a_scene, scene) & !grepl(participant, scene))
      
      if(nrow(fixation_data_scene.df) != 0 &
         nrow(gazesample_data_scene.df) != 0){

        for(aoi_name in aoi.names){

          active_intervals <- interval_vector[which(aoi.names == aoi_name)]

          check_aoi_fix(emdat_export.df.scene,
                        participant,
                        a_scene,
                        aoi_name,
                        aois.data,
                        active_intervals,
                        segment.names,
                        gazesample_data_scene.df,
                        fixation_data_scene.df)
        }
      }
      
      if(nrow(gazesample_data_scene.df) != 0){

        for(aoi_name in aoi.names){

          active_intervals <- interval_vector[which(aoi.names == aoi_name)]

          check_aoi_eve(emdat_export.df.scene,
                        participant,
                        a_scene,
                        aoi_name,
                        aois.data,
                        active_intervals,
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
# fixationrate
# totaltimespent
# proportiontime
# meanfixationduration
# longestfixation
# timetofirstfixation
# timetolastfixation
# numtransfrom
# proptransfrom
# stddevfixationduration 
# proportionnum

check_aoi_fix <- function(emdat_output.df, 
                          participant, 
                          a_scene,
                          aoi_name,
                          aois.data,
                          active_intervals,
                          segment.names,
                          gazesample_data_scene.df,
                          fixation_data_scene.df){
  
  ### set up the tests ###
  aoi <- aois.data[aois.data[,"aoi_name"] == aoi_name,]
  aoi_feature_name_root <- set_root_name(aoi_name)
  
  scene_numfix <- nrow(fixation_data_scene.df) 
  
  # keeps only those points in the aoi boundaries    
  internal_data.df <- subset(fixation_data_scene.df, 
                             is_inside(fixation_data_scene.df, aoi$left, aoi$right, aoi$bottom, aoi$top))
  
  # stores the data by segment into vectors
  internal_data_vector <- c()
  fixation_data_vector <- c()
  gazesample_data_vector <- c()
  segs_length <- length(segment.names)
  for(i in 1:segs_length) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    fixation_data_vector[[i]] <- subset(fixation_data_scene.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data_scene.df, grepl(segment.names[i], scene))
  }
  
  # get start and end times (of all_data's in the python code) as well as the length
  # for the scene
  length <- 0
  starts <- numeric(segs_length)
  ends <- numeric(segs_length)
  for(i in 1:segs_length){
    
    start_and_end_times <- get_seg_start_and_end_times(gazesample_data_vector[[i]])
    starts[i] <- start_and_end_times$start
    ends[i] <- start_and_end_times$end
    length <- length + start_and_end_times$end - start_and_end_times$start
  }
  
  # for each seg, retains only those points in the active intervals of the aoi 
  # also marks whether the aoi is active or not  
  isActives <- logical(segs_length)
  for(j in 1:segs_length){
    
    if(active_intervals[[1]][1] != "" && active_intervals[[1]][1] != "-1,-1"){
      
      # initialize a dataframe with the correct column names and types  
      fixation_data_seg.df_cumulative <- fixation_data_vector[[1]][0,]
      
      # this inside loop gets data inside the intersection of seg and active intervals
      for(i in 1:length(active_intervals[[1]])){
      
        interval_start <- get_tuple_element(1, active_intervals[[1]][i])
        interval_end <- get_tuple_element(2, active_intervals[[1]][i])
      
        # seg time interval is subset of an active interval; keeps all points    
        if(interval_start <= starts[j] && ends[j] <= interval_end) {
        
          fixation_data_seg.df_cumulative <- fixation_data_vector[[j]]
          isActives[j] <- TRUE
          break   
        }
        
        # some intersection; extract points inside 
        if(starts[j] <= interval_end && interval_start <= ends[j]){
        
          start <- max(starts[j], interval_start)
          end <- min(ends[j], interval_end)
          
          isActives[j] <- TRUE
          fixation_data_seg.df_temp <- fixation_data_vector[[j]][
            which(start <= fixation_data_vector[[j]]$timestamp &
                    fixation_data_vector[[j]]$timestamp + 
                    fixation_data_vector[[j]]$fixationduration <= end),]
        
          fixation_data_seg.df_cumulative <- rbind(fixation_data_seg.df_cumulative, fixation_data_seg.df_temp) 
        }
      }
    
      fixation_data_vector[[j]] <- fixation_data_seg.df_cumulative
      
      # takes only points also in the aoi boundaries     
      internal_data_vector[[j]] <- merge(fixation_data_vector[[j]], internal_data_vector[[j]], sort = FALSE)
    } else if(active_intervals[[1]][1] == "-1,-1"){
        
        # inactive
        internal_data_vector[[j]] <- fixation_data_vector[[1]][0,]
    } else{
      
        # always active
        isActives[j] <- TRUE
    }
  }
  
  # bind segs processed above into a whole df again for ease of testing     
  internal_data.df_temp <- data.frame()
  fixation_data_scene.df_temp <- data.frame()
  for(i in 1:segs_length){
    
    internal_data.df_temp <- rbind(internal_data.df_temp, internal_data_vector[[i]])
    fixation_data_scene.df_temp <- rbind(fixation_data_scene.df_temp, fixation_data_vector[[i]])
  }
  internal_data.df <- internal_data.df_temp
  fixation_data_scene.df <- fixation_data_scene.df_temp
  
  ### numfixations ###
  feature_name <- paste(aoi_feature_name_root, "numfixations", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  numfixs <- nrow(internal_data.df)
  
  verify_equivalence(numfixs ,output_value, participant, a_scene, feature_name)
  
  ### proportionnum ###
  feature_name <- paste(aoi_feature_name_root, "proportionnum", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]

  if(nrow(fixation_data_vector[[1]]) != 0){

    internal_value <- nrow(internal_data_vector[[1]]) / nrow(fixation_data_vector[[1]])
  } else{

    internal_value <- 0
  }
  
  if(segs_length > 1) {
    
    i <- 2
    internal_value_numerator <- nrow(internal_data_vector[[1]])
    
    while(i <= segs_length){
    
      if(isActives[i]){
      
        internal_value_numerator <- internal_value_numerator + nrow(internal_data_vector[[i]])
        internal_value <- internal_value_numerator / scene_numfix
      }
      i <- i + 1 
    }
  }
  
  verify_equivalence(internal_value ,output_value, participant, a_scene, feature_name)
  
  ### fixationrate ###
  feature_name <- paste(aoi_feature_name_root, "fixationrate", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  fix_duration <- sum(internal_data.df$fixationduration)
  internal_value <- 0
  
  # else clause looks a little ad hoc, but that is for P18 when merge happens with fixation_duration = 0
  if(nrow(internal_data.df) != 0){
    
    internal_value <- numfixs / fix_duration
  } else if(!is.na(isActives[2]) & isActives[2] == TRUE & fix_duration == 0){
    
      internal_value <- -1    
  }
  
  verify_equivalence(internal_value ,output_value, participant, a_scene, feature_name)
  
  ### totaltimespent ###
  feature_name <- paste(aoi_feature_name_root, "totaltimespent", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  verify_equivalence(fix_duration, output_value, participant, a_scene, feature_name)
  
  ### proportiontime ###
  feature_name <- paste(aoi_feature_name_root, "proportiontime", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  # no merge case for P18 
  if(!is.na(isActives[2]) & isActives[2] == FALSE) {
    
    length <- ends[1] - starts[1]
  }
  
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
  scene_mean <- internal_value # from the preceding computation
  feature_name <- paste(aoi_feature_name_root, "stddevfixationduration", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  # helper function for computing stdev for aoi fixation duration, taking into account
  # scene level aggregation for scenes with multiple segments  
  find_segsd_with_weight <- function(internal_value_vector, scene_mean){
    
    vector_length <- nrow(internal_value_vector)
    numerator <- 0
    
    if(vector_length > 0){
      
      stddev <- sd(internal_value_vector$fixationduration)
      if(is.na(stddev)){
        
        stddev <- 0
      } 
      
      numerator <- (vector_length-1)*(stddev^2) + vector_length*(mean(internal_value_vector$fixationduration)-scene_mean)^2
    }
    return(numerator)
  }
  
  numerator <- 0
  denominator <- 0
  internal_value <- 0
  
  for(i in 1:segs_length){
    
    numerator <- numerator + find_segsd_with_weight(internal_data_vector[[i]], scene_mean)
    denominator <- denominator+nrow(internal_data_vector[[i]])
  }
  if(denominator > 1){
    internal_value <- sqrt(numerator/(denominator-1))
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### longestfixation ###
  feature_name <- paste(aoi_feature_name_root, "longestfixation", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  internal_value <- -1
  
  for(i in 1:segs_length){
    
    if(nrow(internal_data_vector[[i]]) != 0){
      
      internal_value_temp <- max(internal_data_vector[[i]]$fixationduration)
      
      if(internal_value_temp > internal_value){
        
        internal_value <- internal_value_temp
      }
    }
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetofirstfixation ###
  feature_name <- paste(aoi_feature_name_root, "timetofirstfixation", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  internal_values <- numeric(segs_length)
  
  for(i in 1:segs_length){
    
    if(nrow(internal_data_vector[[i]]) != 0){
      
      internal_values[i] <- internal_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- min(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    i <- i+1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetolastfixation ###
  feature_name <- paste(aoi_feature_name_root, "timetolastfixation", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  for(i in 1:segs_length){
    
    column_length <- nrow(internal_data_vector[[i]])
    if(column_length != 0){
      
      internal_values[i] <- 
        internal_data_vector[[i]][column_length,]$timestamp - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- max(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### numtransfrom ###
  aoi1 <- list(x_left = aoi$left, x_right = aoi$right, y_bottom = aoi$bottom, y_top = aoi$top)
  total_count <- 0
  internal_values <- list()
  
  # initialize the vector for storing the number of incoming saccades into the given aoi from each aoi      
  for(name in aois.data[,1]){
    
    internal_values[name] <-0
  }
  
  # if more than one segs in the scene, aggregate the results by applying numtransfrom_per_scene
  # multiple times
  numtransfrom_per_scene <- function(i){
    
    for(name in aois.data[,1]){
      
      aoi2 <- aois.data[aois.data[,1] == name,]
      aoi2 <- list(x_left = aoi2$left , x_right = aoi2$right, y_bottom = aoi2$bottom, y_top = aoi2$top)
      
      internal_value <- trans_from(fixation_data_vector[[i]], aoi1, aoi2)
      internal_values[[name]] <<- internal_values[[name]] + internal_value
      total_count <<- total_count + internal_value
    }
  }
  lapply(1:segs_length, numtransfrom_per_scene)
  
  # check the results 
  for(name in aois.data[,1]){
    
    feature_name <- paste(aoi_feature_name_root, "numtransfrom_", name, sep = "")
    output_value <- subset(emdat_output.df, select = feature_name)[1,]
    
    verify_equivalence(internal_values[[name]], output_value, participant, a_scene, feature_name)
  }
  
  ### proptransfrom ###
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
# timetolastdoubleclic	
# timetolastleftclic	
# timetolastrightclic	

check_aoi_eve <- function(emdat_output.df, 
                          participant, 
                          a_scene,
                          aoi_name,
                          aois.data,
                          active_intervals,
                          segment.names,
                          events_data_scene.df,
                          gazesample_data_scene.df){
  
  ### set up the tests ###
  aoi <- aois.data[aois.data[,"aoi_name"] == aoi_name,]
  
  aoi_feature_name_root <- set_root_name(aoi_name)
  
  # keeps only those points in the aoi boundaries
  internal_data.df <- subset(events_data_scene.df,
                             grepl('MouseClick', event) &
                             as.numeric(as.character(x_coord)) > aoi$left &
                             as.numeric(as.character(x_coord)) <= aoi$right &
                             as.numeric(as.character(y_coord)) <= aoi$bottom &
                             as.numeric(as.character(y_coord)) > aoi$top)
  
  # stores the data by segment into vectors
  internal_data_vector <- c()
  gazesample_data_vector <- c()
  segs_length <- length(segment.names)
  for(i in 1:segs_length) {
    
    internal_data_vector[[i]] <- subset(internal_data.df, grepl(segment.names[i], scene))
    gazesample_data_vector[[i]] <- subset(gazesample_data_scene.df, grepl(segment.names[i], scene))
  }
  
  # get start and end times (of all_data's in the python code) as well as the length
  # for the scene
  length <- 0
  starts <- numeric(segs_length)
  ends <- numeric(segs_length)
  for(i in 1:segs_length){
    
    start_and_end_times <- get_seg_start_and_end_times(gazesample_data_vector[[i]])
    starts[i] <- start_and_end_times$start
    ends[i] <- start_and_end_times$end
    length <- length + start_and_end_times$end - start_and_end_times$start
  }
  
  # for each seg, retains only those points in the active intervals of the aoi 
  for(j in 1:segs_length){
    
    # "" for always active
    if(active_intervals[[1]][1] != "" && active_intervals[[1]][1] != "-1,-1"){
      
      # initialize a dataframe with the correct column names and types  
      events_data_seg.df_cumulative <- internal_data_vector[[1]][0,]
      
      # this inner loop gets data inside the intersection of seg and active intervals
      for(i in 1:length(active_intervals[[1]])){
        
        interval_start <- get_tuple_element(1, active_intervals[[1]][i])
        interval_end <- get_tuple_element(2, active_intervals[[1]][i])
        
        # seg time interval is subset of the active interval; keeps all points    
        if(interval_start <= starts[j] && ends[j] <= interval_end) {
          
          events_data_seg.df_cumulative <- internal_data_vector[[j]]
          break   
        }
        
        # some intersection; extract points inside
        if(starts[j] <= interval_end && interval_start <= ends[j]){
          
          start <- max(starts[j], interval_start)
          end <- min(ends[j], interval_end)
          
          events_data_seg.df_temp <- internal_data_vector[[j]][
            which(start <= internal_data_vector[[j]]$timestamp &
                  internal_data_vector[[j]]$timestamp <= end),]
          
          events_data_seg.df_cumulative <- rbind(events_data_seg.df_cumulative, events_data_seg.df_temp)           
        }
      }
      
      internal_data_vector[[j]] <- events_data_seg.df_cumulative
    } else if(active_intervals[[1]][1] == "-1,-1"){
      
        # inactive
        internal_data_vector[[j]] <- internal_data_vector[[1]][0,]
    }
  }
  
  # bind segs processed above into a whole df again for ease of testing
  internal_data.df_temp <- data.frame()
  for(i in 1:segs_length){
    
    internal_data.df_temp <- rbind(internal_data.df_temp, internal_data_vector[[i]])
  }
  internal_data.df <- internal_data.df_temp
  
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
  
  double_clicks <- 0
  left_clicks <- 0
  first_double_clicks <- c()
  first_left_clicks <- c()
  last_double_clicks <- c()
  last_left_clicks <- c()
  
  for(i in 1:segs_length){
    
    clicks <- find_double_and_left_clicks(internal_data_vector[[i]])
    double_clicks <- double_clicks + clicks[1]
    left_clicks <- left_clicks + clicks[2]
    first_double_clicks[i] <- clicks[3]
    first_left_clicks[i] <- clicks[4]
    last_double_clicks[i] <-  clicks[5]
    last_left_clicks[i] <- clicks[6]
  }
  internal_value <- double_clicks
  
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
  
  internal_value <- left_clicks
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### leftclicrate ###
  feature_name <- paste(aoi_feature_name_root, "leftclicrate", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  # no merge case for P18 
  if(segs_length == 2 && nrow(internal_data_vector[[2]]) == 0) {
    
    length <- ends[1] - starts[1]
  }
  
  if(length != 0){
    
    internal_value <- internal_value / length
  } else {
    
    internal_value <- 0 
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetofirstdoubleclic ###
  feature_name <- paste(aoi_feature_name_root, "timetofirstdoubleclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  internal_values <- c()
  
  for(i in 1:segs_length){
    
    if(first_double_clicks[i] != -1){
      
      internal_values[i] <- first_double_clicks[i] - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- min(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetofirstleftclic ###
  feature_name <- paste(aoi_feature_name_root, "timetofirstleftclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  for(i in 1:segs_length){
    
    if(first_left_clicks[i] != -1){
      
      internal_values[i] <- first_left_clicks[i] - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- min(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetofirstrightclic ###
  feature_name <- paste(aoi_feature_name_root, "timetofirstrightclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  for(i in 1:segs_length){
    
    rightclicks.df <- subset(internal_data_vector[[i]], event=="RightMouseClick")
    if(nrow(rightclicks.df) != 0){
      
      internal_values[i] <- rightclicks.df[1,]$timestamp - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- min(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetolastdoubleclic ###
  feature_name <- paste(aoi_feature_name_root, "timetolastdoubleclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  internal_values <- c()
  
  for(i in 1:segs_length){
    
    if(last_double_clicks[i] != -1){
      
      internal_values[i] <- last_double_clicks[i] - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- max(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetolastleftclic ###
  feature_name <- paste(aoi_feature_name_root, "timetolastleftclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  for(i in 1:segs_length){
    
    if(last_left_clicks[i] != -1){
      
      internal_values[i] <- last_left_clicks[i] - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- max(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
  
  ### timetolastrightclic ###
  feature_name <- paste(aoi_feature_name_root, "timetolastrightclic", sep = "")
  output_value <- subset(emdat_output.df, select = feature_name)[1,]
  
  for(i in 1:segs_length){
    
    rightclicks.df <- subset(internal_data_vector[[i]], event=="RightMouseClick")
    rightclick_counts <- nrow(rightclicks.df)
    
    if(rightclick_counts != 0){
      
      internal_values[i] <- rightclicks.df[rightclick_counts,]$timestamp - gazesample_data_vector[[i]][1,]$timestamp
    } else{
      
      internal_values[i] <- -1
    }
  }
  
  internal_value <- internal_values[1]
  i <- 2
  while(i <= length(internal_data_vector)){
    
    if(internal_values[i] != -1){
      
      internal_value <- max(internal_value, internal_values[i] + 
                              (gazesample_data_vector[[i]][1,]$timestamp - gazesample_data_vector[[1]][1,]$timestamp)) 
    }
    
    i <- i + 1
  }
  
  verify_equivalence(internal_value, output_value, participant, a_scene, feature_name)
}

##########################################################################################

# When called, commences the tests for the given list of participants
run_aoiTest <- function(participants, last_participant){
  
  for(i in 1:length(participants)){
    
    participant <- participants[i]
    readfiles_aoi(participant,
                  paste(seg_file_path, "P", participant, ".seg", sep = ""),
                  paste(aoi_file_path, participant, ".aois", sep = ""), 
                  last_participant)
  }
  writeLines(paste("####### cumulative total number of tests run: ", cumulative_counter, " #######"))
  cumulative_counter <<- 0
}

##### To Run #####

# Set up the tests: choose the range of particpants to run the tests on
participants <- list("16", "17", "18")

# Run
# Note: last_participant refers to the last in the EMDAT output file used, not necessarily that
#       in the list of participants
run_aoiTest(participants, "18")






