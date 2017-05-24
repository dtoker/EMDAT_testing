# root_path <- "Part2_EMDATInternal_EMDATOutput/new_data/SegFiles/P"
# inverted_time_counts <- 0
# inverted_times_per_participants <- list()  
# 
# find_inverted_time <- function(seg_file, participant){
#   
#   seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
#   acceptable_seg_file.df <- subset(seg_file.df, end > start)
#   removed <- setdiff(seg_file.df[,1], acceptable_seg_file.df[,1])
#   
#   inverted_times_per_participants[[substr(seg_file, 60, 63)]] <<- length(removed)
#   inverted_time_counts <<- inverted_time_counts + length(removed)
#   
#   if(length(removed) != 0){
#     for(i in 1:length(removed)){
#       
#       scene <- subset(seg_file.df,scene==removed[i])
#       print(paste("end_time < start_time for participant ", 
#                   participant, 
#                   ": ", 
#                   removed[i],
#                   " start: ",
#                   scene$start,
#                   " end: ",
#                   scene$end,
#                   sep = ""))
#     }
#   }
# }
# 
# # 143 missing
# for(i in 101:142){
#   
#   part_a <- paste(as.character(i), "a", sep = "")
#   part_b <- paste(as.character(i), "b", sep = "")
#   
#   find_inverted_time(paste(root_path, part_a, ".seg", sep = ""), part_a)
#   find_inverted_time(paste(root_path, part_b, ".seg", sep = ""), part_b)
# }
# 
# for(i in 144:162){
#   
#   part_a <- paste(as.character(i), "a", sep = "")
#   part_b <- paste(as.character(i), "b", sep = "")
#   
#   find_inverted_time(paste(root_path, part_a, ".seg", sep = ""), part_a)
#   find_inverted_time(paste(root_path, part_b, ".seg", sep = ""), part_b)
# }
# 
# ##### Summary #####
# 
# print(paste(cat("\n"), "##### RESULT SUMMARY ######"))
# print(paste("Total inverted time counts: ", 
#             inverted_time_counts, 
#             sep = ""))
# print(paste("Scenes lost(not including misssing particpant 143): ", 
#             100*(inverted_time_counts/(61*2*40)),
#             "%",
#             sep = ""))
# 
# for(i in 101:142){
#   part_a <- paste(as.character(i), "a", sep = "")
#   part_b <- paste(as.character(i), "b", sep = "")
#   
#   print(paste("inverted_times for ",
#               part_a,
#               ": ",
#               inverted_times_per_participants[[part_a]],
#               " ",
#               part_b,
#               ": ",
#               inverted_times_per_participants[[part_b]],
#               sep = "")
#   )
# } 
# 
# for(i in 144:162){
#   part_a <- paste(as.character(i), "a", sep = "")
#   part_b <- paste(as.character(i), "b", sep = "")
#   
#   print(paste("inverted_times for ",
#               part_a,
#               ": ",
#               inverted_times_per_participants[[part_a]],
#               " ",
#               part_b,
#               ": ",
#               inverted_times_per_participants[[part_b]],
#               sep = "")
#   )
# }

root_path <- "Part2_EMDATInternal_EMDATOutput/new_data/SegFiles/P"
inverted_time_counts <- 0

count_scenes <- function(seg_file, participant){
  
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  scenes <- seg_file.df[,1]
  print(paste(participant, " has ", as.character(length(scenes)), " scenes", sep = ""))
}

# 143 missing
for(i in 101:142){
  
  part_a <- paste(as.character(i), "a", sep = "")
  part_b <- paste(as.character(i), "b", sep = "")
  
  count_scenes(paste(root_path, part_a, ".seg", sep = ""), part_a)
  count_scenes(paste(root_path, part_b, ".seg", sep = ""), part_b)
}

for(i in 144:162){
  
  part_a <- paste(as.character(i), "a", sep = "")
  part_b <- paste(as.character(i), "b", sep = "")
  
  count_scenes(paste(root_path, part_a, ".seg", sep = ""), part_a)
  count_scenes(paste(root_path, part_b, ".seg", sep = ""), part_b)
}

