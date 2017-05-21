root_path <- "Part2_EMDATInternal_EMDATOutput/new_data/SegFiles_splitTX/P"
inverted_time_counts <- 0

find_inverted_time <- function(seg_file, participant){
  
  seg_file.df <- read.csv(seg_file, sep="\t", header = FALSE, col.names = c("scene","segment","start","end"))
  acceptable_seg_file.df <- subset(seg_file.df, end > start)
  removed <- setdiff(seg_file.df[,1], acceptable_seg_file.df[,1])
  inverted_time_counts <<- inverted_time_counts + length(removed)
  if(length(removed) != 0){
    print(paste("Scene with end_time < start_time for participant ", participant, ": ", removed, sep = ""))
  }
}

# 143 missing
for(i in 101:142){
  
  part_a <- paste(as.character(i), "a", sep = "")
  part_b <- paste(as.character(i), "b", sep = "")
  
  find_inverted_time(paste(root_path, part_a, ".seg", sep = ""), part_a)
  find_inverted_time(paste(root_path, part_b, ".seg", sep = ""), part_b)
}

for(i in 144:162){
  
  part_a <- paste(as.character(i), "a", sep = "")
  part_b <- paste(as.character(i), "b", sep = "")
  
  find_inverted_time(paste(root_path, part_a, ".seg", sep = ""), part_a)
  find_inverted_time(paste(root_path, part_b, ".seg", sep = ""), part_b)
}

print(paste("Total inverted time counts: ", inverted_time_counts, sep = ""))
