### final_results

library(tidyverse)

################ Step 1
# find thresholds for impostors analysis (based on the refined testing)
################

load("imposters_testing_refine__results.RData")

my_authors <- c("Blei", "Guetersloh", "Kisch", "Musil")

for(i in 1:length(my_authors)){
  
  verifications[[i]] <- split(verifications[[i]], ceiling(seq_along(verifications[[i]])/validation_rounds))
  verifications[[i]] <- lapply(verifications[[i]], function(x) c(mean(x), sd(x)))
  
  convalidations[[i]] <- split(convalidations[[i]], ceiling(seq_along(convalidations[[i]])/validation_rounds))
  convalidations[[i]] <- lapply(convalidations[[i]], function(x) c(mean(x), sd(x)))
  
}

results_per_method <- methods_combination[1:length(verifications[[1]]),]
colnames(results_per_method) <- c("imposters", "MFW", "distance", "imposters_group")

for(i in 1:length(verifications)){
  
  tmp_df <- cbind(do.call(rbind.data.frame, verifications[[i]]), do.call(rbind.data.frame, convalidations[[i]]))
  colnames(tmp_df) <- c(my_authors[i], paste(my_authors[i], "sd", sep = "_"), paste("NOT", my_authors[i], sep = "_"), paste("NOT", my_authors[i], "sd", sep = "_"))
  results_per_method <- cbind(results_per_method, tmp_df)
  
}

validation_mean <- numeric()
validation_sd <- numeric()
validation_negative_mean <- numeric()
validation_negative_sd <- numeric()

for(i in 1:length(verifications[[i]])){
  
  all_values <- unlist(lapply(verifications, function(x) x[i]), recursive = F)
  validation_mean[i] <- mean(unlist(lapply(all_values, function(x) x[1])))
  validation_sd[i] <- mean(unlist(lapply(all_values, function(x) x[2])))
  
  all_values <- unlist(lapply(convalidations, function(x) x[i]), recursive = F)
  validation_negative_mean[i] <- mean(unlist(lapply(all_values, function(x) x[1])))
  validation_negative_sd[i] <- mean(unlist(lapply(all_values, function(x) x[2])))
  
}

results_per_method <- cbind(results_per_method, validation_mean, validation_sd, validation_negative_mean, validation_negative_sd)

results_per_method$quality <- results_per_method$validation_mean - results_per_method$validation_negative_mean

results_per_method$quality_sd <- (results_per_method$validation_sd - results_per_method$validation_negative_sd)/2

evaluation_df <- data.frame(imposters_group = character(), author = character(), present = logical(), imposters_prop = numeric(), imposters_sd = numeric(), stringsAsFactors = F)

for(my_group in unique(results_per_method$imposters_group)){
  
  tmp_df <- results_per_method %>% filter(imposters_group == my_group)
  
  for(author in my_authors){
    
    evaluation_df <- rbind(evaluation_df, data.frame(imposters_group = my_group, author = author, present = TRUE, imposters_prop = mean(tmp_df[,author]), imposters_sd = sd(tmp_df[,author]), stringsAsFactors = F))
    evaluation_df <- rbind(evaluation_df, data.frame(imposters_group = my_group, author = author, present = FALSE, imposters_prop = mean(tmp_df[,paste("NOT", author, sep = "_")]), imposters_sd = sd(tmp_df[,paste("NOT", author, sep = "_")]), stringsAsFactors = F))
    
  }
  
}

# select best group
evaluation_df$quality_syn <- 0
evaluation_df$quality_syn[which(evaluation_df$present)] <- evaluation_df$imposters_prop[which(evaluation_df$present)] - evaluation_df$imposters_sd[which(evaluation_df$present)]
evaluation_df$quality_syn[which(!evaluation_df$present)] <- evaluation_df$imposters_prop[which(!evaluation_df$present)] + evaluation_df$imposters_sd[which(!evaluation_df$present)]

for(i in 1:(length(evaluation_df$imposters_group)/2)){
  
  evaluation_df$quality_syn[(i*2)-1] <- evaluation_df$quality_syn[(i*2)-1] - evaluation_df$quality_syn[(i*2)]
  evaluation_df$quality_syn[(i*2)] <- 0
  
}

evaluation_syn <- evaluation_df %>% group_by(imposters_group) %>% summarize(quality_syn = sum(quality_syn))

selected_group <- which.max(evaluation_syn$quality_syn)

evaluation_df <- evaluation_df %>% filter(imposters_group == selected_group)


red_threshold <- numeric()
green_threshold <- numeric()

for(i in 1:length(my_authors)){
  
  tmp_df <- evaluation_df %>% filter(author == my_authors[i])
  
  red_threshold[i] <- tmp_df$imposters_prop[which(!tmp_df$present)] + tmp_df$imposters_sd[which(!tmp_df$present)]
  if(tmp_df$imposters_prop[which(tmp_df$present)] - tmp_df$imposters_sd[which(tmp_df$present)] < red_threshold[i])
    red_threshold[i] <- tmp_df$imposters_prop[which(tmp_df$present)] - tmp_df$imposters_sd[which(tmp_df$present)]
  
  green_threshold[i] <- tmp_df$imposters_prop[which(tmp_df$present)] - tmp_df$imposters_sd[which(tmp_df$present)]
  if(tmp_df$imposters_prop[which(!tmp_df$present)] + tmp_df$imposters_sd[which(!tmp_df$present)] > green_threshold[i])
    green_threshold[i] <- tmp_df$imposters_prop[which(!tmp_df$present)] + tmp_df$imposters_sd[which(!tmp_df$present)]
  
}

imposters_thresholds <- data.frame(author = my_authors, red_threshold, green_threshold, stringsAsFactors = F)

################ Step 2
# read results of candidates analysis
################

attributions_text <- readLines("candidates_analysis__results.txt")

heimat_attributions <- list()

for(i in 1:length(my_authors)){
  
  attributions <- attributions_text[which(grepl(pattern = my_authors[i], x = attributions_text))]
  attributions <- strsplit(attributions, ":")
  attributions <- unlist(lapply(attributions, function(x) x[2]))
  attributions <- as.numeric(gsub("%", "", attributions))
  heimat_attributions[[i]] <- attributions
  
}

################ Step 3
# read results of impostors analysis
################

imposters_text <- readLines("imposters_analysis__results.txt")

heimat_imposters <- list()
heimat_imposters_scores <- list()

for(i in 1:length(my_authors)){
  
  imposters <- imposters_text[which(grepl(pattern = my_authors[i], x = imposters_text))]
  imposters <- strsplit(imposters, ":")
  imposters <- unlist(lapply(imposters, function(x) x[2]))
  imposters <- as.numeric(imposters)
  imposters_scores <- imposters
  
  for(n in 1:length(imposters)){
    
    if(imposters[n] <= imposters_thresholds$red_threshold[imposters_thresholds$author == my_authors[i]])
      result <- "Negated"
    if(imposters[n] > imposters_thresholds$red_threshold[imposters_thresholds$author == my_authors[i]] & imposters[n] < imposters_thresholds$green_threshold[imposters_thresholds$author == my_authors[i]])
      result <- "Uncertain"
    if(imposters[n] >= imposters_thresholds$green_threshold[imposters_thresholds$author == my_authors[i]])
      result <- "Confirmed"
    
    imposters[n] <- result
    
  }
  
  heimat_imposters[[i]] <- imposters
  heimat_imposters_scores[[i]] <- imposters_scores
  
}


final_attributions <- character()
final_attributions_perc <- numeric()
final_imposters <- character()
final_imposters_score <- character()

for(i in 1:length(heimat_imposters[[1]])){
  
  attributions <- unlist(lapply(heimat_attributions, function(x) x[i]))
  if(length(which(attributions > 50)) > 0){
    final_attributions[i] <- my_authors[which(attributions > 50)]
    final_attributions_perc[i] <- attributions[which(attributions > 50)]
    final_imposters[i] <- unlist(lapply(heimat_imposters, function(x) x[i]))[which(attributions > 50)]
    final_imposters_score[i] <- unlist(lapply(heimat_imposters_scores, function(x) x[i]))[which(attributions > 50)]
  }else{
    final_attributions[i] <- "Uncertain"
    final_attributions_perc[i] <- 0
    final_imposters[i] <- "Uncertain"
    final_imposters_score[i] <- 0
  }
  
}

################ Step 4
# merge all results in a single table
################

metadata <- read.csv("test_set/metadata.csv", stringsAsFactors = F)
metadata <- metadata[metadata$stylometry == "yes",]

metadata$attribution <- final_attributions
metadata$attribution_percentage <- final_attributions_perc
metadata$imposters_confirmation <- final_imposters
metadata$imposters_score <- final_imposters_score
metadata$imposters_score <- round(as.numeric(metadata$imposters_score), digits = 3)

write.csv(metadata, file = "Heimat_stylometry__final_results.csv", row.names = F)