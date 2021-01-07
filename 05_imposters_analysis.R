### imposters_analysis

library(tidyverse)
library(stylo)

validation_rounds <- 10

# read file with results of refined testing
load("imposters_testing_refine__results.RData")

################ Step 1
# process results to find the best-working group of impostors
################

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

methods_combination <- methods_combination %>% filter(Var4 == selected_group)

################ Step 2
# analysis of Heimat texts with the impostors method
################

my_texts <- list.files("training_set/")
my_authors <- strsplit(my_texts, "_")
my_authors <- unlist(lapply(my_authors, function(x) x[1]))
my_authors_sel <- unique(my_authors)
my_texts <- list.files("training_set", full.names = T)

# cleaning function
clean_texts <- function(my_texts){
  for(i in 1:length(my_texts)){
    my_string <- my_texts[i]
    ##substitute old S
    my_string <- gsub("ſ", "s", my_string)
    ##substitute umlauts and double S
    my_string <- gsub("ä", "ae", my_string)
    my_string <- gsub("ö", "oe", my_string)
    my_string <- gsub("ü", "ue", my_string)
    my_string <- gsub("ß", "ss", my_string)
    
    ##substitute line breaks
    my_string <- gsub("([a-z])-\\s+([a-z])", "\\1\\2", my_string)
    my_string <- gsub("([a-z])¬\\s+([a-z])", "\\1\\2", my_string)
    
    ##delete multiple spaces
    my_string <- gsub(pattern = " +", replacement = " ", x = my_string)
    
    ##save new version
    my_texts[i] <- my_string
    if(length(my_texts) == 1){
      print("Cleaned")
    }else{
      print(i)
    }
    
  }
  return(my_texts)
}

# additional function (for culling)
there_is_zero <- function(x){
  if(0 %in% x){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

# prepare candidates corpus

candidates_corpus <- list()

for(i in 1:length(my_texts)){
  
  tmp_text <- paste(readLines(my_texts[i]), collapse = " ")
  tmp_text <- clean_texts(tmp_text)
  candidates_corpus[[i]] <- stylo::txt.to.words.ext(tmp_text, corpus.lang = "German")
  print(i)
  
}

# find authors' texts
author_reference <- list()

for(i in 1:length(my_authors_sel)){
  
  # prepare corpora per author
  author_texts <- which(my_authors == my_authors_sel[i])
  author_reference[[i]] <- unlist(candidates_corpus[author_texts])
  names(author_reference)[i] <- my_authors_sel[i]
  
}

# create imposters corpus

download.file("https://owncloud.gwdg.de/index.php/s/b7umCKWXFY2AbVh/download", destfile = "Kolimo_corpus.RData")
download.file("https://owncloud.gwdg.de/index.php/s/627ruOt6gE4uDiY/download", destfile = "Kolimo_metadata.RData")
load("Kolimo_corpus.RData")
load("Kolimo_metadata.RData")

# define function for processing texts

text_process <- function(my_texts){
  texts_stylo <- list()
  for(i in 1:length(my_texts)){
    texts_stylo[[i]] <- stylo::txt.to.words.ext(input.text = my_texts[i], corpus.lang = "German")
    print(i)
  }
  return(texts_stylo)
}

# prepare texts
full_metadata_withDates <- full_metadata_withDates[1:6100,]
all_texts <- Kolimo_texts[which(full_metadata_withDates$date > 1880)]
full_metadata_withDates <- full_metadata_withDates[which(full_metadata_withDates$date > 1880),]

# exclude (possible) duplicated titles
duplicated_testing <- paste(full_metadata_withDates$gnd_id, full_metadata_withDates$title)
exclude <- which(duplicated(duplicated_testing))
if(length(exclude) > 0){
  all_texts <- all_texts[-exclude]
  full_metadata_withDates <- full_metadata_withDates[-exclude,]
}

# clean imposters texts
all_texts <- clean_texts(all_texts)

# separate candidate authors from imposters
candidates <- c("http://d-nb.info/gnd/118585916", # Musil
                "http://d-nb.info/gnd/118511653", # Blei
                "http://d-nb.info/gnd/118562533", # Kitsch
                "http://d-nb.info/gnd/118543342" # Guetersloh
)
exclude <- which(full_metadata_withDates$gnd_id %in% candidates)
all_texts <- all_texts[-exclude]
full_metadata_withDates <- full_metadata_withDates[-exclude,]

# clean imposters texts 
full_corpus_tmp <- text_process(all_texts)

# collapse all texts by the same imposter
full_corpus <-  list()
all_imposters <- unique(full_metadata_withDates$gnd_id)
for(i in 1:length(all_imposters)){
  
  my_imposter_texts <- which(full_metadata_withDates$gnd_id == all_imposters[i])
  full_corpus[[i]] <- unlist(full_corpus_tmp[my_imposter_texts])
  
}

# exclude texts shorter than 5000 words

length_limit <- 5000
texts_l <- unlist(lapply(full_corpus, length))
good_length <- which(texts_l >= length_limit)
full_corpus <- full_corpus[good_length]
all_imposters <- all_imposters[good_length]
names(full_corpus) <- all_imposters
remove(full_corpus_tmp)

# exclude texts that do not share words

# computing a list of most frequent words (trimmed to top 2000 items):
features = make.frequency.list(candidates_corpus, head = 2000)

exclude <- numeric()

# find texts that share at least 50% of MFW
for(i in 1:length(full_corpus)){
  
  print(i)
  present_words <- length(which(features %in% full_corpus[[i]]))
  if(present_words < 1000)
    exclude <- c(exclude, i)
  
}
full_corpus <- full_corpus[-exclude]
all_imposters <- all_imposters[-exclude]

# select imposters for analysis based on previous tests
imposters_selection <- which(all_imposters %in% validation_imposters[[selected_group]])

full_corpus_tmp <- full_corpus[imposters_selection]

# prepare Heimat corpus
metadata <- read.csv("test_set/metadata.csv", stringsAsFactors = F, row.names = 1)

heimat_stylo <- list()

metadata <- metadata[which(metadata$stylometry == "yes"),]

for(i in 1:length(metadata$issue)){
  
  filename <- paste("test_set/", metadata$issue[i], "/", substr(metadata$issue[i], 4, 9), "-", str_pad(metadata$article[i], 3, side = "left", pad = "0"), ".txt", sep = "")
  
  my_text <- readLines(filename)
  my_text <- paste(my_text, collapse = " ")
  my_text <- clean_texts(my_text)
  heimat_stylo[[i]] <- stylo::txt.to.words.ext(my_text, corpus.lang = "German")
  
  print(i)
  
}

# prepare final results container

filename <- "imposters_analysis__results.txt"

cat("Imposters results\n\n", file = filename)

imposters_final_result <- list()

# main imposters loop (on each Heimat text)

for(heimat_text in 1:length(heimat_stylo)){
  
  imposters_final_result[[heimat_text]] <- list()
  
  cat("#####################\n\nText ", heimat_text, "\nIssue ", metadata$issue[heimat_text], " Article no. ", metadata$article[heimat_text], "\nTitle: ", metadata$title[heimat_text], "\n\nPossible authors\n", sep = "", file = filename, append = T)
    
  # run imposters analysis on each candidate author
  for(i in 1:length(my_authors_sel)){
    
    candidate_stylo <- author_reference[i]
    my_candidate <- my_authors_sel[i]
    
    # prepare full corpus for analysis 
    imposters_corpus <- c(test = heimat_stylo[heimat_text], candidate_stylo, full_corpus_tmp)
    
    test_id <- which(names(imposters_corpus)=="test")
    candidate_id <- which(grepl(x = names(imposters_corpus), pattern = my_candidate))
    
    # computing a list of most frequent words (trimmed to top 2000 items):
    features = make.frequency.list(imposters_corpus, head = 2000)
    
    # producing a table of relative frequencies:
    data_full = make.table.of.frequencies(imposters_corpus, features, relative = TRUE)
    
    result_tmp <- numeric()
    analysis_round <- 1
    
    for(method in 1:length(methods_combination$Var1)){
      
      # culling
      culled_ids <- which(!apply(data_full, 2, there_is_zero))
      data <- data_full[,culled_ids]
      if(dim(data)[2] > methods_combination$Var2[method])
        data <- data[,1:methods_combination$Var2[method]]
      
      for(validation in 1:validation_rounds){
        
        # who wrote the test text? (in my case, this is the 1st row in the table):
        result_tmp[analysis_round] <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = methods_combination$Var3[method])
        analysis_round <- analysis_round + 1
      
      }
      
      cat("text:", heimat_text, "author:", i, "validation:", method/length(methods_combination$Var1), "\n", file = "progress.log")
      
    }
    
    imposters_final_result[[heimat_text]][[i]] <- result_tmp
    cat(my_candidate, " probability: ", mean(result_tmp), "\n", file = filename, sep = "", append = T)
    
    save(imposters_final_result, methods_combination, validation_imposters, selected_group, my_authors_sel, metadata, file = gsub(".txt", ".RData", filename))
    
  }
  
}



