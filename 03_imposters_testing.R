### imposters_testing

library(stylo)

# define variables for analysis

methods_combination <- read.csv("features/03_imposters_testing_features.csv", stringsAsFactors = F)

my_passage_length <- 500
validation_rounds <- 10

# read texts in training set

my_texts <- list.files("training_set/")
my_authors <- strsplit(my_texts, "_")
my_authors <- unlist(lapply(my_authors, function(x) x[1]))
my_authors_sel <- unique(my_authors)
my_texts <- list.files("training_set", full.names = T)

# prepare lists for final results

verifications <- vector(mode = "list", length = length(my_authors_sel))
convalidations <- vector(mode = "list", length = length(my_authors_sel))

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

# prepare imposters texts 
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

# prepare dev corpus (per author)

all_dev_files <- list.files("development_set", full.names = T)
all_dev_authors <- list.files("development_set")
all_dev_authors <- gsub(".txt", "", all_dev_authors)

mw_corpus <- list()

for(i in 1:length(all_dev_files)){
  
  my_text <- readLines(all_dev_files[i])
  my_text <- paste(my_text, collapse = " ")
  my_text <- clean_texts(my_text)
  
  my_text_stylo <- stylo::txt.to.words.ext(my_text, corpus.lang = "German")
  
  # use moving window to create multiple selections of 500-word-long texts
  mw_corpus[[i]] <- list()
  
  for(n in 1:ceiling((length(my_text_stylo)/50))){
    
    mw_corpus[[i]][[n]] <- my_text_stylo[((n-1)*50)+(1:500)]
    
  }
  
  # delete chunks shorter than 500 words
  number_NAs <- unlist(lapply(mw_corpus[[i]], function(x) length(which(is.na(x)))))
  exclude <- which(number_NAs > 0)
  mw_corpus[[i]] <- mw_corpus[[i]][-exclude]
  
}

names(mw_corpus) <- all_dev_authors

# prepare final results container

filename <- "imposters_testing__results.txt"

cat("Imposters results\n\n", file = filename)

# main loop for analysis 

for(method in 1:length(methods_combination$n_best_imposters)){
  
  cat("\n\nConfiguration:", methods_combination$n_best_imposters[method], "imposters", methods_combination$MFW[method], "MFW", methods_combination$distance[method], "\n\n", file = filename, append = T)
  
  for(validation in 1:validation_rounds){
    
    # select best imposters (at the moment, random)
    full_corpus_tmp <- full_corpus[sample(1:length(all_imposters), methods_combination$n_best_imposters[method], replace = F)]
    
    cat("\n\nValidation", validation, "\n\n", file = filename, append = T)
    
    # find authors' texts
    author_reference <- list()
    text_to_attribute <- list()
    
    for(i in 1:length(my_authors_sel)){
      
      # prepare corpora per author
      author_texts <- which(my_authors == my_authors_sel[i])
      author_reference[[i]] <- unlist(candidates_corpus[author_texts])
      names(author_reference)[i] <- my_authors_sel[i]
      
      # prepare datasets for analysis
      text_to_attribute[[i]] <- mw_corpus[[i]][[sample(1:length(mw_corpus[[i]]), 1)]]
      
    }
    
    # run imposters analysis
    for(i in 1:length(my_authors_sel)){
      
      candidate_stylo <- author_reference[i]
      my_candidate <- my_authors_sel[i]
      
      # prepare full corpus for analysis 
      imposters_corpus <- c(test = list(text_to_attribute[[i]]), candidate_stylo, full_corpus_tmp)
      
      test_id <- which(names(imposters_corpus)=="test")
      candidate_id <- which(grepl(x = names(imposters_corpus), pattern = my_candidate))
      
      # computing a list of most frequent words (trimmed to top 2000 items):
      features = make.frequency.list(imposters_corpus, head = 2000)
      
      # producing a table of relative frequencies:
      data = make.table.of.frequencies(imposters_corpus, features, relative = TRUE)
      
      # culling
      culled_ids <- which(!apply(data, 2, there_is_zero))
      data <- data[,culled_ids]
      if(dim(data)[2] > methods_combination$MFW[method])
        data <- data[,1:methods_combination$MFW[method]]
      
      # who wrote the test text? (in my case, this is the 1st row in the table):
      imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = methods_combination$distance[method])
      
      cat(my_candidate, " probability: ", imposters_results, "\n", file = filename, sep = "", append = T)
      
      verifications[[i]] <- c(verifications[[i]], imposters_results)
      
      # testing on the wrong author (Imposters should negate the attribution)
      
      # prepare full corpus for analysis
      other_author <- sample((1:length(my_authors_sel))[-i], 1)
      imposters_corpus <- c(test = list(text_to_attribute[[other_author]]), candidate_stylo, full_corpus_tmp)
      
      test_id <- which(names(imposters_corpus)=="test")
      candidate_id <- which(grepl(x = names(imposters_corpus), pattern = my_candidate))
      
      # computing a list of most frequent words (trimmed to top 2000 items):
      features = make.frequency.list(imposters_corpus, head = 2000)
      
      # producing a table of relative frequencies:
      data = make.table.of.frequencies(imposters_corpus, features, relative = TRUE)
      
      # culling
      culled_ids <- which(!apply(data, 2, there_is_zero))
      data <- data[,culled_ids]
      if(dim(data)[2] > methods_combination$MFW[method])
        data <- data[,1:methods_combination$MFW[method]]
      
      # who wrote the test text? (in my case, this is the 1st row in the table):
      imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = methods_combination$distance[method])
      
      cat("Not ", my_candidate, " probability: ", imposters_results, "\n", file = filename, sep = "", append = T)
      
      convalidations[[i]] <- c(convalidations[[i]], imposters_results)
      
      cat("method:", method/length(methods_combination$n_best_imposters), "validation:", validation/validation_rounds, "\n", file = "progress.log")
      
    }
    
  }
  
  save(verifications, convalidations, methods_combination, my_passage_length, validation_rounds, file = gsub(".txt", ".RData", filename))
  
}
