### imposters_testing_refine

library(stylo)

# define variables for analysis

n_best_imposters <- 16
MFW_series <- c((1:10)*20)
my_distances <- c("dist.delta", "dist.eder", "dist.canberra", "dist.wurzburg")

my_passage_length <- 500
validation_rounds <- 10
imposters_selections <- 12

methods_combination <- expand.grid(n_best_imposters, MFW_series, my_distances, 1:imposters_selections, stringsAsFactors = FALSE)

# read texts in training set

my_texts <- list.files("training_set/")
my_authors <- strsplit(my_texts, "_")
my_authors <- unlist(lapply(my_authors, function(x) x[1]))
my_authors_sel <- unique(my_authors)
my_texts <- list.files("training_set", full.names = T)

# prepare lists for final result

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
all_texts_candidates <- all_texts[exclude]
full_metadata_withDates_candidates <- full_metadata_withDates[exclude,]
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

# prepare multiple combinations of imposters to find the best one

validation_imposters <- list()

for(i in 1:imposters_selections){
  
  # select best imposters (at the moment, random)
  validation_imposters[[i]] <- sample(all_imposters, n_best_imposters, replace = F)
  
}

# prepare dev corpus (per author)

blei_text <- readLines("development_set/blei.txt")
musil_text <- readLines("development_set/musil.txt")
guetersloh_text <- readLines("development_set/guetersloh.txt")
kisch_text <- readLines("development_set/kisch.txt")

# 1. Blei

blei_text <- paste(blei_text, collapse = " ")
blei_text <- clean_texts(blei_text)
blei_stylo <- stylo::txt.to.words.ext(blei_text, corpus.lang = "German")

# use moving window to create multiple selections of 500-word-long texts
blei_stylo_mw <- list()

for(i in 1:ceiling((length(blei_stylo)/50))){
  
  blei_stylo_mw[[i]] <- blei_stylo[((i-1)*50)+(1:500)]
  
}

# 2. Musil

musil_text <- paste(musil_text, collapse = " ")
musil_text <- clean_texts(musil_text)
musil_stylo <- stylo::txt.to.words.ext(musil_text, corpus.lang = "German")

# use moving window to create multiple selections of 500-word-long texts
musil_stylo_mw <- list()

for(i in 1:ceiling((length(musil_stylo)/50))){
  
  musil_stylo_mw[[i]] <- musil_stylo[((i-1)*50)+(1:500)]
  
}

# 3. Guetersloh

guetersloh_text <- paste(guetersloh_text, collapse = " ")
guetersloh_text <- clean_texts(guetersloh_text)
guetersloh_stylo <- stylo::txt.to.words.ext(guetersloh_text, corpus.lang = "German")

# use moving window to create multiple selections of 500-word-long texts
guetersloh_stylo_mw <- list()

for(i in 1:ceiling((length(guetersloh_stylo)/50))){
  
  guetersloh_stylo_mw[[i]] <- guetersloh_stylo[((i-1)*50)+(1:500)]
  
}

# 4. Kisch

kisch_text <- paste(kisch_text, collapse = " ")
kisch_text <- clean_texts(kisch_text)
kisch_stylo <- stylo::txt.to.words.ext(kisch_text, corpus.lang = "German")

# use moving window to create multiple selections of 500-word-long texts
kisch_stylo_mw <- list()

for(i in 1:ceiling((length(kisch_stylo)/50))){

  kisch_stylo_mw[[i]] <- kisch_stylo[((i-1)*50)+(1:500)]

}

# create dev corpus (multiple text selections per author) 

mw_corpus <- list(Musil = musil_stylo_mw, Blei = blei_stylo_mw, Guetersloh = guetersloh_stylo_mw, Kisch = kisch_stylo_mw)

# prepare final results container

filename <- "imposters_testing_refine__results.txt"

cat("Imposters results\n\n", file = filename)

# main loop for analysis 

for(method in 1:length(methods_combination$Var1)){
  
  cat("\n\nConfiguration:", methods_combination$Var1[method], "imposters", methods_combination$Var2[method], "MFW", methods_combination$Var3[method], "-- imposters group", methods_combination$Var4[method], "\n\n", file = filename, append = T)
  
  cat("Imposters:", validation_imposters[[methods_combination$Var4[method]]], "\n\n", sep = "\n", file = filename, append = T)
  
  imposters_selection <- which(all_imposters %in% validation_imposters[[methods_combination$Var4[method]]])
    
  full_corpus_tmp <- full_corpus[imposters_selection]

  ### run imposters analysis
  
  for(validation in 1:validation_rounds){
    
    cat("\n\nValidation", validation, "\n\n", file = filename, append = T)
    
    # find authors' texts
    author_corpus <- list()
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
      if(dim(data)[2] > methods_combination$Var2[method])
        data <- data[,1:methods_combination$Var2[method]]
      
      # who wrote the test text? (in my case, this is the 1st row in the table):
      imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = methods_combination$Var3[method])
      
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
      if(dim(data)[2] > methods_combination$Var2[method])
        data <- data[,1:methods_combination$Var2[method]]
      
      # who wrote the test text? (in my case, this is the 1st row in the table):
      imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = methods_combination$Var3[method])
      
      cat("Not ", my_candidate, " probability: ", imposters_results, "\n", file = filename, sep = "", append = T)
      
      convalidations[[i]] <- c(convalidations[[i]], imposters_results)
      
      cat("method:", method/length(methods_combination$Var1), "validation:", validation/validation_rounds, "\n", file = "progress.log")
      
    }
    
  }

  save(verifications, convalidations, methods_combination, my_passage_length, validation_rounds, validation_imposters, file = gsub(".txt", ".RData", filename))
    
}

