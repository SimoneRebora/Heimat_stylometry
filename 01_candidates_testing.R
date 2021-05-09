### candidates_testing

library(stylo)
library(stringr)

# define methods to be tested
selected_methods <- read.csv("features/01_candidates_testing_features.csv", stringsAsFactors = F)

# read texts in training set
my_texts <- list.files("training_set/")
my_authors <- strsplit(my_texts, "_")
my_authors <- unlist(lapply(my_authors, function(x) x[1]))
my_authors_sel <- unique(my_authors)
n_candidates <- length(my_authors_sel) 
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
    print("Cleaned")
  }
  return(my_texts)
}

# prepare train corpus

full_corpus <- list()

for(i in 1:length(my_texts)){

  tmp_text <- paste(readLines(my_texts[i]), collapse = " ")
  tmp_text <- clean_texts(tmp_text)
  full_corpus[[i]] <- stylo::txt.to.words.ext(tmp_text, corpus.lang = "German")
  print(i)

}

# prepare reference corpora per author

author_reference <- list()
author_reference_centroid <- list()

for(i in 1:length(my_authors_sel)){
  
  # prepare corpora per author
  author_texts <- which(my_authors == my_authors_sel[i])
  author_reference[[i]] <- unlist(full_corpus[author_texts])
  
  # instance-based corpus (i.e. centroid)
  author_reference_centroid[[i]] <- split(author_reference[[i]], ceiling(seq_along(author_reference[[i]])/5000))
  author_reference_centroid[[i]] <- author_reference_centroid[[i]][-length(author_reference_centroid[[i]])]
  
}

n_texts_per_author <- unlist(lapply(author_reference_centroid, length))
names_tmp <- unlist(mapply(rep, my_authors_sel, n_texts_per_author))
author_reference_centroid <- unlist(author_reference_centroid, recursive = F)
names(author_reference_centroid) <- paste(names_tmp, 1:length(names_tmp), sep = "_")

names(author_reference) <- my_authors_sel

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

final_attributions <- list()

filename <- paste("candidates_testing__", 
                  length(selected_methods$MFW), "methods",
                  ".RData", sep = "")

# main loop for analysis (works on the candidate authors)

for(text_id in 1:length(mw_corpus)){

  final_attributions[[text_id]] <- selected_methods
  final_attributions[[text_id]]$attribution <- ""
  mw_test <- mw_corpus[[text_id]]

  for(method in 1:length(selected_methods$MFW)){

    if(selected_methods$centroid[method]){
      training_set_corpus <- author_reference_centroid
    }else{
      training_set_corpus <- author_reference
    }

    tmp_attribution <- character()

    for(validation in 1:50){

      test_set_corpus <- list(test = mw_test[[sample(1:length(mw_test), 1)]])

      stylo_results <- stylo(gui = FALSE, corpus.lang="German", analysis.type="CA", mfw.min=selected_methods$MFW[method], mfw.max=selected_methods$MFW[method], mfw.incr=0, distance.measure=selected_methods$distance[method], culling.min=selected_methods$culling[method], culling.max=selected_methods$culling[method], write.pdf.file = FALSE, parsed.corpus = c(test_set_corpus, training_set_corpus))

      # check if culling reduced MFW below the expected value (thus causing a repeated analysis)
      if(length(stylo_results$features.actually.used) < selected_methods$MFW[method] - (selected_methods$MFW[which(selected_methods$culling == selected_methods$culling[method])[2]] - selected_methods$MFW[which(selected_methods$culling == selected_methods$culling[method])[1]]))
        next

      stylo_final_results <- stylo_results$distance.table[1,]

      if(selected_methods$centroid[method]){

        stylo_final_results <- 0
        names(stylo_final_results) <- "test"
        ordered_results <- sort(stylo_results$distance.table[1,])

        for(candidate_n in 1:n_candidates){

          chunck_authors <- strsplit(names(ordered_results), "_")
          chunck_authors <- unlist(lapply(chunck_authors, function(x) x[1]))
          my_candidate_ids <- which(chunck_authors == my_authors_sel[candidate_n])[1:3]
          result_per_author <- mean(ordered_results[my_candidate_ids])
          names(result_per_author) <- my_authors_sel[candidate_n]
          stylo_final_results <- c(stylo_final_results, result_per_author)

        }

      }

      tmp_attribution[validation] <- names(which.min(stylo_final_results[2:(n_candidates+1)]))

    }

    if(length(tmp_attribution) == 0)
      next

    tmp_attribution_t <- sort(table(tmp_attribution), decreasing = T)
    final_attributions[[text_id]]$attribution[method] <- names(tmp_attribution_t)[1]

    cat("text:", text_id, "method:", method/length(selected_methods$MFW), "\n", file = "progress.log")

  }

  cat("##################", text_id, "##############\n\n\n")

  unlink("*_EDGES.csv")

  save.image(filename)

}

print("Process complete!")
