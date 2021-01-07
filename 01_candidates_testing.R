### candidates_testing

library(stylo)
library(stringr)

# define methods to be tested
selected_methods <- expand.grid(MFW = (1:30)*10, distance = c("dist.manhattan", "dist.euclidean", "dist.delta", "dist.eder", "dist.canberra", "dist.wurzburg", "dist.argamon", "dist.cosine", "dist.entropy", "dist.minmax", "dist.simple"), culling = 100, centroid = c(T, F), stringsAsFactors = FALSE)
selected_methods <- rbind(selected_methods, expand.grid(MFW = (1:20)*100, distance = c("dist.manhattan", "dist.euclidean", "dist.delta", "dist.eder", "dist.canberra", "dist.wurzburg", "dist.argamon", "dist.cosine", "dist.entropy", "dist.minmax", "dist.simple"), culling = c(50,0), centroid = c(T, F), stringsAsFactors = FALSE))

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

final_attributions <- list()

filename <- paste("candidates_testing__", 
                  length(selected_methods$MFW), "methods",
                  ".RData", sep = "")

# main loop for analysis (works on the four authors)

for(text_id in 1:4){

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
