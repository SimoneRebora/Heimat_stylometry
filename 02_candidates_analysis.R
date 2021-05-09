### candidates_analysis

library(stylo)
library(stringr)

# define selected methods
selected_methods <- read.csv("features/02_candidates_analysis_features.csv", stringsAsFactors = F)

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

# load Heimat texts

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

# prepare reference corpora per author

author_reference <- list()

for(i in 1:length(my_authors_sel)){
  
  # prepare corpora per author
  author_texts <- which(my_authors == my_authors_sel[i])
  author_reference[[i]] <- unlist(full_corpus[author_texts])
  
}

names(author_reference) <- my_authors_sel

# prepare final results container

final_attributions <- list()

filename <- paste("candidates_analysis__", 
                  length(heimat_stylo), "texts__",
                  n_candidates, "candidates__", 
                  length(selected_methods$MFW), "methods__",
                  "centroidFALSE",
                  ".RData", sep = "")

# main loop for analysis (works on the Heimat articles)

for(text_id in 1:length(heimat_stylo)){
  
  test_set_corpus <- list(test = heimat_stylo[[text_id]])
  final_attributions[[text_id]] <- selected_methods
  final_attributions[[text_id]]$attribution <- ""
  
  for(method in 1:length(selected_methods$MFW)){
    
    stylo_results <- stylo(gui = FALSE, corpus.lang="German", analysis.type="CA", mfw.min=selected_methods$MFW[method], mfw.max=selected_methods$MFW[method], mfw.incr=0, distance.measure=selected_methods$distance[method], culling.min=selected_methods$culling[method], culling.max=selected_methods$culling[method], write.pdf.file = FALSE, parsed.corpus = c(test_set_corpus, author_reference))
    
    # check if culling reduced MFW below the expected value (thus causing a repeated analysis)
    if(length(stylo_results$features.actually.used) < selected_methods$MFW[method] - (selected_methods$MFW[2] - selected_methods$MFW[1]))
      next
    
    stylo_final_results <- stylo_results$distance.table[1,]
    
    final_attributions[[text_id]]$attribution[method] <- names(which.min(stylo_final_results[2:(n_candidates+1)]))
    
    cat("text:", text_id, "method:", method/length(selected_methods$MFW), "\n", file = "progress.log")
    
  }
  
  test_df <- final_attributions[[text_id]]
  table(test_df$attribution)
  
  cat("##################", text_id, "##############\n\n\n")
  
  unlink("*_EDGES.csv")
  
  save.image(filename)
  
}

# write results in txt file

outfile <- "candidates_analysis__results.txt"

features <- gsub(".RData", "", filename)
features <- unlist(strsplit(features, "__"))

cat(features, "\n", sep = "\n", file = outfile)

for(i in 1:length(final_attributions)){
  
  cat("#####################\n\nText ", i, "\nIssue ", metadata$issue[i], " Article no. ", metadata$article[i], "\nTitle: ", metadata$title[i], "\n\nPossible authors\n", sep = "", file = outfile, append = T)
  
  tmp_df <- final_attributions[[i]]
  
  tmp_df <- tmp_df[which(tmp_df$attribution != ""),]
  
  for(my_author in my_authors_sel){
    
    probability <- length(which(tmp_df$attribution == my_author)) / length(tmp_df$MFW) * 100
       
    cat(my_author, ": ", round(probability, digits = 2), "%\n", sep = "", file = outfile, append = T)
    
  }
  
  cat("\n", file = outfile, append = T)
  
}


print("Process complete!")
