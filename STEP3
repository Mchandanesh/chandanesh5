
#GLOBAL.R
## Allocate memory
options(java.parameters = "-Xmx10g")


#load required packages##################################################
library(dplyr)
library(SnowballC)
library(slam)
library(RWeka)
library(Matrix)
library(rvest)
library(XML)
library(stringr)
library(stringi)
library(tidyverse)
library(stringr)
library(tm)
library(udpipe)
library(text2vec)
library(glmnet)
library(caret)
library(caTools)
library(tidytext)
library(widyr)
library(ggraph)
library(ggforce)
library(igraph)
library(syuzhet)
library(igraph)
library(shinycssloaders)
library(wordcloud2)
library(shiny)
library(shinythemes)
#load required packages##################################################
packages_needed <- c('dplyr','SnowballC','slam','tm',
                      'RWeka','Matrix','rvest',
                      'XML','stringr','stringi',
                      'tidyverse','stringr', 
                      'udpipe','text2vec','glmnet',
                      'caret', 'caTools','tidytext',
                      'widyr','ggraph', 'ggforce',
                      'igraph','syuzhet','igraph', 
                      'shinycssloaders', 'wordcloud2',
                     'shiny', 'shinythemes')
 for (i in packages_needed){
   if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
   require(i, character.only=TRUE)
 }


#chunk_into_sentences######################################################
# input: review column
# output: sublists of review, split in to sentences .!?,<>
chunk_into_sentences <- function(text) {
  break_points <- c(1, as.numeric(gregexpr('[[:alnum:]][.!?,<>()]', text)[[1]]) + 1)
  sentences <- NULL
  for(i in 1:length(break_points)) {
    res <- substr(text, break_points[i], break_points[i+1]) 
    if(i>1) { sentences[i] <- sub('.', '', res) } else { sentences[i] <- res }
  }
  sentences <- sentences[sentences=!is.na(sentences)]
  if(length(break_points)==2) {sentences<-text}
  return(sentences)
}
#https://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences


#cleanup_function_before######################################################
# input: Splitted review, list of items to delete the sentence before the keyword (however, but)
# output: Review list without the sentences before the keyword
cleanup_function_before <- function(this_review_splited, list_delete_before){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_before)){
    m <- grepl(list_delete_before[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!lead(logical_list,default = FALSE)]
  #return_review <- paste(return_review, collapse = ' ') # combine sentences list into one review
  return(return_review)
}


#cleanup_function_after######################################################
# input: Splitted review, list of items to delete the sentence after the keyword (despite, in spite of)
# output: Review list without the sentences after the keyword
cleanup_function_after <- function(this_review_splited, list_delete_after){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_after)){
    m <- grepl(list_delete_after[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!lag(logical_list,default = FALSE)]
   #return_review <- paste(return_review, collapse = ' ')# combine sentences list into one review
  return(return_review)
}

#cleanup_function_this_sentence######################################################
# input: Splitted review, list of items to delete the sentence if the keyword occurs (while)
# output: Review list without the sentences if the keyword occurs in the sentence
cleanup_function_this_sentence <- function(this_review_splited, list_delete_this_sentence){
  text <- tolower(this_review_splited)
  text <- str_replace(text, "[.!?,]", "")
  logical_list<-rep(0, length(text))
  for (i in 1:length(list_delete_this_sentence)){
    m <- grepl(list_delete_this_sentence[i], text)
    logical_list<-logical_list|m
  }
  return_review <-this_review_splited[!logical_list]
 # return_review <- paste(return_review, collapse = ' ')# combine sentences list into one review
  return(return_review)
}


#func_replace_emoji######################################################
# input: Splitted review, table to replace the emoticons with the emotion description
# output: Review list with emotion description instead of emoticons
func_replace_emoji<- function(text, emoji_table){
  clean_text <-stri_replace_all_fixed(text,
                                      pattern = emoji_table$emoji.chars,
                                      replacement = paste(emoji_table$emoji.descriptions," "),
                                      vectorize_all=FALSE)
  return(clean_text)
}


#elongated_words2######################################################
# input: Single word
# output: Boolean value if a word is elongated or not (Eg: Aweeesomeeee)
# Rule: A character coming together more than twice
elongated_words2 <- function(my_str){
  temp <- strsplit(my_str, "")[[1]]
  
  for(i in 1:length(temp)){
    count = 1
    if(i+1 <= length(temp)){
      for(j in (i+1):length(temp)){
        if(temp[i] == temp[j]){
          count = count + 1
          if(count>=3){
            return(T)
          }
        } else{
          break
        }
      }
    }
    
  }
  return(F)
  
}

#clean_elongated_words2######################################################
# input: Single word
# output: Clean elongated word (Eg: Aweeeeesome will become Aweesome)
# Rule: Reduce elongations with 2 character repetitions together only
clean_elongated_words2 <- function(my_str){
  temp <- strsplit(my_str, "")[[1]]
  my_word <- NA
  for(i in 1:length(temp)){
    if(is.na(my_word)){
      my_word <- temp[i]
    }
    if((i+1) <= length(temp)){
      if(temp[i] != temp[i+1]){
        my_word <- paste(c(my_word, temp[i+1]), collapse = "")
      } else if((i-1) >= 1){
        if(temp[i] != temp[i-1]){
          my_word <- paste(c(my_word, temp[i+1]), collapse = "")
        }
      }
      
    }
    
  }
  return(my_word)
  
}


#correct######################################################
# input: Single word
# output: Correct the spelling from dictionary (Eg: Aweesome will become Awesome)
# Rule: Closest distant word
correct <- function(word) {
  
  word = tolower(word)
  edit_dist <- adist(word, wordlist)
  c(wordlist[edit_dist <= min(edit_dist,2)],word)[1]
}


#clean_words######################################################
# input: Words in the review
# output: Rating value written in review comment on 10 (Eg: 3/5 will return 6)
clean_words <- function(words){
  
  num1 <- numeric(length(words))
  num2 <- numeric(length(words))
  
  for(j in 1:length(words)){
    words[j] <- gsub("[a-z]|[A-Z]", "" ,words[j])
    words[j] <- gsub("\\,", "." ,words[j])
    
    while((!grepl("^[0-9]",words[j])) | (!grepl("[0-9]$",words[j]))){
      words[j] <- gsub("^[[:punct:]]", "" ,words[j])
      words[j] <- gsub("[[:punct:]]$", "" ,words[j])
    }
    
    temp1 <- unlist(strsplit(words[j],'/'))[1]
    temp2 <- unlist(strsplit(words[j],'/'))[2]
    
    
    if(suppressWarnings(!is.na(as.numeric(temp1)))){
      num1[j] <- as.numeric(temp1)
    } else{
      
      temp1 <- unlist(strsplit(temp1,'&'))
      temp1 <- unlist(strsplit(temp1,'-'))
      temp1 <- unlist(strsplit(temp1,'$'))
      temp1 <- unlist(strsplit(temp1,'@'))
      temp1 <- unlist(strsplit(temp1,'!'))
      temp1 <- unlist(strsplit(temp1,'%'))
      temp1 <- unlist(strsplit(temp1,'\\('))
      temp1 <- unlist(strsplit(temp1,'\\+'))
      temp1 <- unlist(strsplit(temp1,'\\)'))
      temp1 <- unlist(strsplit(temp1,'\\*'))
      num1[j] <- mean(as.numeric(temp1))
    }
    
    if(suppressWarnings(!is.na(as.numeric(temp2)))){
      num2[j] <- as.numeric(temp2)
    } else{
      
      temp2 <- substr(temp2, 1, 2)
      num2[j] <- ifelse(temp2 != "10", as.numeric(substr(temp2,1,1)), as.numeric(temp2))
    }
  }
  
  res <- sum(num1)*10/sum(num2)
  
  #if(res > 10 | is.nan(res)){
  #  return(NA)
  #} else{
    #return(res)
  #}
  
#}
  
  if (is.na(res) | res > 10) {
    # Code to execute if the condition is TRUE
    return(TRUE)
  } else {
    # Code to execute if the condition is FALSE
    return(FALSE)
  }

}
#IsWordCap######################################################
# input: Single word
# output: Boolean value for capital words (Eg: COOL will return True)
IsWordCap<-function(word){
  if (word==toupper(word) & is.na(as.numeric(word)) & nchar(word)>=2 )
  {
    out<-TRUE
  }
  else {out<-FALSE}
  return(out)
}

#CountWordsCap######################################################
# input: Whole review
# output: Count all capital words (Eg: The movie was COOL and FABULOUS will return 2)
CountWordsCap<-function(review){
  review_clean<- str_replace_all(review, "[^[:alnum:]]", " ")
  review_split <- str_split(review_clean, boundary("word"))
  tf_vector<-unlist(sapply(unlist(review_split), IsWordCap))
  n<-sum(tf_vector)
  return(n)
}

#LoadToEnvironment######################################################
# input: RData to be loaded, in the new environment
# output: env with Rdata
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}


#clean_reviews######################################################
# input: Review data with text column for reviews
# output: Multiple features added to the data as follow:
# elongated_words_freq - Elongated word frequency
# rating_words_value - Rating of movie written in the review comment
# elongated_sentiment - Average intensified valence for the elongated words
# exclamed_sentiment - Average intensified valence for the exclaimed words
# is_elongated - Boolean (If the review has elongated words)
# is_exclaimed - Boolean (If the review has exclaimed words)
# capital_freq - Capital words frequency
# is_capital - Boolean (If the review has capital words)
clean_reviews <- function(data, wordlist){
  
  mycorpus <- VCorpus(VectorSource(data$text))
  corpus_frame <- data.frame(text=unlist(sapply(mycorpus, `[`, "content")), stringsAsFactors=F)
  
  list_delete_before <- c("\\<however\\>","\\<but\\>")
  
  list_delete_after <- c("\\<despite\\>","\\<in spite of\\>")
  
  list_delete_this_sentence <- c("\\<while\\>")
  
  # replace emoji characters with their descriptions
  # https://unicode.org/Public/emoji/12.0/emoji-test.txt
  emoji_table<- read.csv("emoji_table.csv", header = TRUE)
  
  #run functions################################################################
  raw_review <- corpus_frame[,1]
  corpus_frame_split <- lapply(raw_review,chunk_into_sentences)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_before, list_delete_before)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_after, list_delete_after)
  corpus_frame_split <- lapply(corpus_frame_split,cleanup_function_this_sentence, list_delete_this_sentence)
  #combine sublists (sentences) back into one element per review
  clean_review <- lapply(corpus_frame_split, paste, collapse = ' ')
  #replace emoji
  clean_review <- lapply(clean_review, func_replace_emoji, emoji_table)
  #save output file
  mycorpus_clean <- VCorpus(VectorSource(clean_review))
  corpus_frame_clean <- data.frame(text=unlist(sapply(mycorpus_clean, `[`, "content")), stringsAsFactors=F)
  
  corpus_frame_clean$ratings <- data$ratings
  corpus_frame_clean$sentiment <- data$count_pos
  corpus_frame_clean$sentiment <- NULL
  
  corpus_frame_clean$elongated_words <- NA
  corpus_frame_clean$cleaned_elongated_words <- NA
  corpus_frame_clean$elongated_words_freq <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    words <- unlist(strsplit(corpus_frame_clean$text[i],' '))
    
    # Removes punctuations
    words = gsub("[[:punct:]]", " ", words)
    # Removes numbers
    words = gsub("[[:digit:]]", " ", words)
    
    words = gsub("\\s+"," ",words)
    
    words <- unlist(strsplit(words,' '))
    
    words <- sapply(words, elongated_words2)
    words <- names(words)[words == T]
    
    words <- tolower(words)
    
    cleaned_elongated_words <- sapply(words, clean_elongated_words2)
    
    #Concatenate back to a string
    corpus_frame_clean$elongated_words[i] <-
      paste(words, collapse=" ")
    
    corpus_frame_clean$cleaned_elongated_words[i] <-
      paste(cleaned_elongated_words, collapse=" ")
    
    corpus_frame_clean$elongated_words_freq[i] <-
      length(words)
  }
  
  corpus_frame_clean$Spelled_elongated_words <- NA
  
  
  for (i in 1:length(corpus_frame_clean$cleaned_elongated_words)){
    words <- unlist(strsplit(corpus_frame_clean$cleaned_elongated_words[i],' '))
    words <- as.character(sapply(words,correct))
    
    #Concatenate back to a string
    corpus_frame_clean$Spelled_elongated_words[i] <-
      paste(words, collapse=" ")
  }
  
  
  corpus_frame_clean$rating_words <- NA
  corpus_frame_clean$rating_words_value <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(grepl("+[0-9]/[0-9]+", corpus_frame_clean$text[i])){
      words <- unlist(strsplit(corpus_frame_clean$text[i],' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    if(grepl("+[0-9] out of [0-9]+", corpus_frame_clean$text[i]
             ,ignore.case = T)){
      
      body_comment <- gsub(" out of ","/", corpus_frame_clean$text[i])
      
      words <- unlist(strsplit(body_comment,' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    
    if(grepl("+(two|three|four|five|six|seven|eight|nine|ten) out of (two|three|four|five|six|seven|eight|nine|ten)+", corpus_frame_clean$text[i]
             ,ignore.case = T)){
      
      body_comment <- gsub(" out of ","/", corpus_frame_clean$text[i])
      body_comment <- gsub("one","1", body_comment, ignore.case = T)
      body_comment <- gsub("two","2", body_comment, ignore.case = T)
      body_comment <- gsub("three","3", body_comment, ignore.case = T)
      body_comment <- gsub("four","4", body_comment, ignore.case = T)
      body_comment <- gsub("five","5", body_comment, ignore.case = T)
      body_comment <- gsub("six","6", body_comment, ignore.case = T)
      body_comment <- gsub("seven","7", body_comment, ignore.case = T)
      body_comment <- gsub("eight","8", body_comment, ignore.case = T)
      body_comment <- gsub("nine","9", body_comment, ignore.case = T)
      body_comment <- gsub("ten","10", body_comment, ignore.case = T)
      
      words <- unlist(strsplit(body_comment,' '))
      words <- words[grepl("*[0-9]/[0-9]*", words)]
      
      corpus_frame_clean$rating_words_value[i] <- clean_words(words)
      
      corpus_frame_clean$rating_words[i] <-
        paste(words, collapse=" ")
      
    }
    
    
  }
  
  
  corpus_frame_clean$exclamated_sentences <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    sentences <- unlist(strsplit(corpus_frame_clean$text[i],'\\.'))
    
    sentences <- gsub("([!\\s])\\1+", "\\1", sentences, perl=TRUE)
    
    sentences <- unlist(strsplit(sentences,'(?<=[!])', perl = T))
    
    sentences <- sentences[grepl("+\\!+",sentences)]
    
    corpus_frame_clean$exclamated_sentences[i] <-
      paste(sentences, collapse=" .")
    
  }
  
  CommentsText <- sapply(corpus_frame_clean$exclamated_sentences
                         ,function(x) iconv(x, 'utf8', 'ascii',""))
  
  text <- tolower(CommentsText)
  
  ud_model <- udpipe_load_model("english-ewt-ud-2.3-181115.udpipe")
  
  pos_tokens <- udpipe_annotate(ud_model, x =text)
  pos_tokens <- as.data.frame(pos_tokens)
  pos_tokens <- subset(pos_tokens, pos_tokens$upos %in% c("ADJ", "VERB"))
  
  pos_tokens$doc_id <- gsub("doc","", pos_tokens$doc_id)
  pos_tokens$doc_id <- as.numeric(pos_tokens$doc_id)
  
  corpus_frame_clean$exclamated_words <- ""
  
  for(i in unique(pos_tokens$doc_id)){
    
    words <- subset(pos_tokens$lemma, pos_tokens$doc_id == i)
    
    corpus_frame_clean$exclamated_words[i] <-
      paste(words, collapse=" ")
  }
  
  
  
  
  dictionary <- read.csv("SentimentDictionary.csv")
  dictionary[,2:4] <- sapply(dictionary[,2:4],function(x) x-5)
  
  corpus_frame_clean$elongated_sentiment <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(corpus_frame_clean$Spelled_elongated_words[i] != ""){
      
      split <- strsplit(corpus_frame_clean$Spelled_elongated_words[i],split=" ")[[1]] 
      
      m <- match(split, dictionary$Word)
      
      present <- !is.na(m)
      
      wordvalences <- dictionary$VALENCE[m[present]] * 2
      
      corpus_frame_clean$elongated_sentiment[i] <- mean(wordvalences, na.rm=TRUE)
      
      if (is.na(corpus_frame_clean$elongated_sentiment[i])) 
        corpus_frame_clean$elongated_sentiment[i] <- 0 
      else corpus_frame_clean$elongated_sentiment[i] <- corpus_frame_clean$elongated_sentiment[i]
    }
  }
  
  
  corpus_frame_clean$exclamed_sentiment <- NA
  
  for (i in 1:nrow(corpus_frame_clean)){
    
    if(corpus_frame_clean$exclamated_words[i] != ""){
      
      split <- strsplit(corpus_frame_clean$exclamated_words[i],split=" ")[[1]] 
      
      m <- match(split, dictionary$Word)
      
      present <- !is.na(m)
      
      wordvalences <- dictionary$VALENCE[m[present]] * 2
      
      corpus_frame_clean$exclamed_sentiment[i] <- mean(wordvalences, na.rm=TRUE)
      
      if (is.na(corpus_frame_clean$exclamed_sentiment[i])) 
        corpus_frame_clean$exclamed_sentiment[i] <- 0 
      else corpus_frame_clean$exclamed_sentiment[i] <- corpus_frame_clean$exclamed_sentiment[i]
    }
  }
  
  
  corpus_frame_clean$elongated_words <- NULL
  corpus_frame_clean$cleaned_elongated_words <- NULL
  corpus_frame_clean$Spelled_elongated_words <- NULL
  corpus_frame_clean$rating_words <- NULL
  corpus_frame_clean$exclamated_sentences <- NULL
  corpus_frame_clean$exclamated_words <- NULL
  corpus_frame_clean$ratings <- NULL
  
  corpus_frame_clean$is_elongated <- ifelse(corpus_frame_clean$elongated_words_freq > 0, 1, 0)
  corpus_frame_clean$is_rated <- ifelse(!is.na(corpus_frame_clean$rating_words_value),1, 0)
  corpus_frame_clean$is_exclaimed <- ifelse(!is.na(corpus_frame_clean$exclamed_sentiment),1, 0)
  
  corpus_frame_clean[is.na(corpus_frame_clean)] <- 0
  
  corpus_frame_clean <- corpus_frame_clean[!is.na(corpus_frame_clean$text),]
  
  review_comments<-corpus_frame_clean$text
  FreqCapWords<-sapply(review_comments,CountWordsCap)
  
  corpus_frame_clean$capital_freq <- as.vector(FreqCapWords)
  corpus_frame_clean$is_capital <- ifelse(corpus_frame_clean$capital_freq > 0, 1, 0)
  
  return(corpus_frame_clean)
  
}

raw_data <- read.csv("D:/jntua_project/my/all_reviews_movie_0-1866.csv")

data <- data.frame(text = raw_data$comments_body)

load("wordListSpelling.Rdata")

data <- clean_reviews(data, wordlist)

data <- cbind(data, sentiment = raw_data$count_pos)

write.csv(data, file = "corpus_preprocessed1.csv", row.names = F)
