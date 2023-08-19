

library(tidytext)
library(dplyr)
library(ggplot2)

#read in review data 
Reviews <- read.csv("D:/jntua_project/my/all_reviews_movie_0-1866.csv")
Reviews <- select(Reviews, X, ratings, comments_body)
Review_Polarity <- Reviews %>% mutate(Polarity = if_else(ratings > 5, "Positive", "Negative"))

Review_Polarity <- Review_Polarity[, c(1,3:4)]


#frequency plot of positive and negative reviews
barplot(table(Review_Polarity$Polarity))


#nrc (Mohammad and Turney 2013) categorizes 13,901 terms as positive or negative and/or by emotion, (trust, fear, sadness)
get_sentiments(lexicon="nrc")

#bing lexicon (Hu and Liu 2014) includes 6,788 words clasified as positive or negative
get_sentiments(lexicon="bing")

#AFINN lexicon (Nielsen 2011) assigns numerical sentiment scores (-5 to 5) to 2,476 terms
get_sentiments(lexicon="afinn")


#unnest_tokens() removes punctuation, changes to lowercase. Tokenizes sentences based on words
tidy_Reviews <- Review_Polarity %>% unnest_tokens(word, comments_body)


#we consider removing stopwords, using stop_words list in tidytext package
#we see if any words exist in the stop words list and atleast one of the lexicon list.
#we do this check with nrc, bing, afinn lexicons.

stop_lexicon_match <- stop_words %>% inner_join(sentiments %>% filter(lexicon=="bing" | lexicon=="nrc" | lexicon=="AFINN"), by="word")

#unique stop words that exist on both the stop words list and nrc, bing or afinn lexicon lists
length(unique(stop_lexicon_match$word))
#based on 78 stop words that are part of sentiment list we will not remove stop words in our analysis.

#nrc sentiment
nrc_sentiment <- tidy_Reviews %>% inner_join(get_sentiments("nrc"), by="word")


#bing sentiment
bing_sentiment <- tidy_Reviews %>% inner_join(get_sentiments("bing"), by="word")


#afinn sentiment
afinn_sentiment <- tidy_Reviews %>% inner_join(get_sentiments("afinn"), by="word")


#nrc sentiment we take words rated as positive or negative and not neutral ones 
nrc_sentiment <- nrc_sentiment[nrc_sentiment$sentiment %in% c("positive", "negative"), ]


# aggregate review-level polarity information. Each review is scored as a sum of its points.
#for bing and nrc lexicons we count each positive as +1 and negative as -1.

nrc_sentiment$score <- ifelse(nrc_sentiment$sentiment == "negative", -1, 1)
nrc_aggregate <- nrc_sentiment %>% select(X, score) %>% group_by(X) %>% summarise(nrc_score = sum(score))


bing_sentiment$score <- ifelse(bing_sentiment$sentiment == "negative", -1, 1)
bing_aggregate <- bing_sentiment %>% select(X, score) %>% group_by(X) %>% summarise(bing_score = sum(score))


# sum scores for afinn
afinn_aggregate <- afinn_sentiment %>% select(X, score) %>% group_by(X) %>% summarise(afinn_score = sum(score))


#we aggregate review level and add sentiment information to original reviews dataset. 
#includes one column for each lexicon to make it easy to judge which scored the review sentiments best.

Review_Polarity_final <- merge(x=Review_Polarity, y= nrc_aggregate, all.x=TRUE, by="X")
Review_Polarity_final <- merge(x=Review_Polarity_final, y= bing_aggregate, all.x=TRUE, by="X")
Review_Polarity_final <- merge(x=Review_Polarity_final, y= afinn_aggregate, all.x=TRUE, by="X")


#to label whether review is positive or negative according to each lexicon
Review_Polarity_final$afinn_judgement <- ifelse(Review_Polarity_final$afinn_score < 0, "negative", 
                                                ifelse(Review_Polarity_final$afinn_score > 0, "positive", "neutral"))

Review_Polarity_final$bing_judgement <- ifelse(Review_Polarity_final$bing_score < 0, "negative", 
                                               ifelse(Review_Polarity_final$bing_score > 0, "positive", "neutral"))

Review_Polarity_final$nrc_judgement <- ifelse(Review_Polarity_final$nrc_score < 0, "negative", 
                                              ifelse(Review_Polarity_final$nrc_score > 0, "positive", "neutral"))

#Sentiment Analysis Results

#nrc # correctly identifies 81% of positive and 41% of negative reviews
table(Review_Polarity_final$nrc_judgement, Review_Polarity_final$Polarity)

#bing # correctly identifies 77% of +ve reviews and 60% of negative reviews
table(Review_Polarity_final$bing_judgement, Review_Polarity_final$Polarity)

#afinn # correctly identifies 86% of +ve reviews and 53%% of negative reviews
table(Review_Polarity_final$afinn_judgement, Review_Polarity_final$Polarity)
