

########################################################################
# This script is to run the Chosen model.
# input: corpus_preprocessed.csv
# output: trained GLM Net model with AUC score
########################################################################

#load required packages##################################################
library(text2vec)
library(glmnet)
library(dplyr)
library(caret)
library(caTools)
library(stringr)
library(caret)
library(ROCR)

### Step 1:load input data ######
data = read.csv("corpus_preprocessed1.csv",sep=",",stringsAsFactors = FALSE)

data[is.na(data)] <- 0
data$ratings <- NULL

### Step 2:Split the data into training and testing ######
set.seed(2)
spl <- sample.split(data$sentiment, SplitRatio = 0.5)

train <- subset(data, spl == T)
test <- subset(data, spl == F)  

#write.csv(train,"train.csv", row.names = F)
write.csv(test,"test.csv", row.names = F)


### Step 3:Pre-processing data (Creating independent variables) ######

#model_creation######################################################
# input: training data
# output: List of variables which will be used to transfer information to the testing data
# tfidf_model- TFIDF model used while training, vectorizer- Training vocabulary word vector, m_lsa- LSA model used while training, train_tfidf- Training TFIDF
model_creation <- function(train){
  
  train$text <- train %$%
    str_to_lower(text) %>%
    str_replace_all("[^[:alpha:]]", " ") %>%
    str_replace_all("\\s+", " ")
  
  train_tokens <- itoken(train$text, 
                         preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  vectorizer <- create_vocabulary(train_tokens, ngram = c(1, 1), stopwords = stopwords::stopwords("en")) %>%
    prune_vocabulary(term_count_min = 3, doc_proportion_max = 0.5, vocab_term_max = 4000) %>%
    vocab_vectorizer()
  
  train_dtm <- create_dtm(train_tokens, vectorizer)
  tfidf_model <- TfIdf$new(norm = "l2", sublinear_tf = T)
  train_tfidf <- fit_transform(train_dtm, tfidf_model)
  
  m_lsa <- LSA$new(n_topics = 25)
  lsa <- fit_transform(train_tfidf, m_lsa)
  
  return(list(tfidf_model, vectorizer, m_lsa, train_tfidf))
}

#create_features_from_model######################################################
# input: data- Data frame with reviews, model- TFIDF model used while training, vectorizer- Training vocabulary word vector, lsa- LSA model used while training
# output: TFIDF, LSA and all the features combind in a matrix which is used for modelling
create_features_from_model <- function(data, model, vectorizer, lsa){
  
  data$text <- data %$%
    str_to_lower(text) %>%
    str_replace_all("[^[:alpha:]]", " ") %>%
    str_replace_all("\\s+", " ")
  
  data_tokens <- itoken(data$text, 
                        preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  data_dtm <- create_dtm(data_tokens, vectorizer)
  data_tfidf <- transform(data_dtm, model)
  
  data_lsa <- transform(data_tfidf, lsa)
  
  res <- data %>%
    select(-c(text,sentiment)) %>%
    sparse.model.matrix(~ . - 1, .) %>%
    cbind(data_tfidf, data_lsa)
  
  return(res)
}

## Save TR Model for shiny app
tr_model <- model_creation(train)
save(tr_model, file= "tr_model.Rdata")

train_indep <- create_features_from_model(train, model = tr_model[[1]], vectorizer = tr_model[[2]], lsa = tr_model[[3]])
test_indep <- create_features_from_model(test, model = tr_model[[1]], vectorizer = tr_model[[2]], tr_model[[3]])


### Step 4:Training the GLM Net model ######
glmnet_clas <- cv.glmnet(x = train_indep, y = train[['sentiment']], 
                         family = 'binomial', alpha = 1, type.measure = "auc", nfolds = 4, thresh = 1e-3, maxit = 1e3)

## Save glmnet_clas model for shiny app
save(glmnet_clas, file= "glmnet_clas.Rdata")

plot(glmnet_clas)
# Max train AUC
print(paste("max AUC =", round(max(glmnet_clas$cvm), 4)))

### Step 5:Predict using the testing data ######
pred <- predict(glmnet_clas, test_indep, type = 'response')[,1]

# Test AUC
glmnet:::auc(test$sentiment, pred)

# ROC curve
predML <- prediction(pred,test$sentiment)
perfML <- performance(predML,"tpr","fpr")
plot(perfML)
abline(0,1)

pred <- as.factor(ifelse(pred>0.5,1,0))
confusionMatrix(pred, as.factor(test$sentiment))
