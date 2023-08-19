
########################################################################
# This script is to run the baseline model.
# input: all_reviews.csv
# output: trained Random Forest model with AUC score
########################################################################

#load required packages##################################################
setwd("D:/jntua_project/my")
packages_needed <- c('SnowballC','slam','tm',
                     'RWeka','Matrix','rvest',
                     'XML','stringr','stringi',
                     'dplyr','tidyverse')

for (i in packages_needed){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org", quiet=TRUE)
  require(i, character.only=TRUE)
}
### Step 1:load input data ######
data <- read.csv("all_reviews_movie_0-1866.csv")

# First we will delete non-recognizable characters, otherwise the tm package will get in trouble later. 
CommentsText <- sapply(data$comments_body,function(x) iconv(x, 'utf8', 'ascii',""))
comments <- VCorpus(VectorSource(CommentsText))

### Step 2: Pre-processing ######

# We can apply these transformer functions to the entire corpus using a 'mapper', tm_map

comments <- tm_map(comments, removePunctuation)
comments <- tm_map(comments, removeNumbers)
comments <- tm_map(comments, stripWhitespace)

# Create new transformation functions
# we can create new tranformation functions using the content_transformer wrapper

comments <- tm_map(comments,content_transformer(tolower))

#remove stopwords
forremoval <- stopwords('english')

# remove stop words but keep ("no","not","nor")
comments <- tm_map(comments, removeWords,c(forremoval[!forremoval %in% c("no","not","nor")])) 

comments_frame <- data.frame(text=unlist(sapply(comments, `[`, "content")), stringsAsFactors=F)

### Step 3: Structure and apply ML model ######

# 1. create a complete data set
SentimentReal<-data$count_pos
head(SentimentReal) 

# 2. Create a training and test set

set.seed(2) # Set a seed to have the same subsets every time 

# Set sample (stratified)
# Make our dependent variable dichotomous
y <- as.factor(SentimentReal)

levels(y)

# Define proportion to be in training set 
p <- 0.5

# Define observations to be in training set
class1_train <- sample(which(y==as.integer(levels(y)[1])), floor(p*table(y)[1]),replace=FALSE)
class2_train <- sample(which(y==as.integer(levels(y)[2])), floor(p*table(y)[2]),replace=FALSE)

training_locations <- c(class1_train,class2_train) 

# Create the training and test set now
# Store them in a list for easiness

txt_l <- list()
txt_l[[2]] <- list()

txt_l[[1]]<- comments_frame[sort(training_locations),1]
txt_l[[2]]<- comments_frame[-sort(training_locations),1]


# Make our training and test set corpora
for (i in 1:2){
  txt_l[[i]] <- VCorpus(VectorSource((txt_l[[i]])))
}


# 3. Create the term-document matrices
# Remember that we create two different matrices
# the variables of the test matrix should be the same as for the training matrix. 

# create a function that allows to make the training and test set correctly, with the n-grams specified
Ngram <- function(inputset1,inputset2,mindegree,maxdegree){
  # inputset1 = training dataset
  # inputset2 = test dataset
  # mindegree = minimum n-gram
  # maxdegree = maximum n-gram
  
  outputlist <- list()
  
  # training
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mindegree, max = maxdegree))
  tr <- DocumentTermMatrix(inputset1, control = list(tokenize = Tokenizer,
                                                     weighting = function(x) weightTf(x),
                                                     RemoveNumbers=TRUE,
                                                     removePunctuation=TRUE,
                                                     stripWhitespace= TRUE))
  # test
  test <- DocumentTermMatrix(inputset2, control = list(tokenize = Tokenizer,
                                                       weighting = function(x) weightTf(x),
                                                       RemoveNumbers=TRUE,
                                                       removePunctuation=TRUE,
                                                       stripWhitespace= TRUE))
  
  # Apply sparseness reduction 
  # also reducing the number of documents (respondents) because there will be rows which will not have values anymore
  
  #tr <- removeSparseTerms(tr,0.9999)
  
  # Reform the test DTM to have the same terms as the training case 
  # Remember that, because we will use a model, that our test set should contain the same elements as our training dataset
  Intersect <- test[,intersect(colnames(test), colnames(tr))]
  diffCol <- tr[,setdiff(colnames(tr),colnames(test))]
  newCols <- as.simple_triplet_matrix(matrix(0,nrow=test$nrow,ncol=diffCol$ncol))
  newCols$dimnames <- diffCol$dimnames
  testNew<-cbind(Intersect,newCols)
  testNew<- testNew[,colnames(tr)]
  
  ## Convert term document matrices to common sparse matrices to apply efficient SVD algorithm
  
  dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}
  
  outputlist<- list(train=dtm.to.sm(tr),test=dtm.to.sm(testNew))
  
  return(outputlist)
}

# apply our function
# we store this in a new list, unigram

unigram <-Ngram(txt_l[[1]],txt_l[[2]],1,1)

# 4. Apply Singular Value Decomposition
# SVD will help to reduce this to a selected number of terms
# Note that we implemented an approximation with the package irlba, since the 'normal' svd gets stuck with very large datasets

if (!require("irlba")) install.packages("irlba", quiet=TRUE) ; require("irlba")

SVD_all <- function(inputset,k){
  outputlist <- list()
  
  outputlist[[i]]<-list()
  
  ### specify 'k' concepts, apply to training dataset
  trainer <- irlba(t(inputset[[1]]), nu=k, nv=k)
  
  ### apply the same training concepts to testing dataset
  tester <- as.data.frame(as.matrix(inputset[[2]] %*% trainer$u %*%  solve(diag(trainer$d))))
  
  outputlist<- list(train = as.data.frame(trainer$v), test= tester)
  
  return(outputlist)
}

# select k=20 for SVD
svdUnigram <- SVD_all(unigram,20)

# 5. Prediction models

# Create datasets to use: append our dependent variable to our dataset 

train  <- cbind(y[sort(training_locations)],svdUnigram[[1]])
test <- cbind(y[-sort(training_locations)],svdUnigram[[2]])

### Apply Random Forest model ######

if (!require("randomForest")) install.packages("randomForest", quiet=TRUE) ; require("randomForest")

### use ntree 1001 (not even number) so it can make decision 1 or 0
RF_model_train <- randomForest(x=train[,2:dim(train)[[2]]],y=train[,1],importance=TRUE,ntree=1001)
RF_predict <- predict(RF_model_train,test[,2:dim(test)[[2]]],type = "prob")[,2]
# This returns the probabilities, which is more useful for the evaluation measures

# Calculate auc
if (!require("ROCR")) install.packages("ROCR", quiet=TRUE) ; require("ROCR")

predML <- prediction(RF_predict,test[,1])

# ROC curve
perfML <- performance(predML,"tpr","fpr")
plot(perfML)
abline(0,1)

## auc
auc.perfML = performance(predML, measure = "auc")
auc.perfML@y.values

if (!require("caret")) install.packages("caret", quiet=TRUE) ; require("caret")
if (!require("e1071")) install.packages("e1071", quiet=TRUE) ; require("e1071")
pred <- as.factor(ifelse(RF_predict>0.5,1,0))
confusionMatrix(pred, test[,1])

