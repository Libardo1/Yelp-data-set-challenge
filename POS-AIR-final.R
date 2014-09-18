library("rjson")
json_reviewfile <- "yelp_phoenix_academic_dataset/yelp_academic_dataset_review.json"
json_reviewdata <- fromJSON(paste(readLines(json_reviewfile), collapse=""))
json_businessfile <- "yelp_phoenix_academic_dataset/yelp_academic_dataset_business.json"
json_businessdata <- fromJSON(paste(readLines(json_businessfile), collapse=""))
restaurant_data <- list()
for(data in json_businessdata[[1]]){if("restaurant" %in% data$categories){c(restaurant_data,data)}}
businessdata_matrix = do.call(rbind, json_businessdata[[1]])
businessdata_frame <- data.frame(businessdata_matrix)
elementNos <- grep("Restaurants",json_businessdata[[1]])
businessdata_frame <- businessdata_frame[rownames(businessdata_frame)%in%elementNos,  ]
reviewdata_matrix = do.call(rbind, json_reviewdata[[1]])
reviewdata_frame <- data.frame(reviewdata_matrix)
reviewRestaurantdata_frame <- reviewdata_frame[reviewdata_frame$business_id%in%businessdata_frame$business_id,  ]
json_businessdata = NULL
json_businessfile = NULL
library(tm)
c = 1:150000
trainingReviews = reviewRestaurantdata_frame[rownames(reviewRestaurantdata_frame)%in%c,  ]
reviewcorpus <- Corpus(VectorSource(trainingReviews$text))
reviewcorpus <- tm_map(reviewcorpus, removeNumbers)
reviewcorpus <- tm_map(reviewcorpus, removePunctuation)
reviewcorpus <- tm_map(reviewcorpus , stripWhitespace)
reviewcorpus <- tm_map(reviewcorpus, tolower)
reviewcorpus <- tm_map(reviewcorpus, removeWords, stopwords("english")) # this stopword file is reviewcorpust C:\Users\[usernreviewcorpusme]\Documents\R\win-librreviewcorpusry\2.13\tm\stopwords 
reviewcorpus <- tm_map(reviewcorpus, stemDocument) 
dtm <- DocumentTermMatrix(reviewcorpus)
freqmatrix <- findFreqTerms(dtm, lowfreq=dtm$nrow/10)
library("stringr")
freqterms = str_replace_all(toString(freqmatrix), "[[:punct:]]", " ")
library("NLP")
freqterms <- as.String(freqterms)
library("openNLP")
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(freqterms, list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(freqterms, pos_tag_annotator, a2)
a3w <- subset(a3, type == "word")
tags<- sapply(a3w$features, '[[', "POS")
wordsWithTags = paste(freqterms[a3w],tags)
sprintf("%s/%s", freqterms[a3w], tags)
sample = paste(freqterms[a3w],tags)
adjectives = list()
for(item in sample){if(grepl("JJ",item) || grepl("RB",item)){adjectives = c(adjectives,unlist(strsplit(item, split=' ', fixed=TRUE))[1])}}
termFreqmatrix = matrix(0,dim(dtm)[1],length(adjectives))
for(i in 1:dim(dtm)[1]){for(j in 1:length(adjectives)){termFreqmatrix[i,j] = str_count(reviewcorpus[[i]], adjectives[[j]])}}
k = rowSums(termFreqmatrix)
for(i in 1:dim(dtm)[1]){for(j in 1:length(adjectives)){termFreqmatrix[i,j] = termFreqmatrix[i,j]/k[[i]]}}
starsMatrix = trainingReviews[[4]]
starsMatrix = as.data.frame(starsMatrix)
starsMatrix = t(starsMatrix)
starsSet = starsMatrix[1:dim(starsMatrix)[1]]
starsSet = as.data.frame(starsSet)
remove(businessdata_matrix)
remove(json_reviewdata)
remove(json_reviewfile)
remove(json_businessdata)
remove(json_businessfile)
remove(reviewdata_matrix)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
termFreqmatrix <- transform(termFreqmatrix,  ratings = starsSet)
library(functional)
termFreqmatrix = termFreqmatrix[apply(termFreqmatrix, 1, Compose(is.finite, all)),]

#library(e1071)
#library(rpart)
#index <- 1:nrow(termFreqmatrix)
#testindex <- sample(index, trunc(length(index)/5))
#testset   <- termFreqmatrix[testindex,]
#trainset  <- termFreqmatrix[-testindex,]
#svm.model <- svm(starsSet ~ ., data = trainset, cost = 100, gamma = 1)
#svm.pred  <- predict(svm.model, testset[,-size])
#svm.pred <- as.list(svm.pred)
#svm.pred <- as.matrix(svm.pred)
#calculatedTestResults = testset[rownames(svm.pred), ]
#error = as.numeric(calculatedTestResults[[size]]) - as.numeric(svm.pred)
#rmse(error)
rating <- function(coefficient,testDataRow){
  k = length(testDataRow)
  k = k - 2
  rating = coefficient[[1]]
  for(i in 1:k){
    rating = rating + coefficient[[i+1]]*testDataRow[[i]]
  }
  return(rating)
}
trainingSize = length(termFreqmatrix[[1]]) - 11000
model = lm(starsSet ~ ., data = termFreqmatrix[1:trainingSize, ])
predictedRatings = list()
testData = termFreqmatrix[trainingSize+1:11000, ]
coefficients = coef(model)
for(i in 1:11000){predictedRatings = c(predictedRatings,rating(coefficients,testData[i, ]))}
testRatings = list()
testRatings = c(testRatings,testData[[length(testData[1, ])]])
testRatings <- as.numeric(testRatings)
predictedRatings <- as.numeric(predictedRatings)
error <- testRatings - predictedRatings
rmse(error)
