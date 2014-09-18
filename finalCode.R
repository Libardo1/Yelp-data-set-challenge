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
c = 1:70000
trainingReviews = reviewRestaurantdata_frame[rownames(reviewRestaurantdata_frame)%in%c,  ]
reviewcorpus <- Corpus(VectorSource(trainingReviews$text))
reviewcorpus <- tm_map(reviewcorpus, removeNumbers)
reviewcorpus <- tm_map(reviewcorpus, removePunctuation)
reviewcorpus <- tm_map(reviewcorpus , stripWhitespace)
reviewcorpus <- tm_map(reviewcorpus, tolower)
reviewcorpus <- tm_map(reviewcorpus, removeWords, stopwords("english")) # this stopword file is reviewcorpust C:\Users\[usernreviewcorpusme]\Documents\R\win-librreviewcorpusry\2.13\tm\stopwords 
reviewcorpus <- tm_map(reviewcorpus, stemDocument)
dtm <- DocumentTermMatrix(reviewcorpus)
freqmatrix <- findFreqTerms(dtm, lowfreq=dtm$nrow/7)
library(stringr)
termFreqmatrix = matrix(0,dim(dtm)[1],length(freqmatrix))
for(i in 1:dim(dtm)[1]){for(j in 1:length(freqmatrix)){termFreqmatrix[i,j] = str_count(reviewcorpus[[i]], freqmatrix[[j]])}}
k = rowSums(termFreqmatrix)
for(i in 1:dim(dtm)[1]){for(j in 1:length(freqmatrix)){termFreqmatrix[i,j] = termFreqmatrix[i,j]/k[[i]]}}
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
termFreqmatrix <- transform(termFreqmatrix,  ratings = starsSet)
library(functional)
termFreqmatrix = termFreqmatrix[apply(termFreqmatrix, 1, Compose(is.finite, all)),]
size = dim(termFreqmatrix)[2]
library(e1071)
library(rpart)
index <- 1:nrow(termFreqmatrix)
testindex <- sample(index, trunc(length(index)/5))
testset   <- termFreqmatrix[testindex,]
trainset  <- termFreqmatrix[-testindex,]
svm.model <- svm(starsSet ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred  <- predict(svm.model, testset[,-size])
svm.pred <- as.list(svm.pred)
svm.pred <- as.matrix(svm.pred)
calculatedTestResults = testset[rownames(svm.pred), ]
error = as.numeric(calculatedTestResults[[size]]) - as.numeric(svm.pred)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(error)
trainingSize = length(termFreqmatrix[[1]]) - 11000
model = lm(starsSet ~ ., data = termFreqmatrix[1:trainingSize, ])
predictedRatings = list()
testData = termFreqmatrix[trainingSize+1:11000, ]
coefficients = coef(model)
for(i in 1:11000){predictedRatings = c(predictedRatings,(coefficients[[1]] + testData[[1]][[i]]*coefficients[[2]] + testData[[2]][[i]]*coefficients[[3]] + testData[[3]][[i]]*coefficients[[4]] + testData[[4]][[i]]*coefficients[[5]] + testData[[5]][[i]]*coefficients[[6]] + testData[[6]][[i]]*coefficients[[7]] + testData[[7]][[i]]*coefficients[[8]] + testData[[8]][[i]]*coefficients[[9]] + testData[[9]][[i]]*coefficients[[10]] + testData[[10]][[i]]*coefficients[[11]] + testData[[11]][[i]]*coefficients[[12]] + testData[[12]][[i]]*coefficients[[13]] + testData[[13]][[i]]*coefficients[[14]] + testData[[14]][[i]]*coefficients[[15]] + testData[[15]][[i]]*coefficients[[16]] + testData[[16]][[i]]*coefficients[[17]] + testData[[17]][[i]]*coefficients[[18]]))}
testRatings = list()
testRatings = c(testRatings,testData[[size]])
testRatings <- as.numeric(testRatings)
predictedRatings <- as.numeric(predictedRatings)
error <- testRatings - predictedRatings
rmse(error)
