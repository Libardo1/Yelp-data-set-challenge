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
rowForSentimentMatrix <- function(reviews){ 
  completeList = data.frame()
  for(review in reviews){
    rowList = list() 
    differencentStrings = strsplit(review,".",fixed = TRUE)
    count1 = 0
    count2 = 0
    count3 = 0
    count4 = 0
    count5 = 0
    count6 = 0
    count7 = 0 
    library('qdap')
    sentiMentList = list()  
    for(line in differencentStrings[[1]]){
      if(line != ""){
      sentiMentList = c(sentiMentList,polarity(line)[[2]][[4]])}}
    for(senti in sentiMentList){
      if(!is.finite(senti)) senti = 0
      if(senti >= -1 && senti < -0.50){count1 = count1 + 1} 
      else if(senti >= -0.50 && senti < -0.25){count2 = count2 + 1}
      else if(senti >= -0.25 && senti < 0){count3 = count3 + 1}
      else if(senti == 0){count4 = count4 + 1}
      else if(senti > 0 && senti <= 0.25){count5 = count5 + 1}
      else if(senti > 0.25 && senti <= 0.5){count6 = count6 + 1}
      else{count7 = count7 + 1;} 
    }
    rowList = c(rowList,count1)
    rowList = c(rowList,count2)
      rowList = c(rowList,count3)
      rowList = c(rowList,count4)
      rowList = c(rowList,count5)
      rowList = c(rowList,count6)
      rowList = c(rowList,count7)
      completeList = rbind(completeList,rowList)
    }
    return(completeList)
  }
  library(parallel)
  numWorkers <- 5
  res <- mclapply(reviewcorpus[1:10000], rowForSentimentMatrix, mc.cores = numWorkers)
  sentimentReviewDF = data.frame()
  for(i in 1:length(res)){for(j in 1:length(res[[1]])){sentimentReviewDF[i,j] = res[[i]][[j]]}}
  k = rowSums(sentimentReviewDF)
  for(i in 1:10000){for(j in 1:length(res[[1]])){sentimentReviewDF[i,j] = sentimentReviewDF[i,j]/k[[i]]}}
  starsMatrix = trainingReviews[[4]]
  starsMatrix = as.data.frame(starsMatrix)
  starsMatrix = t(starsMatrix)
  starsSet = starsMatrix[1:dim(starsMatrix)[1]]
  starsSet = as.data.frame(starsSet)
  starsSet = starsSet[1:10000, ]
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
  sentimentReviewDF <- transform(sentimentReviewDF,  ratings = starsSet)
  library(functional)
  size = dim(sentimentReviewDF)[2]
  library(e1071)
  library(rpart)
  index <- 1:nrow(sentimentReviewDF)
  testindex <- sample(index, trunc(length(index)/6))
  testset   <- sentimentReviewDF[testindex,]
  trainset  <- sentimentReviewDF[-testindex,]
  svm.model <- svm(ratings ~ ., data = trainset, cost = 100, gamma = 1)
  svm.pred  <- predict(svm.model, testset[,-size])
  svm.pred <- as.list(svm.pred)
  svm.pred <- as.matrix(svm.pred)
  calculatedTestResults = testset[rownames(svm.pred), ]
  error = as.numeric(calculatedTestResults[[size]]) - as.numeric(svm.pred)
  rmse(error)
model = lm(ratings ~ ., data = trainset)
predictedRatings = list()
coefficients = coef(model)
for(i in 1:1666){predictedRatings = c(predictedRatings,(coefficients[[1]] + testset[[1]][[i]]*coefficients[[2]] + testset[[2]][[i]]*coefficients[[3]] + testset[[3]][[i]]*coefficients[[4]] + testset[[4]][[i]]*coefficients[[5]] + testset[[5]][[i]]*coefficients[[6]] + + testset[[5]][[i]]*coefficients[[7]]))}
testRatings = list()
testRatings = c(testRatings,testset[[size]])
testRatings <- as.numeric(testRatings)
predictedRatings <- as.numeric(predictedRatings)
error <- testRatings - predictedRatings
rmse(error)
