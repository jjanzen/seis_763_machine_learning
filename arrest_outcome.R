library(caret)
# import data
# https://data.police.uk/data/
list.files()
setwd("/Users/a149174/UST_GPS/seis_763/ml_test/data/westMidlands/2016-11")
data1 <- read.csv("2016-11-west-midlands-stop-and-search.csv", head=T, sep=',')
setwd("/Users/a149174/UST_GPS/seis_763/ml_test/data/westMidlands/2016-10")
data2 <- read.csv("2016-10-west-midlands-stop-and-search.csv", head=T, sep=',')
setwd("/Users/a149174/UST_GPS/seis_763/ml_test/data/westMidlands/2016-09")
data3 <- read.csv("2016-09-west-midlands-stop-and-search.csv", head=T, sep=',')
setwd("/Users/a149174/UST_GPS/seis_763/ml_test/data/westMidlands/2016-08")
data4 <- read.csv("2016-08-west-midlands-stop-and-search.csv", head=T, sep=',')
setwd("/Users/a149174/UST_GPS/seis_763/ml_test/data/westMidlands/2016-07")
data5 <- read.csv("2016-07-west-midlands-stop-and-search.csv", head=T, sep=',')
setwd("/Users/a149174/UST_GPS/seis_763/ml_test/data/westMidlands/2016-06")
data6 <- read.csv("2016-06-west-midlands-stop-and-search.csv", head=T, sep=',')
data <- rbind(data1,data2,data3,data4)

for (i in 1:nrow(data)){
# convert timestamp to time (two number character)
data$Time[i] <- (strsplit(strsplit(as.character(data['Date'][i,]),"T")[[1]][2],":")[[1]][1])
# round lat/long to 2 decimal points
data$Latitude[i] <- round(data$Latitude[i],2)
data$Longitude[i] <- round(data$Longitude[i],2)
}

# explore data
summary(data)
head(data)

# grab the columns based on exploring
police_data <- data[,c("Type", "Time", "Latitude", 'Longitude', "Age.range", "Self.defined.ethnicity","Legislation", "Object.of.search", "Outcome")]
summary(police_data)
head(police_data)
dim(police_data)

# split 50/50
inTrain <- createDataPartition(y=police_data$Outcome, p=0.5, list=F)
training <- police_data[inTrain,]
testing <- police_data[-inTrain,]
summary(testing)
nrow(testing)

# % of all outcomes which are "Nothing Found".  To determine if adding value
library(sqldf)
testing_outcome <- sqldf(paste("select Outcome, count(Outcome) as Outcome_count from testing group by Outcome"))
x.sub <- subset(testing_outcome, Outcome == 'Nothing found - no further action')
threshold <- x.sub[1,2]/nrow(testing)

# fit a model
set.seed(32343)
# k-fold cross validation
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(Outcome~.,  data=training, method="LogitBoost", trControl=train_control)
#model <- train(Outcome~., data=training, method="svmRadial", trControl=train_control)
# summarize results
print(model) # 70.6% accuracy
# prediction
predictions <- predict(model, newdata=testing)
predictions
# confusion matrix
cm_cv <- confusionMatrix(predictions, testing$Outcome)
cm_cv
cm_cv$byClass
cm_cv$overall

chg_accuracy <- cm_cv$overall[1] - threshold
if(chg_accuracy > 0.0){
    print("improved accuracy by ")
    print(chg_accuracy)
} else{
    print("reduced accuracy by ")
    print(chg_accuracy)
}


# final model (not using cv)
#http://machinelearningmastery.com/compare-models-and-select-the-best-using-the-caret-r-package/
#modelFit <- train(Outcome~., data=training, method="LogitBoost")
modelFit <- train(Outcome~., data=training, method="svmRadial")
#modelFit <- train(Outcome~., data=training, method="kknn")
#modelFit <- train(Outcome~., data=training, method="rf")
modelFit
modelFit$finalModel
# prediction
predictions <- predict(modelFit, newdata=testing)
predictions
# confusion matrix
cm <- confusionMatrix(predictions, testing$Outcome)
cm$byClass
cm$overall

chg_accuracy <- cm$overall[1] - threshold
if(chg_accuracy > 0.0){
    print("improved accuracy by ")
    print(chg_accuracy)
} else{
    print("reduced accuracy by ")
    print(chg_accuracy)
}

# http://stackoverflow.com/questions/31138751/roc-curve-from-training-data-in-caret
install.packages("pROC")
library(pROC)
# Select a parameter setting
selectedIndices <- modelFit$pred$mtry == 2

plot.roc(modelFit$pred$obs[selectedIndices],
         modelFit$pred$M[selectedIndices])

twoClassSummary(predictions, lev = levels(testing$Outcome))
?twoClassSummary


# matrix work http://www.r-tutor.com/r-introduction/matrix/matrix-construction
test_matrix <- matrix( c(1:6),nrow=2, ncol=3)
test_matrix2 <- matrix( c(10:15),nrow=2, ncol=3)
test_matrix * test_matrix2
t(test_matrix)
cbind(test_matrix2, test_matrix)
class(test_matrix2)
v = c(1:6)
v * test_matrix2
c(test_matrix2)
test_matrix2
sum(test_matrix2 * test_matrix)
 #list can have mix of data types
x <- list(1, "a", TRUE, 1 + (0+4i))
x
test_matrix
t(test_matrix)
dim(test_matrix)
