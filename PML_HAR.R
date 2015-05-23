require(caret)

training_set <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing_set <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

#remove first five columns
training_set <- training_set[,-c(1:5)]

#calculate mean for each integer and numerics columns
#assigns means to NAs
for (col in names(training_set)){if(class(training_set[[col]]) %in% c("integer", "numeric")){
        avg<-mean(training_set[[col]], na.rm = TRUE)
        training_set[[col]][is.na(training_set[[col]])]<-avg}}

training_set <- training_set[,-nearZeroVar(training_set)]

inTrain <- createDataPartition(y=training_set$classe, p=0.7, list=FALSE)
train <- training_set[inTrain,]
test <- training_set[-inTrain,]

cv <- trainControl(method="cv", number=3)
modelFit <- train(classe~., data=train, method="rf", trControl=cv)

prediction <- predict(modelFit, train, type="raw")
c<-confusionMatrix(prediction, train$classe)
#print("In Sample Error Rate")
#print(1-c[["overall"]][["Accuracy"]])


prediction <- predict(modelFit, test, type="raw")
c<-confusionMatrix(prediction, test$classe)
#print("Out of Sample Error Rate")
#print(1-c[["overall"]][["Accuracy"]])


#generate data for submitting
prediction <- predict(modelFit, testing_set, type="raw")
prediction

for (i in seq(20)){
        fileName<-paste("problem",i,".txt",sep="_")
        write.table(prediction[i],file=fileName,quote=FALSE,row.names=FALSE,col.names=FALSE)
}