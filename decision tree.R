df = read.csv("C:/Users/Lenovo/Documents/R data source/3. Decision Trees/Movie_regression.csv")
df<- dummy.data.frame(df)
df<-df[,-12]

summary(df)
mean(df$Time_taken,na.rm = TRUE)
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken,na.rm = TRUE)
summary(df)

uv = 3*quantile(df$Twitter_hastags,0.99)
df$Twitter_hastags[df$Twitter_hastags>uv] <- uv
summary(df$Twitter_hastags)

lv =0.3*quantile(df$Time_taken,0.01)
df$Time_taken[df$Time_taken<lv] <-lv
summary(df)


library("caTools")
set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
train = subset(df,split==TRUE)
test = subset(df,split==FALSE)

library("rpart")
install.packages("rpart.plot")
library("rpart.plot")
# Running regression model on training set
regtree <- rpart(formula = Collection~.,data=train,control = rpart.control(maxdepth = 3))
rpart.plot(regtree,box.palette = "RdBu",digits=-3)

# for regression tree we use type = vector for classification we use class
test$pred= predict(regtree,test,type = "vector")
View(test)
# performace of model

MSE2 <-mean((test$pred-test$Collection)^2)
 # if MSE value is low then model is more accurate

############################CLASSIFICATION DECISION TREE##############################
df = read.csv("C:/Users/Lenovo/Documents/R data source/3. Decision Trees/Movie_classification.csv")
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken,na.rm = TRUE)
summary(df)
set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
trainc = subset(df,split==TRUE)
testc = subset(df,split==FALSE)
 ## classification tree model on train set
classtree= rpart(formula = Start_Tech_Oscar~.,data=trainc,method = "class",control = rpart.control(maxdepth = 3))
rpart.plot(classtree,box.palette = "RdBu",digits = -3)

# predict value at any point
testc$pred<-predict(classtree,testc,type = "class")
table(testc$Start_Tech_Oscar,testc$pred)
View(testc)
accuracy=64/112
accuracy


#####bagging regression #######
install.packages("randomForest")
library("randomForest")

df = read.csv("C:/Users/Lenovo/Documents/R data source/3. Decision Trees/Movie_regression.csv")
mean(df$Time_taken,na.rm = TRUE)
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken,na.rm = TRUE)
set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
train = subset(df,split==TRUE)
test = subset(df,split==FALSE)
bagging <- randomForest(formula= Collection~.,data=train,mtry=17) ## how many of the predictor 
# variable we want to consider
# if we reduce mtry it become case of random forest
test$bagging <- predict(bagging,test)
MSE2bagging <- mean((test$bagging-test$Collection)^2)

### MSE of bagging is less than decision tree huge improvement 

randomfor <- randomForest(formula=Collection~.,data=train,ntree=500)
test$random <- predict(randomfor,test)
MSE2random <- mean((test$random-test$Collection)^2)


### Mean square error of random forest for regression is less than bagging and decision tree
# MSE random forest < bagging< decision forest


# Adaboost

install.packages("adabag")
library("adabag")
install.packages("caret")
trainc$Start_Tech_Oscar <- as.factor(trainc$Start_Tech_Oscar)

adaboost <- boosting(Start_Tech_Oscar~., data=trainc, boos=TRUE,mfinal=1000)

predada <- predict(adaboost,testc)
table(predada$class,testc$Start_Tech_Oscar)
