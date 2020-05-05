logidata = read.csv("C:/Users/Lenovo/Documents/R data source/2. Classification/House-Price.csv")
View(logidata)


# preprocessing the data removing na values and quantile values
uv =3*quantile(logidata$n_hot_rooms,0.99)
logidata$n_hot_rooms[logidata$n_hot_rooms>uv]<-uv
summary(logidata$n_hot_rooms)

lv=0.3*quantile(logidata$rainfall,0.01)
logidata$rainfall[logidata$rainfall<lv]<-lv
summary(logidata$rainfall)

logidata$n_hos_beds[is.na(logidata$n_hos_beds)] <-mean(logidata$n_hos_beds,na.rm = TRUE) 
summary(logidata$n_hos_beds)

# creating dummy values of airport , drop and bus terminal
logidata <- dummy.data.frame(logidata)
logidata <- logidata[,-12]
logidata = logidata[,-17]
logidata$avg_dist =(logidata$dist1+logidata$dist2+logidata$dist3+logidata$dist4)/4
logidata$avg_dist
logidata = logidata[,-6:-9]
View(logidata)

set.seed(0)
split = sample.split(logidata,SplitRatio = 0.8)
train_set= subset(logidata,split==TRUE)
test_set=subset(logidata,split==FALSE)

# now we do logistic regression using glm function
train.fit =glm(Sold~.,data=train_set,family = binomial)

# now we find predicted probability of test set using predict function

test.prob = predict(train.fit,test_set,type = 'response')
test.pred=rep('NO',120)
# where the probability more than .5 we have value YES
test.pred[test.prob>0.5]='YES' 

# now creating confusion matrix

table(test.pred,test_set$Sold)

# using logistic regression we get 78/120 correct prediction lower than training set

# now using lda method 

lda.fit =lda(Sold~.,data = train_set)
lda.pred =predict(lda.fit,test_set)
lda.class=lda.pred$class
table(lda.class,test_set$Sold)
# using LDA we get 80/120 accuracy 
#   0  1
# 0 44 16
# 1 24 36

# we found that LDA is better than Logistic regression with probability 80/120