install.packages("class")
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


trainX =train_set[,-16]
testX= test_set[,-16]
View(train_set)

trainy = train_set$Sold
testy = test_set$Sold

k=3

# stardardise  variables for knn

trainX_s = scale(trainX)
testX_s= scale(testX)

set.seed(0)

#knn classifier
knn.pred = knn(trainX_s,testX_s,trainy,k=9)
table(knn.pred,testy)
