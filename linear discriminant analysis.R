# logistic regression with single predictor

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


glm.fit = glm(Sold~price,data=logidata,family = binomial) # here binomial represent logistic regression
summary(glm.fit)

glm.fit = glm(Sold~. ,data = logidata,family = binomial)
summary(glm.fit)

glm.probe=predict(glm.fit,type = "response")
glm.probe[1:10]

lda.fit = lda(Sold~.,data=logidata)
summary(logidata)
lda.pred = predict(lda.fit,data=logidata)
lda.pred$posterior

lda.class = lda.pred$class
# creating confusion matrix to compare predicted and actual value
table(lda.class,logidata$Sold)
# if we use 0.8 probability instead of 0.5
sum(lda.pred$posterior[,1]>0.8)
