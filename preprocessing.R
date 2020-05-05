df<- read.csv("C:/Users/Lenovo/Documents/R data source/1. Linear Regression/House_price.csv",header = TRUE)
View(df)
str(df)

summary(df)
hist(df$crime_rate)
pairs(~price+crime_rate+n_hot_rooms+rainfall,data = df)
barplot(table(df$waterbody))
barplot(table(df$airport))
barplot(table(df$bus_ter))

#observations
# n_hot_rooms and rainfall have outlier
#bus terminal is useless as it has all the values true
#n_houses_bed have missing values
# crime_rate has some other functional relationship with price
# lets do the preprocessing thing
uv =3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv]<-uv
summary(df$n_hot_rooms)

lv=0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<lv]<-lv
summary(df$rainfall)
mean(df$n_hos_beds,na.rm = TRUE)

df$n_hos_beds[is.na(df$n_hos_beds)] <-mean(df$n_hos_beds,na.rm = TRUE) 
summary(df$n_hos_beds)



plot(df$price,df$crime_rate)
df$crime_rate=log(1+df$crime_rate)
df$avg_dist =(df$dist1+df$dist2+df$dist3+df$dist4)/4
df$avg_dist
df <- df[,-7:-10]
View(df)
df <- df[,-14]
install.packages("dummies")
df <- dummy.data.frame(df)
df <- df[,-9]
df<- df[,-14]
cor(df)
round(cor(df),2)
df <- df[,-16]


#linear model
# y = Beta 0(intercept) + beta1*x
simple_model <- lm(price~room_num,data = df) 
summary(simple_model)
# we saw that Beta 1 value is  9.0997
# and if we increase room num value by 1 unit price will increase by 9.0997

plot(df$room_num,df$price)
abline(simple_model)

multilinear <- lm(price~.,data = df) 
summary(multilinear)

# Multiple R-squared:  0.7208 means 72 % of the variance is explained in this model pretty good model
#better to report ajusted R square

# observations
# air_qual  -15.897400 1% increase in air quality will decrease the house price by 15%
# positive beta value increase house price value by x units
# negative beta value decrease house price value by x units


# train test split

install.packages("caTools")

# WE need to  set the seed value so that we get the same no. of observation each time
set.seed(0)
split = sample.split(df,SplitRatio = 0.8)
training_set = subset(df,split==TRUE)
test_set = subset(df,split==FALSE)
lm_a = lm(price~.,data = training_set)
summary(lm_a)

train_a = predict(lm_a,training_set)
test_a  = predict(lm_a,test_set)
pred
mean((training_set$price-train_a)^2)
mean((test_set$price-test_a)^2)

install.packages("leaps")

# now we use subsets selection methods for splitting the data
lm_best = regsubsets(price~.,data=df,nvmax = 15)
summary(lm_best)
coef(lm_best,8)
# just replace forward with backward in the regsubset function
lm_forward = regsubsets(price~.,data=df,nvmax = 15,method = "forward")
summary(lm_forward)
summary(lm_forward)

# shrinhake penalty reduce the coeff to zero
# lasso has less no of variables 

###############################################################

# train our model using shrinkage method
# ridge regresssion glmnet
install.packages("glmnet")

x = model.matrix(price~.,data = df)[,-1]
x
y = df$price
View(y)
grid=10^seq(10,-2,length=100)
lm_ridge = glmnet(x,y,alpha=0,lambda = grid)
summary(lm_ridge)

cv_fit =cv.glmnet(x,y,alpha=0,lambda = grid)
plot(cv_fit)
opt_lam = cv_fit$lambda.min
tss=sum((y-mean(y))^2)
y_a = predict(lm_ridge,s=opt_lam,newx = x)
rss=sum((y_a-y)^2)
rsq = 1-rss/tss
lm_lasso=glmnet(x,y,alpha=1,lambda = grid)
# cv.glmnet() is cross validation function to get the optimum value of lambda
cv_fit_lasso =cv.glmnet(x,y,alpha=1,lambda = grid)
y_a_lasso = predict(lm_lasso,s=opt_lam,newx = x)
rss_lasso=sum((y_a_lasso-y)^2)
rsq_lasso=1-rss_lasso/tss

# we observe that rsq of lasso is less as compared to rss of ridge
# rsq of lasso =  0.7074099 rsq of ridge = 0.7201051

install.packages("MASS")

# for linear discriminant analysis we use LDA







