setwd("/Users/karangarg/Desktop/data\ science/linear regression")
data<-read.csv("boston_data.csv")
str(data)
summary(data)

#Outlier treatment
#Univariate Approach
outlier_values <- boxplot.stats(data$medv)$out  # outlier values.
boxplot(data$medv, main="crim", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Multivariate Approach
mod <- lm(medv ~ ., data=data)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

#influential rows from the original data
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
(data[influential, ])



# user defined function for creating descriptive statistics
stats <- function(x) {
  iqr=IQR(x,na.rm=T)
  q1<-quantile(x,0.25,na.rm=T)
  q2<-quantile(x,0.5,na.rm=T)
  q3<-quantile(x,0.75,na.rm=T)
  UC<-q3+1.5*iqr
  LC<-q1-1.5*iqr
  min<-min(x,na.rm=T)
  max<-max(x,na.rm=T)
  return(c(q1=q1, q2=q2, q3=q3,UC=UC, LC=LC, min=min, max=max))
}

vars <- c( "crim","zn", "indus", "chas", "nox", "rm", "age", "dis", "rad",
           "tax", "ptratio", "black", "lstat", "medv")

data_stats<-t(data.frame(apply(data[vars],2,stats)))
View(data_stats)

# Writing Summary stats to external file

write.csv(diag_stats, file = "diag_stats.csv")

## OUTLIER Treatment
data$crim[data$crim>10.00932]<-10.00932
data$zn[data$zn>31.25000]<-31.25000
data$rm[data$rm>7.71900]<-7.71900
data$dis[data$dis>9.92350]<-9.92350
data$lstat[data$lstat>31.57250]<-31.57250
data$medv[data$medv>36.85000]<-36.85000

## Missing Value Treatment
#require(Hmisc)
#data1<-data.frame(apply(data[vars],2, function(x) impute(x, mean))) #Imputing missing values with mean


#Assumption
hist(data$medv)
#hist(log(data$medv))

#Density Plot
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(data$medv), main="Density Plot: Speed", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(data$medv), 2)))  # density plot for 'speed'
    polygon(density(data$medv), col="red")

#Correlation
corrm<- cor(data)  
write.csv(corrm, file = "Correlation Matrix.csv")
#Correlation visualisation
options(repos = c(CRAN = "http://cran.rstudio.com"))
install.packages("corrplot")
library(corrplot)
corrplot(corrm)


#Splitting data into Training, Validaton and Testing Dataset
set.seed(123)
train_ind <- sample(1:nrow(data), size = floor(0.70 * nrow(data)))

training<-data[train_ind,]
testing<-data[-train_ind,]

# Multiple Linear Regression 
fit <- lm(medv~., data=training)
summary(fit)

require(MASS)
step<- stepAIC(fit,direction="both")

fit2<-lm(medv ~  crim + zn + nox + rm + dis + rad + tax + ptratio + black + 
           lstat, data = training)
summary(fit2)

fit3<-lm(medv ~  nox + dis + rad + ptratio + black 
           , data = training)
summary(fit3)

fit4<-lm(medv ~  nox + dis + ptratio + black 
         , data = training)
summary(fit4)

#Multicollinierity Check using VIF
library(car)
vif(fit4)

#######################Validation by Prediction
options(scipen=999)
t1<-cbind(testing, pred_medv=(predict(fit4,testing)))
t1<- transform(t1, APE = abs(pred_medv - medv)/medv)

#MAPE(Error Rate)
mean(t1$APE)

#Calculate prediction accuracy 
pred_medv<- (predict(fit4,testing))
actuals_preds <- data.frame(cbind(actuals=testing$medv, predicteds=pred_medv))  
correlation_accuracy <- cor(actuals_preds)

# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2))
plot(fit4)

#Treating heteroskadisticity

library(caret)
library(e1071)

# Box-Cox transformation to correct for Heteroscedasticity 
shares_new<-BoxCoxTrans(data$medv)
data1 <- cbind(data, medv_new=predict(shares_new, data$medv)) # append the transformed variable


################################### END OF Linear REGRESSION Example  ##################################### 
#Transformation

#Splitting data into Training, Validaton and Testing Dataset
set.seed(123)
train_ind <- sample(1:nrow(data1), size = floor(0.70 * nrow(data1)))

training<-data1[train_ind,]
testing<-data1[-train_ind,]

# Multiple Linear Regression 
fit <- lm(medv~., data=training)
summary(fit)

require(MASS)
step<- stepAIC(fit,direction="both")

fit2<-lm(medv ~  crim + zn + nox + rm + dis + rad + tax + ptratio + black + 
           lstat, data = training)
summary(fit2)

fit3<-lm(medv ~  nox + dis + rad + ptratio + black 
         , data = training)
summary(fit3)

fit4<-lm(medv_new ~  nox + dis + ptratio + black 
         , data = training)
summary(fit4)

#Multicollinierity Check using VIF
library(car)
vif(fit4)

#######################Validation by Prediction
options(scipen=999)
t1<-cbind(testing, pred_medv=(predict(fit4,testing)))
t1<- transform(t1, APE = abs(pred_medv - medv_new)/medv_new)

#MAPE(Error Rate)
mean(t1$APE)

#Calculate prediction accuracy 
pred_medv<- (predict(fit4,testing))
actuals_preds <- data.frame(cbind(actuals=testing$medv, predicteds=pred_medv))  
correlation_accuracy <- cor(actuals_preds)

# Diagnostic plots 
layout(matrix(c(1,2,3,4),2,2))
plot(fit4)



