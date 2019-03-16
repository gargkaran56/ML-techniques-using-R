#Set Directory
setwd("/Users/karangarg/Desktop/data\ science/logistic regression")

#Read CSV file
mydata<-read.csv("data.csv",header=T)
View(mydata)
str(mydata)
summary(mydata)

mydata$Churn <- factor(mydata$Churn)

#Outlier treatment
#User defined function for descriptive analysis
stats <- function(x) {
  iqr=IQR(x,na.rm=T)
  q1<-quantile(x,0.25,na.rm=T)
  q2<-quantile(x,0.5,na.rm=T)
  q3<-quantile(x,0.75,na.rm=T)
  UC<-q3+1.5*iqr
  LC<-q1-1.5*iqr
  min<-min(x,na.rm=T)
  max<-max(x,na.rm=T)
  mean<-mean(x,na.rm=T)
  std<-sd(x,na.rm=T)
  return(c(q1=q1, q2=q2, q3=q3,UC=UC, LC=LC, min=min, max=max, mean=mean, std=std))
}

vars <- c("Number.vmail.messages","Total.day.minutes", "Total.day.calls", "Total.day.charge", "Total.eve.minutes", "Total.eve.calls", "Total.eve.charge", "Total.night.minutes", "Total.night.calls",
          "Total.night.charge", "Total.intl.minutes", "Total.intl.calls", "Total.intl.charge", "Customer.service.calls")

data_stats<-t(data.frame(apply(mydata[vars],2,stats)))
View(data_stats)

# missing values
apply(is.na(mydata[,]),2,sum)

#Missing Value Treatment
#require(Hmisc)
#data1<-data.frame(apply(data[vars],2, function(x) impute(x, mean))) #Imputing missing values with mean

## OUTLIERS
mydata$Number.vmail.messages[mydata$Number.vmail.messages>50.000]<-50.000
mydata$Total.day.minutes[mydata$Total.day.minutes>325.450]<-325.450
mydata$Total.day.calls[mydata$Total.day.calls>154.500]<-154.500
mydata$Total.day.charge[mydata$Total.day.charge>55.330]<-55.330
mydata$Total.eve.minutes[mydata$Total.eve.minutes>338.350]<-338.350
mydata$Total.eve.calls[mydata$Total.eve.calls>159.882187312972]<-159.882187312972
mydata$Total.eve.charge[mydata$Total.eve.charge>28.760]<-28.760
mydata$Total.night.minutes[mydata$Total.night.minutes>337.750]<-337.750
mydata$Total.night.calls[mydata$Total.night.calls>152.000]<-152.000
mydata$Total.night.charge[mydata$Total.night.charge>15.195]<-15.195
mydata$Total.intl.minutes[mydata$Total.intl.minutes>17.500]<-17.500
mydata$Total.intl.calls[mydata$Total.intl.calls>10.500]<-10.500
mydata$Total.intl.charge[mydata$Total.intl.charge>4.725]<-4.725
mydata$Customer.service.calls[mydata$Customer.service.calls>3.500]<-3.500

#Dummy Variables Creation
#loading the Caret package to create dummy variables

library(caret)


dmy <- dummyVars(" ~ International.plan+Voice.mail.plan", data = mydata,fullRank = T)
mydata_transformed <- data.frame(predict(dmy, newdata = mydata))

summary(mydata_transformed)

# binding the dummy data with original data

final_data<-cbind(mydata,mydata_transformed)
str(final_data)

# removal of the redundant variables
library(dplyr)
final_data<-select(final_data,-c(International.plan,Voice.mail.plan))
str(final_data)

#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(mydata), size = floor(0.70 * nrow(mydata)))

training<-mydata[train_ind,]
testing<-mydata[-train_ind,]

#Building Models for training dataset

fit<-glm(Churn~.-Id-State
         ,data = training,
         family = binomial(logit))

#Output of Logistic Regression
summary(fit)

#Stepwise regression
step1=step(fit)

#Second Model
fit2<-glm(Churn ~ International.plan + Voice.mail.plan + Total.day.charge + 
            Total.eve.minutes + Total.night.charge + Total.intl.calls + 
            Total.intl.charge + Customer.service.calls
          ,data = training,
         family = binomial(logit))
summary(fit2)
#VIF

library(car)
vif(fit2)


################################VALIDATION ##############################
#predictions
predictions<-predict(fit2, testing,type = "response" )
head(predictions)


#combining the predictions with the original test data

Test_Prediction<-cbind(testing,predictions)

#Converting the probabilities into 

Test_Prediction$response <- as.factor(ifelse(Test_Prediction$predictions>0.1, 1, 0))
str(Test_Prediction)

#Confusion Matrix

library(caret)
Test_Prediction$Churn<- factor(Test_Prediction$Churn, labels=c("0","1"))

conf_matrix<-confusionMatrix(Test_Prediction$response,Test_Prediction$Churn,positive = "1")
conf_matrix

#ROC curve
# plotting the ROC curve

#Loading the ROCR package
library(ROCR)

#Creating the ROCR data
scores <- prediction(predictions=Test_Prediction$predictions, labels=Test_Prediction$Churn)
logit_perf <- performance(scores, "tpr", "fpr")

#plotting the ROC curve
plot(logit_perf,col = "darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA, main="ROC Curve")
box()
abline(0,1, lty = 300, col = "green")
grid(col="aquamarine")

#AUC(Area Under the Curve)
logit_auc <- performance(scores, "auc")
as.numeric(logit_auc@y.values)  ##AUC Value

#KS statistics
logit_ks <- max(logit_perf@y.values[[1]]-logit_perf@x.values[[1]])
logit_ks
