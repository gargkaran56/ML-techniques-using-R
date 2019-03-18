
setwd("/Users/karangarg/Desktop/data science/Market Basket")

mydata <- read.csv("Cosmetics.csv",header = T, colClasses = "factor") 
mydata<-read.csv(file.choose())

View(mydata)

install.packages("arules")

library(arules)

# use the function apriori 
rules <- apriori(mydata)
rules

## 68880 rules are generated. It has used the default values of confidence , support, minimumlength
## maximum length

summary(rules)

## you will notice that there were 3 rules with just 1 item, 85 rules with 2 items and so on
## it also gives you the summary of the quality measures.For example the minimum support used was 0.1 and 
## it went upto 0.958  

## Reduce to smaller number of rules

rules <- apriori(mydata, parameter = list(minlen=2, maxlen=3, supp=.7))
inspect(rules)

## the rules here are not important because the LHS has "No". We want "Yes" to appear.
## We are more interested in what customers are purchasing.

## Finding interesting rules

rules <- apriori(mydata, parameter = list(minlen=2, maxlen=3, conf=.7),
                 appearance = list(rhs=c("Foundation=Yes"),default="lhs"))

inspect(rules)


#we are using Foundation as the item here as Foundation is the most frequently bought item

summary(mydata)


# Finding interesting rules -2

rules <- apriori(mydata, parameter = list(minlen=2, maxlen=5, conf=.5),
                 appearance = list(rhs=c("Foundation=Yes"),lhs=c("Bag=Yes","Blush=Yes",
                                                                 "Nail.Polish=Yes", "Brushes=Yes", "Concealer=Yes", "Eyebrow.Pencils=Yes",
                                                                 "Bronzer=Yes", "Lip.liner=Yes", "Mascara=Yes", "Eye.shadow=Yes", "Lip.Gloss=Yes",
                                                                 "Lipstick=Yes", "Eyeliner=Yes"), default="none"))

inspect(rules)
quality(rules)<- round(quality(rules),digits = 3)

inspect(rules)

inspect(subset(rules, subset = lift > 1))
inspect(rules)

?apriori
?list













