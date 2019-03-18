library(ggplot2)
library(ggpubr)
library(e1071)
library(outliers)
options(scipen=999)

rm(list=ls())
getwd()

import.csv <- function(filename) {
  return(read.csv(filename, sep = ";", header = TRUE))
}


ww<- import.csv("winequality-white.csv")



attach(ww) 
str(ww) #all variables are numerical/continuous/or an integer, potentially change quality column to a ordinal
dim(ww) #~4900 rows in dataset, 12 columns
sum(is.na(ww)) #no NA's in the dataset
summary(ww) #physiochemical statistics; mean quality looks to be 6




ggplot(ww, aes(ww$quality)) + geom_bar(stat="count") + ggtitle("Quality") +xlab("quality")
ggplot(ww, aes(x=factor(0),y=quality)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Quality") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=fixed.acidity)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Fixed Acidity") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=volatile.acidity)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Volatile Acidity") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=citric.acid)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Citric Acid") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=residual.sugar)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Residual Sugar") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=chlorides)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Chlorides") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=free.sulfur.dioxide)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Free Sulfur Dioxide") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=total.sulfur.dioxide)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Total Sulfur Dioxide") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=density)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Density") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=pH)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("pH") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=sulphates)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Sulphates") + scale_x_discrete(breaks = NULL) + coord_flip()
ggplot(ww, aes(x=factor(0),y=alcohol)) + geom_boxplot(outlier.size=1) + xlab(" ") +
  ggtitle("Alcohol") + scale_x_discrete(breaks = NULL) + coord_flip()


#Looking at how each variable varies with Quality
ggplot(ww, aes(quality,fixed.acidity, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Fixed Acidity")
ggplot(ww, aes(quality,volatile.acidity, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Volatile Acidity")
ggplot(ww, aes(quality,citric.acid, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Citric Acid")
ggplot(ww, aes(quality,residual.sugar, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Residual Sugar")
ggplot(ww, aes(quality,chlorides, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Chlorides")
ggplot(ww, aes(quality,free.sulfur.dioxide, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Free Sulfur Dioxide")
ggplot(ww, aes(quality,total.sulfur.dioxide, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Total Sulfur Dioxide")
ggplot(ww, aes(quality,density, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Density")
ggplot(ww, aes(quality,pH, group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("pH")
ggplot(ww, aes(quality,sulphates,group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Sulphates")
ggplot(ww, aes(quality,alcohol,group=quality)) + geom_boxplot(outlier.size = 1) +
  xlab("Quality") + ylab("Alcohol")




if(!(require(ggpubr))) install.packages("ggpubr")
ggscatter(ww, x = "residual.sugar", y = "density", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Residual Sugar", ylab = "Density")



shapiro.test(ww$fixed.acidity)
shapiro.test(ww$volatile.acidity)
shapiro.test(ww$citric.acid)
shapiro.test(ww$residual.sugar)
shapiro.test(ww$chlorides)
shapiro.test(ww$free.sulfur.dioxide)
shapiro.test(ww$total.sulfur.dioxide)
shapiro.test(ww$density)
shapiro.test(ww$pH)
shapiro.test(ww$sulphates)
shapiro.test(ww$alcohol)
shapiro.test(ww$quality)



library("Hmisc")
wcorr <- rcorr(as.matrix(ww),type = "spearman")
wcorr





ww$quality <- as.factor(ww$quality) 
is.ordered(quality) 
ww$quality <- as.ordered(ww$quality)

#train test split - doing a 75/25 split
set.seed(100)
r.ww <- dim(ww)[1]
#75% to train
train.rate = 0.75
#remainder to test
test.rate = r.ww*(1.-train.rate)
train.ind <- sample(1:r.ww, train.rate*r.ww, replace=FALSE)
test.ind <- setdiff(1:r.ww,train.ind)
train <- subset(ww[train.ind,], select = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar","chlorides",
                                           "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")) 
#build test dataset
test <- subset(ww[test.ind,], select = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides",
                                         "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol"))
#labels for train dataset
train.lbls <- ww$quality[train.ind]
#labels for test dataset
test.lbls <- ww$quality[test.ind]


#First Model - kNN
print("KNN MODEL\n")
set.seed(1)

if(!(require(gmodels))) install.packages("gmodels")
require(class)
knn_pred <- knn(train = train, test = test, train.lbls,k=sqrt(nrow(test)),use.all=FALSE)
summary(knn_pred)
CrossTable(knn_pred,test.lbls, chisq = FALSE)
#Accuracy
(122+419+30)/nrow(test) #very low accuracy

train.new <- cbind(train, train.lbls) #rebuilding original train dataset with data & labels
names(train.new)[names(train.new) == 'train.lbls'] <- 'quality'

#Second Model -  NaieveBayes 
print("NAIEVE BAYES\n")
set.seed(1)
nb_mod <- naiveBayes(quality~., data=train.new)
nb_pred <- predict(nb_mod, newdata = test)
table(nb_pred, test.lbls)
(3+8+194+189+145+2)/nrow(test)

summary(nb_mod)


#Third Model - SVM Classifier
print("SVM CLASSIFIER\n")
set.seed(1)
svm_mod <- svm(quality~., train.new)
summary(svm_mod)
svm_pred <- predict(svm_mod, test)
table(svm_pred, test.lbls)
(2+210+428+54)/nrow(test)


#Fourth Model - Random Forest (Without replacement)
print("RANDOM FOREST WITHOUT REPLACEMENT\n")
set.seed(1)
if(!(require(randomForest))) install.packages("randomForest")

rf_mod1 <- randomForest(quality~., data=train.new,replace=FALSE)
rf_pred1 <- predict(rf_mod1, newdata = test)
table(rf_pred1, test.lbls)
(11+242+ 431+116+ 14)/nrow(test)



#Random Forest with replacement
print("RANDOM FOREST WITH REPLACEMENT")
set.seed(1)
if(!(require(randomForest))) install.packages("randomForest")
rf_mod2 <- randomForest(quality~., data=train.new)

rf_pred2 <- predict(rf_mod2, newdata = test)
table(rf_pred2, test.lbls)
(11+240+426+116+14)/nrow(test)

#Decrease in node impurity from splitting on the variable
print("IMPORTANCE OF ATTRIBUTES\n")
library(caret)
varImp(rf_mod1)
varImp(rf_mod2)
varImpPlot(rf_mod1)
varImpPlot(rf_mod2)
 



print("With train test ratio of 0.9\n")


#train test split - doing a 90/10 split
set.seed(100)
r.ww <- dim(ww)[1]

train.rate = 0.9
test.rate = r.ww*(1.-train.rate)
train.ind <- sample(1:r.ww, train.rate*r.ww, replace=FALSE)
test.ind <- setdiff(1:r.ww,train.ind)
#build train dataset
train <- subset(ww[train.ind,], select = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar","chlorides",
                                           "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")) 
#build test dataset
test <- subset(ww[test.ind,], select = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides",
                                         "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol"))
#labels for train dataset
train.lbls <- ww$quality[train.ind]
#labels for test dataset
test.lbls <- ww$quality[test.ind]


#First Model - kNN
print("KNN MODEL\n")
set.seed(1)

if(!(require(gmodels))) install.packages("gmodels")
require(class)
knn_pred <- knn(train = train, test = test, train.lbls,k=sqrt(nrow(test)),use.all=FALSE)
summary(knn_pred)
CrossTable(knn_pred,test.lbls, chisq = FALSE)
#Accuracy
(61+145+23)/nrow(test) #very low accuracy

train.new <- cbind(train, train.lbls) #rebuilding original train dataset with data & labels
names(train.new)[names(train.new) == 'train.lbls'] <- 'quality'

#Second Model -  NaieveBayes 
print("NAIEVE BAYES\n")
set.seed(1)
nb_mod <- naiveBayes(quality~., data=train.new)
nb_pred <- predict(nb_mod, newdata = test)
table(nb_pred, test.lbls)
(1+4+88+78+59)/nrow(test)

summary(nb_mod)


#Third Model - SVM Classifier
print("SVM CLASSIFIER\n")
set.seed(1)
svm_mod <- svm(quality~., train.new)
summary(svm_mod)
svm_pred <- predict(svm_mod, test)
table(svm_pred, test.lbls)
(86+177+21)/nrow(test)


#Fourth Model - Random Forest (Without replacement)
print("RANDOM FOREST WITHOUT REPLACEMENT\n")
set.seed(1)
if(!(require(randomForest))) install.packages("randomForest")

rf_mod1 <- randomForest(quality~., data=train.new,replace=FALSE)
rf_pred1 <- predict(rf_mod1, newdata = test)
table(rf_pred1, test.lbls)
(2+102+180+45+4)/nrow(test)



#Random Forest with replacement
print("RANDOM FOREST WITH REPLACEMENT")
set.seed(1)
if(!(require(randomForest))) install.packages("randomForest")
rf_mod2 <- randomForest(quality~., data=train.new)

rf_pred2 <- predict(rf_mod2, newdata = test)
table(rf_pred2, test.lbls)
(3+102+184+48+4)/nrow(test)

#Decrease in node impurity from splitting on the variable
print("IMPORTANCE OF ATTRIBUTES\n")

library(caret)
varImp(rf_mod1)
varImp(rf_mod2)

varImpPlot(rf_mod1)
varImpPlot(rf_mod2)


#Impotance of variable through svm model
print("IMPORTANCE OF ATTRIBUTES THROUGH SVM\n")
library(rminer)
M <- fit(quality~., data=train.new, model="svm", kpar=list(sigma=0.10), C=2)
svm.imp <- Importance(M, data=train.new)
svm.imp$imp


