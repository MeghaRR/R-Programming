####*********************Deliverable 2********************************************************



####################################### Imputing Missing Values ####################################

#Import dataset
mydata=read.csv(file.choose())  #FinalDataSet.csv with missing values

#Summary statistics with missing values
summary(mydata)

#import mice package
library(mice)  

#import VIM package
library(VIM)

#checking the missing data in the dataset graphically
mice_plot <- aggr(mydata, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE, labels=names(mydata), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

#Imputing missing data using Mice()
impdata<-mice(mydata)  

#completed dataset after imputation
completeData<-complete(impdata)

#Exporting imputed dataset
write.csv(completeData,"final2.csv")  

############################################### Descriptive analysis ###########################

#import dataset without missing values
final2=read.csv(file.choose())  #final2.csv

#Summary statistics
summary(final2)


#Histograms for each quantitative variable
hist(final2$Age)
hist(final2$eGFR)
hist(final2$Urinary_Alb.Creat_Ratio)
hist(final2$Urinary_Prot.Creat_Ratio)
hist(final2$Serum_Bicarb_Level)

#barcharts for each qualitative variable
barplot(xtabs(~final2$Sex),main = "SEX")
barplot(xtabs(~final2$Race),main = "RACE")
barplot(xtabs(~final2$Diabetes),main = "DIABETES")
barplot(xtabs(~final2$Hypertension),main = "HYPERTENSION")
barplot(xtabs(~final2$ESRD),col=rainbow(2),main = "ESRD")






####*********************Deliverable 3********************************************************


mydata=read.csv(file.choose()) #select final3.csv
summary(mydata)

#scatterplot for the predictive variables
plot(mydata[,c(1:9)])

#correlation using pearson method
res<-cor(mydata[,c(1:9)])
round(res,2)

#pair plots
pairs(mydata[,c(1:9)])

#Set Validation Approach- (188,81)
library (ISLR)
 set.seed (1)
train_idx <- sample(1:nrow(mydata),188,replace=FALSE)
train<- mydata[train_idx,] # select all these rows
test<- mydata[-train_idx,] # select all but these rows
 
#Logistic Regression
 
 #Using all the predictor variables with collinear 
glm1=glm(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+Urinary_Alb.Creat_Ratio+Urinary_Prot.Creat_Ratio+Serum_Bicarb_Level, family=binomial, data=train)
summary(glm1)

#Using only one of the collinear variable :Urinary_Alb.Creat_Ratio 
glm2=glm(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+Urinary_Alb.Creat_Ratio+Serum_Bicarb_Level, family=binomial, data=train)
summary(glm2)

#Using only one of the collinear variable :Urinary_Prot.Creat_Ratio
glm3=glm(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+Urinary_Prot.Creat_Ratio+Serum_Bicarb_Level, family=binomial, data=train)
summary(glm3)

#Using all the predictor variables without Diabetes and 1 collinear variables

glm4=glm(ESRD~Age+Sex+Race+Hypertension+eGFR+Urinary_Alb.Creat_Ratio+Serum_Bicarb_Level, family=binomial, data=train)
summary(glm4)

#predict
glm4pred=predict(glm4 ,test)
summary(glm4pred)





#attach(mydata)
#mean((ESRD-predict(glm1,mydata))[-train]^2)


#LDA
#without collinear variables
library(MASS)
lda.fit=lda(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+Serum_Bicarb_Level,data =train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,test)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, test$ESRD)
mean(lda.class==test$ESRD)

#without collinear variables and diabetes
library(MASS)
lda2=lda(ESRD~Age+Sex+Race+Hypertension+eGFR+Serum_Bicarb_Level,data =train)
lda2
plot(lda2)
lda.pred2=predict(lda2,test)
names(lda.pred2)
lda.class2=lda.pred2$class
table(lda.class2, test$ESRD)
mean(lda.class2==test$ESRD)


#QDA

library(MASS)
#without using collinear
qda1=qda(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+Serum_Bicarb_Level,data = train)
qda1
qda.class1=predict(qda1, test)$class
table(qda.class1, test$ESRD)
mean(qda.class1 == test$ESRD)

#without using collinear and diabetes
qda.fit=qda(ESRD~Age+Sex+Race+Hypertension+eGFR+Serum_Bicarb_Level,data = train)
qda.fit
qda.class=predict(qda.fit, test)$class
table(qda.class, test$ESRD)
mean(qda.class == test$ESRD)


####********************* Deliverable 4 ********************************************************

mydata=read.csv(file.choose()) #select final3.csv
summary(mydata)


#****************** Model Assessment:**********************

#****************** the entire data set as the training data:**********************
attach(mydata)
glm.fit=glm(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+Urinary_Alb.Creat_Ratio+Urinary_Prot.Creat_Ratio+Serum_Bicarb_Level, data=mydata, family=binomial)
summary(glm.fit)
mean((ESRD-predict (glm.fit,mydata))^2)



set.seed(1)
glm.mean1=rep(0,10)
for(i in 1:10)
{
  glm.fit1=glm(ESRD~poly(Age+Sex+Race+Diabetes+Hypertension+eGFR+Urinary_Alb.Creat_Ratio+
                           Urinary_Prot.Creat_Ratio+Serum_Bicarb_Level,i),data=mydata )
  
  glm.mean1[i]=mean((ESRD-predict(glm.fit1,mydata))^2)
  
}
glm.mean1
x<-c(1:10)
plot(x,glm.mean1,type="b",col="red",xlab = "Degree of polynomial",ylab="Model performance")


#******************  the validation set approach:**********************

attach(mydata)
set.seed(1)
train=sample(269,135)
trainSet<-mydata[train,]
validationSet<-mydata[-train,]

glm.mean2=rep(0,10)

for (i in 1:10) 
{
  glm.fit2=glm(trainSet$ESRD~poly(trainSet$Age+trainSet$Sex+trainSet$Race+trainSet$Diabetes+trainSet$Hypertension+
                                    trainSet$eGFR+trainSet$Urinary_Alb.Creat_Ratio+trainSet$Urinary_Prot.Creat_Ratio+trainSet$Serum_Bicarb_Level,i),data=trainSet)
  glm.mean2[i]=mean((trainSet$ESRD-predict(glm.fit2,validationSet))^2)
}
glm.mean2

x<-c(1:10)
plot(x,glm.mean2,type="b",col="blue",xlab = "Degree of polynomial",ylab="Model performance")


#******************   leave-one-out cross validation:**********************


library(boot)
glm.fit=glm(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+Urinary_Alb.Creat_Ratio+Urinary_Prot.Creat_Ratio+Serum_Bicarb_Level, data=mydata, family=binomial)
cv.err=cv.glm(mydata, glm.fit)
cv.err$delta

cv.error=rep (0,10)
for (i in 1:10){
  glm.fit3=glm(ESRD~poly(Age+Sex+Race+Diabetes+Hypertension+eGFR+Urinary_Alb.Creat_Ratio+Urinary_Prot.Creat_Ratio+
                          Serum_Bicarb_Level ,i),data=mydata)
  cv.error[i]=cv.glm(mydata ,glm.fit3)$delta[1]
  
}

cv.error
x<-c(1:10)
plot(x,cv.error,type="b",col="red",xlab = "Degree of polynomial",ylab="Model performance")


#******************    5-fold or 10-fold cross validation**********************


set.seed(17)
cv.error.10= rep(0 ,10)
for (i in 1:10) {
  glm.fit4=glm(ESRD~poly(Age+Sex+Race+Diabetes+Hypertension+eGFR+Urinary_Alb.Creat_Ratio+Urinary_Prot.Creat_Ratio+
                           Serum_Bicarb_Level ,i),data=mydata)
  cv.error.10[i]=cv.glm(mydata ,glm.fit4,K=10)$delta [1]
}
cv.error.10
x<-c(1:10)
plot(x,cv.error.10,type="b",col="black",xlab = "Degree of polynomial",ylab="Model performance")



#******************   Quantifying parameter uncertainty: BOOTSTRAP*********************


library(boot)
boot.fn = function(data, index) return(coef(glm(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+
                                                  Urinary_Alb.Creat_Ratio+Urinary_Prot.Creat_Ratio+Serum_Bicarb_Level, data=mydata, family=binomial, subset = index)))
boot.fn(mydata, 1:269)
set.seed(1)
boot.fn(mydata, sample(269,269,replace = T))
boot.fn(mydata, sample(269,269,replace = T))
boot(mydata, boot.fn, 1000)

summary (glm(ESRD~Age+Sex+Race+Diabetes+Hypertension+eGFR+
               Urinary_Alb.Creat_Ratio+Urinary_Prot.Creat_Ratio+Serum_Bicarb_Level, data=mydata))$coef

