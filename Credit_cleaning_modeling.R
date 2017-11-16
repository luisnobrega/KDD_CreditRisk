library(party)
library(e1071)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(C50)
library(stats)
library(MASS)
library(caret)
library(RSNNS)
library(naivebayes)
library(ROCR)


#load raw data -----------------
train_data = read.csv("train_data.csv", header = TRUE)
test_data = read.csv("test_dataset.csv", header = TRUE)

#DATA TREINO DIVIDIDO 
#subset of training ---------------------
d <- sort(sample(nrow(train_data), nrow(train_data)*.7))
#select training sample
subtrain<-train_data[d,]
subtest<-train_data[-d,]


#preparaçao data-------
subdata<-treat_data(subtrain, subtest)
train <- subdata$tr
test <-subdata$te


#glm---------------------------------------
model <- glm(IND_BOM ~., family=binomial(link='logit'), data=train)
#prediction
prediction_model <- predict(model,test,type="response")
my_submission <- subtest[c("ID", "IND_BOM")]
my_submission[,2]<-'NA'
my_submission[c("IND_BOM")] <- ifelse(prediction_model > 0.5,1,0)

errors = error(my_submission,subtest)
print(errors)

pred_glm<- prediction(prediction_model, subtest[,23])
perf_glm<-performance(pred_glm, "tpr", "fpr")
#png(filename = "roc_compartive.png")
#plot(perf_glm, main="ic", col="blue", lwd=3)
perf.auc <- performance(pred_glm, measure = "auc")
unlist(perf.auc@y.values)


#false_data<-my_submission[2]-subtest[23]
#FN <- sum(false_data == -1)
#FP <- sum(false_data == 1)

#true_data<-my_submission[2]+subtest[23]
#TP<-sum(true_data==2)
#TN<-sum(true_data==0)

#TP+TN+FN+FP

#datax <- aggregate(IND_BOM ~  VPDCS0037, FUN=mean, data=train)
#plot(datax)


#LDA-------------
#model<-lda(IND_BOM ~ ., data = train)
#prediction
#prediction <- predict(model, test)
#my_submission <- subtest[c("ID", "IND_BOM")]
#my_submission[,2]<-'NA'
#my_submission[c("IND_BOM")] <- as.numeric(levels(prediction$class))[prediction$class]

#errors = error(my_submission, subtest)
#errors



#PLSDA-------------
#model<-plsda(train[,1:12], as.factor(train[,13]), probMethod = "Bayes")
#prediction
#prediction <- predict(model, test)
#my_submission <- subtest[c("ID", "IND_BOM")]
#my_submission[,2]<-'NA'
#my_submission[c("IND_BOM")] <- as.numeric(levels(prediction))[prediction]

#errors = error(my_submission, subtest)
#errors



#SVM-------------
#model<-svm(as.factor(IND_BOM) ~ ., data = train_small)
#prediction
#prediction <- predict(object=model, newdata=test)
#my_submission <- test_data[c("ID", "IND_BOM")]
#my_submission[,2]<-'NA'
#my_submission[c("IND_BOM")] <- prediction

#errors = error(my_submission,subtest)
#errors

#naivebayes -------
#model<- naive_bayes(as.factor(IND_BOM) ~., data = train)
#prediction <- predict(model,test,type="prob")
#my_submission <- subtest[c("ID", "IND_BOM")]
#my_submission[,2]<-'NA'
#my_submission[c("IND_BOM")] <- prediction


#errors = error(my_submission,subtest)
#errors

#randomForest---------------
model <- randomForest(as.factor(IND_BOM) ~., data = train_small, importance = TRUE, proximity=TRUE, ntree=500, keep.forest=TRUE)
varImpPlot(model)
print(model)

prediction <- predict(model,test)
my_submission <- subtest[c("ID", "IND_BOM")]
my_submission[,2]<-'NA'
my_submission[c("IND_BOM")] <- prediction
errors = error(my_submission,subtest)
errors


#decision trees -------

  model = rpart(IND_BOM ~., data = train, control = rpart.control(cp=0.00008369587, xval=30, maxdepth=30))
  #png(filename = "trees.png", height = 1000, width = 4000, res = 500)
  #fancyRpartPlot(model)
  #line(perf_glm, col="green", lwd=3)
  #dev.off()
  #plot(model, margin=0.05, compress=TRUE, main="Decision Tree")
  prediction_model <- predict(model, test) 
  my_submission <- subtest[c("ID", "IND_BOM")]
  my_submission[,2]<-'NA'
  my_submission[c("IND_BOM")] <- ifelse(prediction_model > 0.5,1,0)
  
  errors = error(my_submission,subtest)
  print(errors)
  
  pred_rpart<- prediction(prediction_model, subtest[,23])
  perf_rpart<-performance(pred_rpart, "tpr", "fpr")
  png(filename = "roc.png")
  plot(perf_glm, main="ic", colorize = TRUE,lwd=3)
  plot(perf_rpart, add="TRUE",colorize = TRUE, lwd=3)
  dev.off()
  perf.auc <- performance(pred_rpart, measure = "auc")
  unlist(perf.auc@y.values)

summary(model)
plotcp(model)

write.csv(my_submission, file ="my_submission.csv", row.names = FALSE)
write.csv(subtest, file ="subtest.csv", row.names = FALSE)
write.csv(my_submission, file ="glm.csv", row.names = FALSE)


###########function -------
error <- function(submission, original){
  count <- 0;
  for (i in 1:nrow(submission)){
    if (submission[i,2]==original[i,23]){
      count=count+1
    }
  }
  corrects = count/nrow(submission)
  return(corrects)
}


treat_data <- function(train, test){
  
clean_train<-train  
#cleaning

#remove linhas duplicadas (olhando para todos os atributos
dup <-clean_train[duplicated(clean_train[3:23]),]
clean_train <- train[!duplicated(clean_train[3:23]),]

#elimina uma linha de cada par de insconsistentes
inconsistent <-clean_train[(duplicated(clean_train[3:22])),]
clean_train <- clean_train[!duplicated(clean_train[3:22]),]

#elimina a segunda linha de cada para dos incostistentes (2x102)
inconsistent_index <- inconsistent[,"CLIENT_ID"]
clean_train<-clean_train[!(clean_train$CLIENT_ID %in% inconsistent_index),]

#remover atributos - TRAIN
clean_train <- clean_train[,-(which(colnames(clean_train)=="ID"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="DATA_CONSULTA"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="CLIENT_ID"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="VP67"))]
#clean_train <- clean_train[,-(which(colnames(clean_train)=="VP135"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="VP157"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="VPCMS0008"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="VPDCR0027"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="VP169"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="VP176"))]
#clean_train <- clean_train[,-(which(colnames(clean_train)=="VP184"))]
clean_train <- clean_train[,-(which(colnames(clean_train)=="VP144_C"))]

#clean_train <- clean_train[,-(which(colnames(clean_train)=="VPGEO0040"))]

clean_test <- test[,-(which(colnames(test)=="ID"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="DATA_CONSULTA"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="CLIENT_ID"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="VP67"))]
#clean_test <- clean_test[,-(which(colnames(clean_test)=="VP135"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="VP157"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="VPCMS0008"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="VPDCR0027"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="VP169"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="VP176"))]
#clean_test <- clean_test[,-(which(colnames(clean_test)=="VP184"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="VP144_C"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="IND_BOM"))]

#clean_test <- clean_test[,-(which(colnames(clean_test)=="VPGEO0040"))]

#remover outliers
temp <- clean_train[-(which(clean_train$VP119==51|clean_train$VP119==3)),]
#temp <- temp[-(which(temp$VP135==62 | temp$VP135==-1)),]
#temp <- temp[-(which(temp$VP153>10)),]
#temp <- temp[-(which(temp$VP157>9)),]
temp <- temp[-(which(temp$VPCMS0010<21 | temp$VPCMS0010>80)),]
temp <- temp[-(which(temp$VPDCS0037<55 & temp$VPDCS0037>=0)),]
temp <- temp[-(which(temp$VPDCS0050>95)),]
temp <- temp[-(which(temp$VPGEO0040<100 | temp$VPGEO0040>440)),]
#temp <- temp[-(which(temp$VP184>11|temp$VP184==8 | temp$VP184==9)),]
#temp <- temp[-(which(temp$VP176==-1|temp$VP176>85)),]

#ouliers VP134
a <- as.data.frame(table(temp$VP134))
b <- a[which(a$Freq<=5),]
temp <- temp[!(temp$VP134 %in% b$Var1), ]
temp <- temp[-(which(temp$VP134==1| temp$VP134==2)),]

#ouliers VP142
a <- as.data.frame(table(temp$VP142))
b <- a[which(a$Freq<=5),]
temp <- temp[!(temp$VP142 %in% b$Var1), ]
#temp <- temp[-(which(temp$VP142==10|temp$VP142==11|temp$VP142==13|temp$VP142==36|temp$VP142==43|temp$VP142==44|temp$VP142==94|temp$VP142==91)),]

#discretizar vp142
#temp<-discret(temp, grep("VP142", colnames(temp)), 1, 49, 50, 99)
#clean_test<-discret(clean_test, grep("VP142", colnames(clean_test)), 1, 49, 50, 99)

#trocar x = -3 com x = -1 do VP59_146
#temp<-swap (temp, grep("VP59_146", colnames(temp)), -3,-1)
#clean_test<-swap (clean_test, grep("VP59_146", colnames(clean_test)), -3,-1)


#trocar x = 255 com x = -1 do VP135
#temp<-swap (temp, grep("VP135", colnames(temp)), 255,-1)
#clean_test<-swap (clean_test, grep("VP135", colnames(clean_test)), 255,-1)


#trocar x = 0 com x = -1 do VP184
#temp<-swap (temp, grep("VP184", colnames(temp)), 0,-1)
#clean_test<-swap (clean_test, grep("VP184", colnames(clean_test)), 0,-1)


#discretizar vp119
#temp<-discret(temp, grep("VP119", colnames(temp)), 1, 49, 50, 99)
#clean_test<-discret(clean_test, grep("VP119", colnames(clean_test)), 1, 49, 50, 99)

#trocar x = -3 com x=-1 do VP119
#temp<-swap (temp, grep("VP119", colnames(temp)), -3,-1)
#clean_test<-swap (clean_test, grep("VP119", colnames(clean_test)), -3,-1)

#trocar x = -2 com x=-1 do VP119
#temp<-swap (temp, grep("VP119", colnames(temp)), -2,-1)
#clean_test<-swap (clean_test, grep("VP119", colnames(clean_test)), -2,-1)


list(tr=clean_train, te=clean_test)
}

discret <-function (data, attribute, x11, x12, x21, x22){
  for (i in 1:nrow(data))
  {
    if(data[i,attribute] >= x11 & data[i,attribute] <=x12){
      data[i,attribute]= 1
    }
    else if(data[i,attribute] >=x21 & data[i,attribute] <=x22){
      data[i,attribute]=2
    }
    else if((data[i,attribute] ==100) ){
      data[i,attribute] =3
    }
  }
  return (data)
}
swap <- function (data,attribute, x1, x2){
  i <- data[,attribute] == x1
  j <- data[,attribute] == x2
  data[i,attribute] <- x2
  data[j,attribute] <- x1
  return (data)
}


