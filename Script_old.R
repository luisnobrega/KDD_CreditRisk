library(ggplot2)
library(Hmisc)

#Credit problem

#load raw data -----------------
train = read.csv("train_data.csv", header = TRUE)
test = read.csv("test_dataset.csv", header = TRUE)

#subset of training
d = sort(sample(nrow(train), nrow(train)*.6))
#select training sample
subtrain<-train[d,]
subtrain_original<-subtrain
subtest<-train[-d,]
subtest_original<-train[-d,]

#to avoid using always de name of the data set
#attach(train)

#Describes the a R sctucture 
str(train)

#summary of the data data set -- minimuns, maximuns
summary(train)

#Factor -------------------------
#factors of each variable...which values can assume each variable
train$VP67 <-as.factor(train$VP67)
train$VP119 <-as.factor(train$VP119)
train$VP134 <-as.factor(train$VP134)
train$VP135 <-as.factor(train$VP135)
train$VP142 <-as.factor(train$VP142)
train$VP153 <-as.factor(train$VP153)
train$VP157 <-as.factor(train$VP157)
train$VPCMS0008 <-as.factor(train$VPCMS0008)
train$VPCMS0010 <-as.factor(train$VPCMS0010)
train$VPDCS0037 <-as.factor(train$VPDCS0037)
train$VPDCS0050 <-as.factor(train$VPDCS0050)
train$VP169 <-as.factor(train$VP169)
train$VPGEO0040 <-as.factor(train$VPGEO0040)
train$VPDCR0027 <-as.factor(train$VPDCR0027)
train$VP176 <-as.factor(train$VP176)
train$VP184 <-as.factor(train$VP184)
train$VP59_146 <-as.factor(train$VP59_146)
train$VP144_C <-as.factor(train$VP144_C)
train$IND_BOM <-as.factor(train$IND_BOM)



#number BOM(1) and Maus(0)
table(train$IND_BOM)

bom <- length(which(train$IND_BOM==1))
mau <- length(which(train$IND_BOM==0))
total <-bom+mau
per_bom <- bom/total
per_mau <-mau/total

#Histograms --------------------------------------------------------------------------------
ggplot(train, aes(x=IND_BOM, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("IND_BOM") +
  ylab ("Total Count") +
  labs(fill = "IND_BOM")

ggplot(train, aes(x=OPERACAO, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("Operação") +
  ylab ("Total Count") +
  labs(fill = "IND_BOM")

datax <- aggregate(IND_BOM ~ OPERACAO, FUN=mean, data=train)
plot(datax)

ggplot(train, aes(x=DATA_CONSULTA, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("Data") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

temp<- train[which(train$VP67>0 & train$VP67<25),]
table (temp$VP67)
ggplot(train, aes(x=VP67, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VP67") +
  ylab ("Total Count") +
  labs(fill = "IND_BOM")

ggplot(train, aes(x=VP119, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VP119") +
  ylab ("Total Count") +
  labs(fill = "IND_BOM")

ggplot(train, aes(x=VP135, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VP135") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

ggplot(train, aes(x=VP142, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VP142") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")


temp<- train[which(train$VP134>0),]
 ggplot(train, aes(x=VP134, fill = factor(IND_BOM)))+
 stat_count(width=0.5)+
 xlab("VP134") +
 ylab ("Total Count") +
 labs(fill = "Credit BOM")
 
 ggplot(train, aes(x=VP153, fill = factor(IND_BOM)))+
   stat_count(width=0.5)+
   xlab("VP153") +
   ylab ("Total Count") +
   labs(fill = "Credit BOM")
 
 datax <- aggregate(IND_BOM ~ VP153, FUN=mean, data=train)
 plot(datax)

 ggplot(train, aes(x=VP157, fill = factor(IND_BOM)))+
   stat_count(width=0.5)+
   xlab("VP157") +
   ylab ("Total Count") +
   labs(fill = "Credit BOM")
 
 ggplot(train, aes(x=VPCMS0008, fill = factor(IND_BOM)))+
   stat_count(width=1000)+
   xlab("VPCMS0008") +
   ylab ("Total Count") +
   labs(fill = "Credit BOM")
 

 temp<- train[which(train$VPCMS0010<150),]
 
 ggplot(temp, aes(x=VPCMS0010, fill = factor(IND_BOM)))+
   stat_count(width=0.5)+
   xlab("VPCMS0010") +
   ylab ("Total Count") +
   labs(fill = "Credit BOM")
 
 
 ggplot(train, aes(x=VPDCS0037, fill = factor(IND_BOM)))+
   stat_count(width=0.5)+
   xlab("VPDCS0037") +
   ylab ("Total Count") +
   labs(fill = "Credit BOM")
 
 datax <- aggregate(IND_BOM ~ VPDCS0037, FUN=mean, data=train)
 plot(datax)
 
 ggplot(train, aes(x=VPDCS0050, fill = factor(IND_BOM)))+
   stat_count(width=0.5)+
   xlab("VPDCS0050") +
   ylab ("Total Count") +
   labs(fill = "Credit BOM")
 
 datax <- aggregate(IND_BOM ~ VPDCS0050, FUN=mean, data=train)
 plot(datax)

ggplot(train, aes(x=VP169, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("vp169") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

datax <- aggregate(IND_BOM ~ VP169, FUN=mean, data=train)
plot(datax)

ggplot(train, aes(x=VPGEO0040, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VPGEO0040") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

datax <- aggregate(IND_BOM ~ VPGEO0040, FUN=mean, data=train)
plot(datax)


ggplot(train, aes(x=VPDCR0027, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VPDCR0027") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

datax <- aggregate(IND_BOM ~ VPDCR0027, FUN=mean, data=train)
plot(datax)

ggplot(train, aes(x=VP176, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VP176") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

datax <- aggregate(IND_BOM ~ VP176, FUN=mean, data=train)
plot(datax)

ggplot(train, aes(x=VP184, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VP184") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

datax <- aggregate(IND_BOM ~ VP184, FUN=mean, data=train)
plot(datax)

ggplot(train, aes(x=VP59_146, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VP59_146") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

datax <- aggregate(IND_BOM ~ VP59_146, FUN=mean, data=train)
plot(datax)

ggplot(train, aes(x=VP144_C, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("VP144_C") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

datax <- aggregate(IND_BOM ~ VP144_C, FUN=mean, data=train)
plot(datax)



#analysis of data ----------------
#IND_BOM
BOM_TOTAL <- length(which(train$IND_BOM==1))/nrow(train)
MAU_TOTAL <- length(which(train$IND_BOM==0))/nrow(train)

#ver as percentagens de exito conforme a operação
per_operacao1 <- (length(which(train$OPERACAO==1 & train$IND_BOM==1)))/length(which(train$OPERACAO==1))
per_operacao2 <- length(which(train$OPERACAO==2 & train$IND_BOM==1))/length(which(train$OPERACAO==2))
per_operacao3 <- length(which(train$OPERACAO==3 & train$IND_BOM==1))/length(which(train$OPERACAO==3))


#VP67
#dos que não têm débitos por decurso do prazo do prazo nos últimos 3 anos, qual a percentagem que é BOM 
table (train$VP67)
vp67.BOM <- length(which(train$VP67==0 & train$IND_BOM==1))/length(which(train$VP67==0))
vp67.MAU <- length(which(train$VP67==0 & train$IND_BOM==0))/length(which(train$VP67==0))

#VP119
#% de pagamentos efetuados nos últimos 3 anos 
table (train$VP119)
summary (train$VP119)
#vp119.BOM <- length(which(train$VP119==0 & train$IND_BOM==1))/length(which(train$VP119==0))
#vp119.MAU <- length(which(train$VP119==0 & train$IND_BOM==0))/length(which(train$VP119==0))

#VP134
#Menor score dos CNPJs em que o CPF participa 
table (train$VP134)
summary (train$VP134)
vp134.BOM <- length(which(train$VP134==-1 & train$IND_BOM==1))/length(which(train$VP134==-1))

#VP135
#Maior tempo de permanencia como negativo débitos já excluidos (3anos)
table (train$VP135)
summary (train$VP135)
vp135.BOM <- length(which(train$VP135==255 & train$IND_BOM==1))/length(which(train$VP135==255))

#VP142
#% de pagamentos efetuados nos últimos 6 meses 
table (train$VP142)
summary (train$VP142)


#VP59_146
#% de pagamentos efetuados nos últimos 6 meses 
table (train$VP59_146)
summary (train$VP59_146)

#VP153
#total de ramos de atividades diferentes que efetuaram inclusão (1 ano)
table (train$VP153)
summary (train$VP153)
vp153.BOM <- length(which(train$VP153==0 & train$IND_BOM==1))/length(which(train$VP153==0))

#VP157
#total de ramos de atividades diferentes com débitos ativos (nos últimos 5 anos)
table (train$VP157)
summary (train$VP157)
vp157.BOM <- length(which(train$VP157==0 & train$IND_BOM==1))/length(which(train$VP157==0))

#VPCMS0008
#Renda presumida
table (train$VPCMS0008)
summary (train$VPCMS0008)
vpcms0008.BOM <- length(which(train$VPCMS0008==0 & train$IND_BOM==1))/length(which(train$VPCMS0008==0))
rendas<-train[which(train$VPCMS0008>0),]
summary(rendas$VPCMS0008)
rendasAmeio<-rendas[which(rendas$VPCMS0008>mean(rendas$VPCMS0008)),]
rendasAmeioInf<-rendas[which(rendas$VPCMS0008<=mean(rendas$VPCMS0008)),]
rendasAmeio.BOM <- length(which(rendasAmeio$IND_BOM==1))/nrow(rendasAmeio)
rendasAmeioInf.BOM <- length(which(rendasAmeioInf$IND_BOM==1))/nrow(rendasAmeioInf)

table(rendasAmeio$VPCMS0008)
table(rendasAmeioInf$VPCMS0008)

ggplot(rendasAmeio, aes(x=VPCMS0008, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("Renda Presumida") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

ggplot(rendasAmeioInf, aes(x=VPCMS0008, fill = factor(IND_BOM)))+
  stat_count(width=0.5)+
  xlab("Renda Presumida") +
  ylab ("Total Count") +
  labs(fill = "Credit BOM")

#VPCMS0010
#idade
table (train$VPCMS0010)
summary (train$VPCMS0010)
vpcms0010.BOM <- length(which(train$VPCMS0010==255 & train$IND_BOM==1))/length(which(train$VPCMS0010==255))

#VPDCS0037
#percentual de domiciios com energia com o medidor vinculado a um único domícilio
table (train$VPDCS0037)
summary (train$VPDCS0037)

#VPDCS0050
#percentual de domiciios com renda maior que 2 salarios minimos
table (train$VPDCS0050)
summary (train$VPDCS0050)

#VP169
#Existem movimentações de crédito até 12 meses ou possui até 22 anos?
table (train$VP169)
summary (train$VP169)

vp169.BOM <- length(which(train$VP169==0 & train$IND_BOM==1))/length(which(train$VP169==0))
vp169.BOM <- length(which(train$VP169==1 & train$IND_BOM==1))/length(which(train$VP169==1))

vintedoisanos <- train[which(train$VPCMS0010>22 &train$VP169==1) ,]
vintedoisanos.BOM <- length(which(vintedoisanos$IND_BOM==1))/nrow(vintedoisanos)

#VPGEO0040
#media de CPFS com débbitos ativos na região
table (train$VPGEO0040)
summary (train$VPGEO0040)

#VPDCR0027 percentagem de casas com uma casa de banho
table (train$VPDCR0027)
summary (train$VPDCR0027)

#VP176
table (train$VP176)
summary (train$VP176)

#VP184 - consultas efetuadas por mediadoras de seguros
table (train$VP184)
summary (train$VP184)

#VP59_146 - consultas efetuadas por mediadoras de seguros
table (train$VP59_146)
summary (train$VP59_146)
VP59_146.BOM <- length(which(train$VP59_146==1 & train$IND_BOM==1))/length(which(train$VP59_146==1))

#VP144_C - debitos ativos associados a cheques (5 anos)
table (train$VP144_C)
summary (train$VP144_C)
VP144_C.BOM <- length(which(train$VP144_C==10 & train$IND_BOM==1))/length(which(train$VP144_C==10))


#Cleaning Data ------------------------------------------
#Ver client ID repetidos
#Client_ID repetido
dup.CLIENT_ID<-train[which(duplicated(train$CLIENT_ID)),"CLIENT_ID"]
#tabela com client ID repetidos
dup.CLIENT_ID.table<-train[which(train$CLIENT_ID %in% dup.CLIENT_ID),]

#remove linhas duplicadas (olhando para todos os atributos)--------------------
dup <-train[duplicated(train[2:22]),]
clean_train <- train[!duplicated(train[2:22]),]

dup.subtrain <-subtrain[duplicated(subtrain[2:23]),]
subtrain <- subtrain[!duplicated(subtrain[2:23]),]

#atributo ID e data de operçao 
#...on the dataset original
clean_train <- clean_train[,3:ncol(clean_train)]
clean_test <- test[,3:ncol(test)]
#...on the train data splited 
subtrain<-subtrain[,3:ncol(subtrain)]
subtest<-subtest[,3:ncol(subtest)]


#CLIENT_ID não é relevante para criação do modelo

clean_train <- clean_train[,-(which(colnames(clean_train)=="CLIENT_ID"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="CLIENT_ID"))]

subtrain <- subtrain[,-(which(colnames(subtrain)=="CLIENT_ID"))]
subtest <- subtest[,-(which(colnames(subtest)=="CLIENT_ID"))]

#eliminate "percentual de domicilios com 1 banheiro..
#pouca variancia 
#pouco representativo
#tentar ver correlaÃ§Ã£o entre o banheiro e a saÃ­da... tentar excluir 
clean_train <- clean_train[,-(which(colnames(clean_train)=="VPDCR0027"))]
clean_test <- clean_test[,-(which(colnames(clean_test)=="VPDCR0027"))]

clean_train_train <- clean_train_train[,-(which(colnames(clean_train_train)=="VPDCR0027"))]
clean_train_test <- clean_train_test[,-(which(colnames(clean_train_test)=="VPDCR0027"))]

#eliminar VPGEO0040
subtrain <- subtrain[,-(which(colnames(subtrain)=="VPGEO0040"))]
subtest <- subtest[,-(which(colnames(subtest)=="VPGEO0040"))]


#treino-------------
#lda-------------
library (e1071)
library (MASS)  # load the 

lda_mod <- lda (IND_BOM~., data = clean_train)
plot (lda_mod)
lda_pred <-predict (lda_mod, clean_test)
my_submission2 <- test[c("ID", "IND_BOM")]
my_submission2[c("IND_BOM")] <- lda_pred[["class"]]
write.csv(my_submission2, file ="my_submission2.csv", row.names = FALSE)


#glm---------------------------------------
model <- glm(IND_BOM~vp157+VP153, family=binomial(link='logit'), data=subtrain)
#prediction
prediction <- predict(model,subtest,type="response")
my_submission <- subtest_original[c("ID", "IND_BOM")]
my_submission[,2]<-'NA'
my_submission[c("IND_BOM")] <- ifelse(prediction > 0.5,1,0)


#write.csv(my_submission, file ="my_submission.csv", row.names = FALSE)

count <- 0;
for (i in 1:nrow(my_submission)){
  if (my_submission[i,2]==subtest_original[i,23]){
    count=count+1
  }
}

error <- count/nrow(my_submission)
error

#load library
library(ROCR)
#score test data set
subtest$score<-predict(model,type='response',subtest)
pred<-prediction(subtest$score,subtest$IND_BOM)
perf <- performance(pred,"tpr","fpr")
plot(perf)


#Calculating top 3 variables affecting Credit Score Function in R
#get results of terms in regression
g<-predict(model,type='terms',subtest)
#function to pick top 3 reasons
#works by sorting coefficient terms in equation
# and selecting top 3 in sort for each loan scored
ftopk<- function(x,top=5){
  res=names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res,collapse=";",sep="")
}
# Application of the function using the top 3 rows
topk=apply(g,1,ftopk,top=5)
#add reason list to scored tets sample
subtest_topk<-cbind(subtest, my_submission$IND_BOM)
subtest_topk<-cbind(subtest_topk, topk)



#TREE---------------
#load tree package
library(rpart)
fit1<-rpart(IND_BOM~.,data=train)
plot(fit1);text(fit1);

  

#Using Random Forests----------------------
library(randomForest)
arf<- randomForest(IND_BOM~.,data=subtrain,importance=TRUE,proximity=TRUE,ntree=500, keep.forest=TRUE)


#plot variable importance
varImpPlot(arf)
testp4<-predict(arf,test,type='prob')[,2]
pred4<-prediction(testp4,subtest$IND_BOM)
perf4 <- performance(pred4,"tpr","fpr")
#plotting logistic results vs. random forest ROC
plot(perf,col='red',lty=1, main='ROC Logistic Vs. RF');
plot(perf4, col='blue',lty=2,add=TRUE);
legend(0.6,0.6,c('simple','RF'),col=c('red','blue'),lwd=3)


#sem data de operação e ID -> 0.7727
#sem clientID -> 0.7734
#sem atributo do banheiro ->0.7735