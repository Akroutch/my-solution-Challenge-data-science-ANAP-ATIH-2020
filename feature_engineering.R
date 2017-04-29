############################################################################
### load libraries
############################################################################
library(caret)
library(sqldf)
library(doParallel)
detectCores()
registerDoParallel(detectCores() - 1) 
getDoParWorkers()
library(rpart)
library(rattle) #these libraries will be used to get a good visual plot for the decision tree model. 
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(glmnet)
library(party)
library(dplyr)
library(ggplot2)
library(Hmisc)
#####################loading data #######

train = read.table("data2.csv", sep = ";", header = T, fill = T, quote = "", encoding = 'UTF-8')
test  = read.table("test2.csv",  sep = ";", header = T, fill = T, quote = "", encoding = 'UTF-8')

id=test$id
test$id <- NULL  # create ID
test$cible1= -1

####### Delet useless leveles ########
#############################
for ( j in c("rs", "finess", "dep", "act") ) {
  i = which( !( train[[j]] %in% levels( test[[j]] ) ) )
  train[i,j] <- NA
}

train=train[!is.na(train$rs),]
train=train[!is.na(train$finess),]
train=train[!is.na(train$dep),]
train=train[!is.na(train$act),]

for ( j in c("rs", "finess", "dep", "act") ) {
  i = which( !( test[[j]] %in% levels( train[[j]] ) ) )
  test[i,j] <- NA
}


###combine_data
data=rbind(train,test)

data$nb_tot= as.numeric(data$nb_tot)
data$nb_s= as.numeric(data$nb_s)

####### Variable importante !!
data$v3= sqrt(data$nb_s/data$nb_tot)
train$v3= sqrt(train$nb_s/train$nb_tot) ### create v3 in train !!

#rs
d=rpart(cible1~rs,data=train)
data$tr_rs=as.factor(predict(d,data))
#dep
#d=rpart(cible1~dep,data=train)
#data$tr_dep=as.factor(predict(d,data))
#act
d=rpart(cible1~act,data=train)
data$tr_act=as.factor((predict(d,data)))
#finess
d=rpart(cible1~finess,data=train)
data$tr_finess=as.factor((predict(d,data)))

#nb_tot
d=rpart(cible1~nb_tot,data=train)
data$tr_nb_tot=as.factor((predict(d,data)))
#nb_s
d=rpart(cible1~nb_s,data=train)
data$tr_nb_s=as.factor((predict(d,data)))

#v3
d=rpart(cible1~v3,data=train)
data$tr_v3=as.factor((predict(d,data)))

######## select factors features

data$age= as.factor(as.numeric(data$age))
data$rs= as.factor(as.numeric(data$rs))
data$dep= as.factor(as.numeric(data$dep))
data$act= as.factor(as.numeric(data$act))

library(data.table)
data2=as.data.table(data)

data2[,rsCount:=.N,by=rs]
data2[,depCount:=.N,by=dep]
data2[,actCount:=.N,by=act]
data2[,finessCount:=.N,by=finess]

#compute the average
data2[, avgdep:= .(average = mean(v3, na.rm = TRUE)),by = dep]
data2[, avgrs:= .(average = mean(v3, na.rm = TRUE)),by = rs]
data2[, avgact:= .(average = mean(v3, na.rm = TRUE)),by = act]
data2[, avgfiness:= .(average = mean(v3, na.rm = TRUE)),by = finess]

data=as.data.frame(data2)



trainX=data[data$cible1 != -1,]
testX=data[data$cible1 == -1,]


