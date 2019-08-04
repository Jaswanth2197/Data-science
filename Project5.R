setwd("D:/edvancer/R-programming/R/Project5")
b_train = read.csv("bank-full_train.csv",stringsAsFactors = F)
b_test = read.csv("bank-full_test.csv",stringsAsFactors = F)
library(dplyr)
glimpse(b_train)
b_test$y= NA

b_train$data = "train"
b_test$data= "test"
b_all = rbind(b_train,b_test)
glimpse(b_all)
sort(table(b_all$y))
b_all$default = as.numeric(b_all$default=="yes")
b_all$housing = as.numeric(b_all$housing=="yes")
b_all$loan=as.numeric(b_all$loan=="yes")
b_all$y = as.numeric(b_all$y=="yes")
b_all$ID=NULL
createDummies = function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t= t[t>freq_cutoff]
  t= sort(t)
  categories = names(t)[-1]
  
  for(cat in categories){
    name = paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("/","_",name)
    
    data[,name]= as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
glimpse(b_all)
b_all = createDummies(b_all,"job",200)
b_all = createDummies(b_all,"marital",0)
b_all = createDummies(b_all,"education",0)
b_all = createDummies(b_all,"month",200)
b_all = createDummies(b_all,"poutcome",0)
library(tidyr)
b_all= b_all %>% 
  mutate(cont1 = as.numeric(contact %in% c("telephone","cellular"))) %>% 
  select(-contact)

table(b_all$housing)

lapply(b_all, function(x) sum(is.na(x)))
# b_all$y = as.factor(b_all$y)

b_train= b_all %>% filter(data=="train") %>% select(-data)
b_test = b_all %>% filter(data=="test") %>% select(-data)

set.seed(2)
s = sample(1:nrow(b_train),0.7*nrow(b_train))
b_train1 = b_train[s,]
b_train2 = b_train[-s,]


library(cvTools)
library(pROC)
mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}
param=list(mtry=c(5,8,10,15),
           ntree=c(100,200,300,400),
           maxnodes=c(20,40,50,60),
           nodesize=c(1,2,5,10))


subset_paras=function(full_list_para,n=2){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}


my_params=subset_paras(param,40)


myauc=0



for(i in 1:30){
  print(paste('starting iteration',i))
  
  params=my_params[i,]
  
  k=cvTuning(randomForest,y~.,
             data =b_train1,
             tuning =params,
             folds = cvFolds(nrow(b_train1), K=10, type = "random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob"))
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    myauc=score.this
    print(myauc)
  }
  
  print('DONE')
}



library(randomForest)
rf.tuned.model=randomForest(y~.,data=b_train,
                            ntree=400,mtry=5,maxnodes=60,nodesize=2,do.trace=T)

##train
train.score=predict(rf.tuned.model,newdata = b_train,type='prob')[,2]
pROC::roc(b_train$y,train.score)

#variable importance plot
importance(rf.tuned.model)
varImpPlot(rf.tuned.model)
##testof train data

train2.score=predict(rf.tuned.model,newdata = b_train2,type='prob')[,2]
pROC::roc(b_train2$y,train2.score)

##TEst data
test.scores = predict(rf.tuned.model,newdata = b_test,type = "prob")[,1]

real=b_train$y
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,TN=0,FN=0,FP=0,TP=0,KS=0)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  
  
  KS=(TP/P)-(FP/N)
 
  
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,TN,FN,FP,TP,KS))
}

cutoff_data=cutoff_data[-1,]

plot(cutoff_data$KS,cutoff_data$cutoff)

library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=KS))+geom_line()
my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$KS)]

##hardclasses

test.predicted = as.numeric(test.scores<my_cutoff)

table(test.predicted)
test.predicted[test.predicted==0]="no"
test.predicted[test.predicted==1]="yes"
write.csv(test.predicted,"proj5.csv",row.names = F)


###Applying Logisticreg
glimpse(b_train)
sum(b_train$pdays== -1)/nrow(b_train)
fit = lm(y~.-job_blue_collar-month_may,data=b_train1)
summary(fit)
library(car)
sort(vif(fit),decreasing = T)[1:3]

log_fit = glm(y~.-job_blue_collar-month_may,data=b_train1,family = 'binomial')
summary(log_fit)
log_fit = step(log_fit)
summary(log_fit)
formula(log_fit)


log_fit_final = glm(y ~ balance + housing + loan + day + duration + campaign + job_student + 
                      job_housemaid + job_retired + job_admin. + job_technician + 
                      job_management + marital_married + education_primary + month_mar + 
                      month_sep + month_oct + month_jan + month_feb + month_apr + 
                      month_nov + month_jun + month_aug + month_jul + poutcome_other + 
                      poutcome_failure + poutcome_unknown + cont1,data=b_train,family = "binomial")

train.prob.score=predict(log_fit_final,newdata = b_train,type='response')
pROC::auc(b_train$y,train.prob.score)

real=b_train$y
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,TN=0,FN=0,FP=0,TP=0,KS=0)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.prob.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  
  
  KS=(TP/P)-(FP/N)
  
  
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,TN,FN,FP,TP,KS))
}

cutoff_data=cutoff_data[-1,]

library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=KS))+geom_line()

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$KS)]

train2.prob.score = predict(log_fit_final,newdata = b_train2,type = "response")
pROC::auc(b_train2$y,train2.prob.score)

test.prob.scores = predict(log_fit_final,newdata = b_test,type = "response")
test.score = as.numeric(test.prob.scores>my_cutoff)
test.score[test.score==0]="no"
test.score[test.score==1]="yes"
write.csv(test.score,"logis5.csv",row.names = F)
