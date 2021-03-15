source("helper.R")

mushroom_rf<-read.csv("./mushroomSize20.csv",sep=',',head=T,stringsAsFactors = F)

mush10percent<-sample(1:nrow(mushroom_rf),nrow(mushroom_rf)*0.10,replace=F)
m10p<-mushroom_rf[mush10percent,c(1:11)]
predictors<-mushroom_rf[mush10percent,-c(1)]
mush90TR<-mushroom_rf[-mush10percent,c(1:11)]

D10.ActualClassLabel<-m10p$poisonous



formulaStr<-"poisonous~."

accList<-list()
AICList<-list()
devianceList<-list()
sensitivityList<-list()
pricisionList<-list()
specificityList<-list()
recallList<-list()
AUCList<-list()

for (i in 1:100) {
  
  
  tr50<-sample(1:nrow(mush90TR),0.7*nrow(mush90TR),replace = F)
  trainingSet<-mush90TR[tr50,c(1:11)]
  tst<-mush90TR[-tr50,c(1:11)]

  ve<-sample(1:nrow(m10p),nrow(m10p)*0.3,replace=F)
  vetr<-m10p[ve,c(1:11)]
  
  
  model<-glm(formulaStr,data=trainingSet, family = "binomial")
  
  predtr<-predict(model,tst[,2:11])
  predtrclass<-ifelse(predtr<0.5,0,1)
  result<-caret::confusionMatrix(table(tst[[1]],predtrclass))
  source('AUC.R')
  L<-getMetrics(tst[[1]],predtr)
  
  
  acc = result$overall[1]
  accList <- c(accList,acc)
  devianceList <- c(devianceList,model$deviance)
  sensitivityList<-c(sensitivityList,result$byClass[1])
  pricisionList<-c(pricisionList,result$byClass[5])
  specificityList<-c(specificityList,result$byClass[2])
  recallList<-c(recallList,result$byClass[6])
  AUCList<-c(AUCList,L$auc)

}
requiedResults = c(accList, devianceList, sensitivityList, pricisionList, specificityList, recallList, AUCList)
#plot(L$fpr,L$tpr,main=" ROC Plot tpr vs fpr")


accListR<-calList(accList)
devianceListR<-calList(devianceList)
sensitivityListR<-calList(sensitivityList)
pricisionListR<-calList(pricisionList)
specificityListR<-calList(specificityList)
recallListR<-calList(recallList)
AUCListR<-calList(AUCList)


calVar(accListR, devianceListR, sensitivityListR, pricisionListR, specificityListR, recallListR, AUCListR)
calMean(accListR, devianceListR, sensitivityListR, pricisionListR, specificityListR, recallListR, AUCListR)



