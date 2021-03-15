require(VGAM)
require(nnet)
source("helper.R")


mushroom_rf<-read.csv("./mushroomsRF.csv",sep=',',head=T,stringsAsFactors = F)

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
AUCList<-c()


for (i in 1:100) {
  

  tr50<-sample(1:nrow(mush90TR),0.7*nrow(mush90TR),replace = F)
  #trainingSet<-mush90TR[sample(1:nrow(mush90TR),0.5*nrow(mush90TR),replace=F),]
  trainingSet<-mush90TR[tr50,c(1:11)]
  tst<-mush90TR[-tr50,c(1:11)]
  tstR<-mush90TR[-tr50,c(1:11)]
  
  ve<-sample(1:nrow(m10p),nrow(m10p)*1,replace=F)
  vetr<-m10p[ve,c(1:11)]
  
  
  
  
  model<-multinom(formulaStr,data=trainingSet)
  
  #predictions<-predict(model,predictors,probability=TRUE)
  predictions<-predict(model,vetr[,2:11])
  #result = caret::confusionMatrix(table(m10p[,1],predictions))
  result = caret::confusionMatrix(table(vetr[,1],predictions))
  
  prob<-c(predictions)
  kp<-prediction(prob,vetr[,1])
  AUC<-performance(kp,"auc")
  pred_knn<-performance(kp,"tpr","fpr")
  #plot(pred_knn,avg="threshold",colorize=T,lwd=3,main="ROCR knn")
  #text(0.8,0.2,paste("AUC=",round(AUC@y.values[[1]],4),sep=''))
  
  
  
  
  
  acc = result$overall[1]
  accList <- c(accList,acc)
  AICList <- c(AICList,model$AIC)
  devianceList <- c(devianceList,model$deviance)
  sensitivityList<-c(sensitivityList,result$byClass[1])
  pricisionList<-c(pricisionList,result$byClass[5])
  specificityList<-c(specificityList,result$byClass[2])
  recallList<-c(recallList,result$byClass[6])
  AUCList<-c(AUCList,round(AUC@y.values[[1]]))
}


requiedResults = c(accList, devianceList, sensitivityList, pricisionList, specificityList, recallList, AUCList)



accListR<-calList(accList)
devianceListR<-calList(devianceList)
sensitivityListR<-calList(sensitivityList)
pricisionListR<-calList(pricisionList)
specificityListR<-calList(specificityList)
recallListR<-calList(recallList)
AUCListR<-calList(AUCList)




calVar(accListR, devianceListR, sensitivityListR, pricisionListR, specificityListR, recallListR, AUCListR)
calMean(accListR, devianceListR, sensitivityListR, pricisionListR, specificityListR, recallListR, AUCListR)





