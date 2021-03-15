mushroom_rf<-read.csv("./mushroomsRFBtest.csv",sep=',',head=T,stringsAsFactors = F)



mush10percent<-sample(1:nrow(mushroom_rf),nrow(mushroom_rf)*0.70,replace=F)
m10p<-mushroom_rf[mush10percent,c(1:6)]
predictors<-mushroom_rf[mush10percent,-c(1)]
mush90TR<-mushroom_rf[-mush10percent,c(1:6)]

D10.ActualClassLabel<-m10p$poisonous



formulaStr<-"poisonous~."

accList<-list()
AICList<-list()
devianceList<-list()



for (i in 1:100) {
  
  
  tr50<-sample(1:nrow(mush90TR),0.1*nrow(mush90TR),replace = F)
  #trainingSet<-mush90TR[sample(1:nrow(mush90TR),0.5*nrow(mush90TR),replace=F),]
  trainingSet<-mush90TR[tr50,c(1:6)]
  tst<-mush90TR[-tr50,c(2:6)]
  tstR<-mush90TR[-tr50,c(1:6)]
  model<-multinom(formulaStr,data=trainingSet)
  
  #predictions<-predict(model,predictors,probability=TRUE)
  predictions<-predict(model,tst,probability=TRUE)
  #result = caret::confusionMatrix(table(m10p[,1],predictions))
  result = caret::confusionMatrix(table(tstR[,1],predictions))
  acc = result$overall[1]
  accList <- c(accList,acc)
  AICList <- c(AICList,model$AIC)
  devianceList <- c(devianceList,model$deviance)
  
}


as.data.frame(accList)
as.data.frame(AICList)
as.data.frame(devianceList)