source("helper.R")
library(e1071)

mushroom_modify<-read.csv("./mushroomsSize20.csv",sep=',',head=T,stringsAsFactors = F)


mush10percent<-sample(1:nrow(mushroom_modify),nrow(mushroom_modify)*0.10,replace=F)
m10p<-mushroom_modify[mush10percent,c(1:11)]
predictors<-mushroom_modify[mush10percent,-c(1)]
mush90TR<-mushroom_modify[-mush10percent,c(1:11)]

D10.ActualClassLabel<-m10p$poisonous







TBL<-table(mushroom_modify$poisonous)
CL<-names(TBL)
print(paste("P(c=",CL[1],")=",TBL[[1]]/sum(TBL),sep=""))
print(paste("P(c=",CL[2],")=",TBL[[2]]/sum(TBL),sep=""))

nb_likelihood<-function(df,label,class,feature,val)
{nrow(df[df[[feature]]==val&df[[label]]==class,])/nrow(df[df[[label]]==class,])}


accList<-list()
sensitivityList<-list()
pricisionList<-list()
specificityList<-list()
recallList<-list()
AUCList<-list()



for (i in 1:100) {
  
  tstidx<-sample(1:nrow(mush90TR),0.3*nrow(mush90TR))
  
  trmushroom<-mush90TR[-tstidx,]
  tst<-mush90TR[tstidx,]
  ve<-sample(1:nrow(m10p),nrow(m10p)*1,replace=F)
  vetr<-m10p[ve,c(1:11)]
  
  
  
  nbmodel<-naiveBayes(poisonous~.,data=trmushroom)
  
  #nb.predicted<-predict(nbmodel,trmushroom[,-which(names(trmushroom)=='poisonous')],type='raw')
  nb.predicted<-predict(nbmodel,vetr[,-which(names(tst)=='poisonous')],type='raw')
  
  
  
  
  nb.pred<-unlist(lapply(apply(nb.predicted,1,which.max),
                         FUN=function(x)names(as.data.frame(nb.predicted))[[x]]))
  
  #result<-caret::confusionMatrix(table(trmushroom$poisonous,nb.pred))
  result<-caret::confusionMatrix(table(vetr$poisonous,nb.pred))
  
  roc.trpred<-nb.predicted[,2]
  #L<-getMetricsNB(trmushroom[[1]],roc.trpred)
  L<-getMetricsNB(vetr[[1]],roc.trpred)
  
  acc = result$overall[1]
  accList <- c(accList,acc)
  sensitivityList<-c(sensitivityList,result$byClass[1])
  pricisionList<-c(pricisionList,result$byClass[5])
  specificityList<-c(specificityList,result$byClass[2])
  recallList<-c(recallList,result$byClass[6])
  AUCList<-c(AUCList,L$auc)

}


accListR<-calList(accList)
sensitivityListR<-calList(sensitivityList)
pricisionListR<-calList(pricisionList)
specificityListR<-calList(specificityList)
recallListR<-calList(recallList)
AUCListR<-calList(AUCList)


calVarNB(accListR,  sensitivityListR, pricisionListR, specificityListR, recallListR, AUCListR)
calMeanNB(accListR, sensitivityListR, pricisionListR, specificityListR, recallListR, AUCListR)










