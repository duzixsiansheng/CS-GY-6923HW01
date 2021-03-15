source('helper.R') 
mushroom_modify<-read.csv("./mushroomSize20.csv",sep=',',head=T,stringsAsFactors = F)


  
  
mush10percent<-sample(1:nrow(mushroom_modify),nrow(mushroom_modify)*0.10,replace=F)
m10p<-mushroom_modify[mush10percent,c(1:11)]
predictors<-mushroom_modify[mush10percent,-c(1)]
mush90TR<-mushroom_modify[-mush10percent,c(1:11)]
  
D10.ActualClassLabel<-m10p$poisonous

accList<-list()
sensitivityList<-list()
pricisionList<-list()
specificityList<-list()
recallList<-list()
AUCList<-list()



for (i in 1:100) {
  knn.df<-mush90TR
  labelcol<- 1
  predictioncol <- labelcol+1
  n<-nrow(knn.df)
  knn.df<-knn.df[sample(n),]
  train.df <- knn.df[1:as.integer(0.7*n),]
  test.df <- knn.df[as.integer(0.7*n + 1): n,]
  
  ve<-sample(1:nrow(m10p),nrow(m10p)*0.3,replace=F)
  vetr<-m10p[ve,c(1:11)]
  library(class)
  library(ROCR)
  
  cl<-factor(train.df[,1])
  knnPred<-knn(train.df[,-c(labelcol)],test.df[,-c(labelcol)], cl, k = 3, prob=FALSE)
  #prob<-attr(knnPred,"prob")
  prob<-c(knnPred)
  kp<-prediction(prob,test.df[,labelcol])
  AUC<-performance(kp,"auc")
  pred_knn<-performance(kp,"tpr","fpr")
  #plot(pred_knn,avg="threshold",colorize=T,lwd=3,main="ROCR knn")
  #text(0.8,0.2,paste("AUC=",round(AUC@y.values[[1]],4),sep=''))
  TBL<-table(test.df[,labelcol],knnPred)
  #print(paste("accuracy=",sum(diag(TBL))/sum(TBL)))
  
  result<-caret::confusionMatrix(table(test.df$poisonous,knnPred))
  
  
  accList<-c(accList, sum(diag(TBL))/sum(TBL))
  AUCList<-c(AUCList, round(AUC@y.values[[1]],4))
  sensitivityList<-c(sensitivityList,result$byClass[1])
  pricisionList<-c(pricisionList,result$byClass[5])
  specificityList<-c(specificityList,result$byClass[2])
  recallList<-c(recallList,result$byClass[6])

}

accListR<-calList(accList)
sensitivityListR<-calList(sensitivityList)
pricisionListR<-calList(pricisionList)
specificityListR<-calList(specificityList)
recallListR<-calList(recallList)
AUCListR<-calList(AUCList)


calVarNB(accListR,  sensitivityListR, pricisionListR, specificityListR, recallListR, AUCListR)
calMeanNB(accListR, sensitivityListR, pricisionListR, specificityListR, recallListR, AUCListR)



