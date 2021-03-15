euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)) ))
  {
    #print(a[[i]],b[[i]])
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}


knn_predict2 <- function(test_data, train_data, k_value, labelcol){
  #print(k_value)
  #print(labelcol)
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,-c(labelcol)], train_data[j,-c(labelcol)]))
      
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(train_data[j,][[labelcol]]))
      #print(i,j,as.character(train_data[j,][[labelcol]])))
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    # print(k_value)
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    
    head(eu[1:k_value,])
    tbl.sm.df<-table(eu$eu_char)
    cl_label<-  names(tbl.sm.df)[[as.integer(which.max(tbl.sm.df))]]
    
    pred <- c(pred, cl_label)
  }
  return(pred) #return pred vector
}


accuracy <- function(test_data,labelcol,predcol){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,labelcol] == test_data[i,predcol]){ 
      correct = correct+1
    }
  }
  accu = (correct/nrow(test_data)) * 100  
  return(accu)
}


calList <- function(listName){
  temp<-c()
  for (i in listName) {
    temp<-c(temp,i)
  }
  return(temp)
}

calMean <- function(list1, list2, list3, list4, list5, list6, list7){
  
  meanResult<-c(mean(list1), mean(list2), mean(list3), mean(list4), mean(list5), mean(list6), mean(list7))
  return(meanResult)
  
  
}

calVar <- function(list1, list2, list3, list4, list5, list6, list7){
  
  varResult<-c(var(list1), var(list2), var(list3), var(list4), var(list5), var(list6), var(list7))
  return(varResult)
}

getMetricsNB<-function(actual_class,predicted_response)
{
  test.set=data.frame(target=actual_class,prediction=predicted_response)
  TPrates = c()
  TNrates = c()
  thresholds = seq(0, 1, by = 0.05)
  total_positive=nrow(test.set[test.set$target=="e" ,])
  total_negative=nrow(test.set[test.set$target=="p",])
  for (threshold in thresholds) {
    TPrateForThisThreshold = nrow(test.set[test.set$target == "e" & test.set$prediction <= threshold,])/total_positive
    TNrateForThisThreshold = nrow(test.set[test.set$target == "p" & test.set$prediction > threshold,])/total_negative
    TPrates = c(TPrates, TPrateForThisThreshold)
    TNrates = c(TNrates, TNrateForThisThreshold)
  }
  if(!require(pracma))install.packages('pracma')
  library(pracma)
  AUC <- trapz(TPrates,TNrates)
  X=list(fpr=1-TNrates,tpr=TPrates,auc=AUC)
  X
  
}


calMeanNB <- function(list1, list2, list3, list4, list5, list6){
  
  meanResult<-c(mean(list1), mean(list2), mean(list3), mean(list4), mean(list5), mean(list6))
  return(meanResult)
  
  
}

calVarNB <- function(list1, list2, list3, list4, list5, list6){
  
  varResult<-c(var(list1), var(list2), var(list3), var(list4), var(list5), var(list6))
  return(varResult)
}

getMetricskNN<-function(actual_class,predicted_response)
{
  test.set=data.frame(target=actual_class,prediction=predicted_response)
  TPrates = c()
  TNrates = c()
  thresholds = seq(0, 1, by = 0.05)
  total_positive=nrow(test.set[test.set$target==0 ,])
  total_negative=nrow(test.set[test.set$target==1,])
  for (threshold in thresholds) {
    TPrateForThisThreshold = nrow(test.set[test.set$target == 0 & test.set$prediction == FALSE,])/total_positive
    TNrateForThisThreshold = nrow(test.set[test.set$target == 1 & test.set$prediction == TRUE,])/total_negative
    TPrates = c(TPrates, TPrateForThisThreshold)
    TNrates = c(TNrates, TNrateForThisThreshold)
  }
  if(!require(pracma))install.packages('pracma')
  library(pracma)
  AUC <- trapz(TPrates,TNrates)
  X=list(fpr=1-TNrates,tpr=TPrates,auc=AUC)
  X
  
}
