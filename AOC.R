getMetrics<-function(actual_class,predicted_response)
{
  X=list()
  if ( require(ROCR) ) {
    auc_1=prediction(predicted_response,actual_class)
    prf=performance(auc_1, measure="tpr",x.measure="fpr")
    slot_fp=slot(auc_1,"fp")
    slot_tp=slot(auc_1,"tp")
    
    fpr=unlist(slot_fp)/unlist(slot(auc_1,"n.neg"))
    tpr=unlist(slot_tp)/unlist(slot(auc_1,"n.pos"))
    
    auc<-performance(auc_1,"auc")
    AUC<-auc@y.values[[1]]
    X=list(fpr=fpr,tpr=tpr,auc=AUC)
  }
  X
}

