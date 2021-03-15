mushroom_modify<-read.csv("./mushroom_modify.csv",sep=',',head=T,stringsAsFactors = F)

mushroom_modify$poisonous = factor(mushroom_modify$poisonous)

mushroom.rf<-randomForest(poisonous~.,data=mushroom_modify,importance=TRUE)

#importance(mushroom.rf)

varImp(mushroom.rf)

varImpPlot(mushroom.rf)