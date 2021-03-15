mushroomRD<-read.csv("./mushroom.csv",sep=',',head=T,stringsAsFactors = F)
target<-mushroomRD[c(1)]
mushroomRD<-mushroomRD[-c(1)]

devianceLowest<-10000000

for (i in 1:1000) {
   rd<-sample(mushroomRD, size=10)
   #training<-mushroomRD[rd,c(1:10)]
   trset<-cbind(rd,target)
   model<-glm("poisonous~.",data=trset, family = "binomial")

   if(model$deviance < devianceLowest){
      devianceLowest <- model$deviance
      namesRD<-c(names(rd))
   }


}

devianceLowest
namesRD
