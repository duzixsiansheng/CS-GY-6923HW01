mushrooms<-read.csv("./mushrooms.csv",sep=',',head=T,stringsAsFactors = F)
a = list('poisonous','capShape','capSurface','capColor','bruises','odor','gillAttachment','gillSpacing','gillSize','gillColor','stalkShape','stalkRoot','SSAR','SSBR','SCAR','SCBR','veilType','veilColor','ringNumber','ringType','sporePrintColor','population','habitat')
for (feature in a){
  print(feature)
}
