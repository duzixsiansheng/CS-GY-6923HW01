mushrooms<-read.csv("./mushroom.csv",sep=',',head=T,stringsAsFactors = F)

# m10percent<-sample(1:nrow(mushrooms),nrow(mushrooms)*0.10,replace=F)
# 
# m10p<-mushrooms[m10percent,c(1:23)]
# 

pairs(m10p, col = "dodgerblue")

round(cor(mushrooms), 2)
