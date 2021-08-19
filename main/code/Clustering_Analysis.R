library(readxl)
rm(list=ls())
df = as.data.frame(read_excel("./data/PCA_RESULT.xlsx"))



PCA_A <- df[,1:5]
PCA_C <- df[,13:14]

s_PCA_A <- scale(PCA_A, center = TRUE)
s_PCA_C <- scale(PCA_C, center = TRUE)


region = c('강남구', '강동구','강북구','강서구','관악구','광진구',
           '구로구','금천구','노원구','도봉구','동대문구','동작구',
           '마포구','서대문구','서초구','성동구','성북구','송파구',
           '양천구','영등포구','용산구','은평구','종로구','중구','중랑구')

# rownames(s_PCA_A) <- region
# rownames(s_PCA_C) <- region

A_kmeans<-kmeans(s_PCA_A,center=4)
km_l_CL <- A_kmeans$cluster
C_kmeans<- kmeans(s_PCA_C,center=4)
km_a_CL <- C_kmeans$cluster

wss <- 0
for(i in 1:15) wss[i]<-sum(kmeans(s_PCA_A,centers = i)$withinss)

plot(1:15, wss, type="b",xlab = "Number of Clusters", ylab = "Within group sum of squares",main='FA_A')

######## dbscan
library(fpc)    

show_dbscan <- function(datadf, ep, mp) {
  db<-dbscan(datadf,eps=ep,MinPts=mp)
  str(db)
  sum(db$isseed)
  db$cluster
#  return(db$cluster)
}

db_l_CL <- show_dbscan(s_PCA_A, 1.61, 2)
db_l_CL
db_a_CL <- show_dbscan(s_PCA_C, 0.6, 2)
db_a_CL
  
final <- data.frame(cbind(km_l_CL, km_a_CL, db_l_CL, db_a_CL), row.names = region)

write.csv(x=final,file='./data/final_CL.csv')