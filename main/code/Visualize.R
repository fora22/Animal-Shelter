#########################################################################################################
# Data Load
rm(list=ls())
library(readxl)
df_1 <- read_excel('./data/FA_B_MEANSHIFT.xlsx')
df_1.data <- df_1[,3:7]
df_1.cluster <- df_1[,2] 

df_2 <- read_excel('./data/FA_A_KMEANS.xlsx')
df_2.data <- df_2[, 3:6]
df_2.cluster <- df_2[, 2]

df_3 <- read_excel('./data/FA_A_HIR.xlsx')
df_3.data <- df_3[, 3:6]
df_3.cluster <- df_3[, 2]

df_4 <- read_excel('./data/PCA_B_KMEANS.xlsx')
df_4.data <- df_4[, 3:7]
df_4.cluster <- df_4[, 2]

df_5 <- read_excel('./data/FA_B_HIR.xlsx')
df_5.data <- df_5[, 3:7]
df_5.cluster <- df_5[, 2]
#########################################################################################################
#########################################################################################################
# show silhouette
library(factoextra)
show_sil <- function(clus, raw_data) {
  s_df <- scale(raw_data, center = TRUE)
  clust <- as.integer(unlist(c(clus)))
  sil <- silhouette(as.integer(unlist(clust)), dist(s_df))
  fviz_silhouette(sil)
}

show_sil(df_1.cluster, df_1.data) # meanshift, FA_B
show_sil(df_2.cluster, df_2.data) # kmeans, FA_A
show_sil(df_3.cluster, df_3.data) # hiera, FA_A
show_sil(df_4.cluster, df_4.data) # kmeans, PCA_B
show_sil(df_5.cluster, df_5.data) # hiera, FA_B
#########################################################################################################
#########################################################################################################
# show visual clustering
library(cluster)
library(factoextra)
show_viz_kmeans <- function(raw_data, nc, seed_num) {
  # nc는 군집 개수
  set.seed(seed_num)  
  s_df <- scale(raw_data, center = TRUE)
  km <-kmeans(s_df, nc)
  fviz_cluster(km, data = s_df, stand=F, geom = "point" )
}
show_viz_dend <- function(raw_data, nc) {
  # nc는 군집 개수
  s_df <- scale(raw_data, center = TRUE)
  hk <-hkmeans(s_df, nc)
  fviz_dend(hk, cex = 0.6, palette = "jco", 
            rect = TRUE, rect_border = "jco", rect_fill = TRUE)
}

#########################################################################################################
#########################################################################################################
# find Seed Number
# except = FALSE
# i <- 1

# while(!except) {
#   set.seed(i)  
#   test <- show_viz_kmeans(df_4.data, 4, i)
#   t <- TRUE
#   for (j in c(as.integer(unlist(c(df_4.cluster))) == test)) {
#     t <- t & j
#   }
#   except <- t
#   i <- i + 1
# }
# i
#########################################################################################################
#########################################################################################################
# graph
s_df <- scale(df_1.data)

anal_pca <- function(df.data) {
  library(factoextra)
  pca_data <- prcomp(df.data, center = T, scale. = T)  
  screeplot(pca_data, main="", col="blue", type="lines", pch=3, npcs = length(pca_data$sdev))
  fviz_pca_ind(pca_data, col.ind="cos2")   
  # biplot(pca_data)
  print(summary(pca_data))
  return(pca_data$rotation)
}



library(ggplot2)
test <- data.frame(df_PC1, df_PC2, df_1.cluster)
test
test_plot <- ggplot(data = test, aes(x = test$df_PC1, y = test$df_PC2, group = test$FA_B_MEANSHIFT  color = test$FA_B_MEANSHIFT)) + 
  geom_point(shape = 19, size = 4)
test_plot_2 <- test_plot + annotate("rect", xmin=0, xmax=2.6, ymin=0, ymax=0.8, alpha=0.1, fill="red")



test_plot_2
#########################################################################################################
# graph로 적당한 scatter 변수 찾기
panel.fun <- function(x, y, ...) {
  # 출처: https://rfriend.tistory.com/228 [R, Python 분석과 프로그래밍의 친구 (by R Friend)]
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}
plot(df_1.data, pch = 8, bg = c("red", "green3", "blue", "black"), 
     upper.panel = panel.fun,col=1:4, cex = 2)
#########################################################################################################
# meanshift
show_pca <- function(raw_data) {
  w_pca <- anal_pca(raw_data)
  s_pca <-scale(raw_data, center=TRUE)
  m_pca <- matrix(s_pca, nrow=25)
  df_PC1 <- m_pca  %*% w_pca[,1]
  df_PC2 <- m_pca  %*% w_pca[,2]
  # df_PC3 <- m_pca  %*% w_pca[,3]
  
  print(df_PC1)
  print(df_PC2)
  # print(df_PC3)
  
  plot(df_PC1,df_PC2, pch = 8, col = 1:4, cex = 2)
  legend("topright", legend = c("cluster 0", "cluster 1", "cluster 2", "cluster 3"), fill=c("blue", "red" , "green", "black"))
}

# meanshift, FA_B
show_pca(df_1.data)


# kmeans
show_viz_kmeans <- function(raw_data, nc, seed_num) {
  # nc는 군집 개수
  set.seed(seed_num)  
  s_df <- scale(raw_data, center = TRUE)
  km <-kmeans(s_df, nc)
  fviz_cluster(km, data = s_df, stand=F, ellipse = T )
}

# kmeans, FA_A 
show_viz_kmeans(df_2.data, 4, 178)
# show_pca(df_2.data)

# hiera, FA_A 
show_viz_dend(df_3.data, 4)
show_pca(df_3.data)

# kmeans, PCA_B
show_viz_kmeans(df_4.data, 4, 750)
show_pca(df_4.data)

# hiera, FA_B
show_viz_dend(df_5.data, 4)
show_pca(df_5.data)
#########################################################################################################
#########################################################################################################
# dbScan visualize
df = as.data.frame(read_excel("./data/PCA_RESULT.xlsx"))
PCA_A <- df[,1:5]
PCA_C <- df[,13:14]

s_PCA_A <- scale(PCA_A, center = TRUE)
s_PCA_C <- scale(PCA_C, center = TRUE)

library(fpc)    

show_dbscan <- function(datadf, ep, mp) {
  db<-dbscan(datadf,eps=ep,MinPts=mp)
  str(db)
  sum(db$isseed)
  db$cluster
  #  return(db$cluster)
}

show_pca(PCA_A)

db_visual <-  dbscan(s_PCA_C,eps=0.6,MinPts=2)
db_visual

fviz_cluster(db_visual, s_PCA_C, stand = F, frame = F, ellipse = T, geom = "point")


#########################################################################################################