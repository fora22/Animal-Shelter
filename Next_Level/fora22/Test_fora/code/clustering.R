rm(list=ls())
df <- read.csv('./seoul_data/DR_result.csv', encoding = 'UTF-8')
head(df)
row_names <- df[,1]
df <- df[,-1]
# rownames(df) <- row_names
df


df_FA <- df[1:4]   # 수정 필요
df_PCA <- df[5:8]   # 수정 필요

################################################################################
################################################################################
# 군집화 개수 결정
data.raw <- df_PCA
d.scale <- scale(data.raw)

## 덴드로그램
set.seed(333)
dist_data <-dist(d.scale, method = "euclidean")
hc_ward <- hclust(dist_data, method="ward.D")

plot(hc_ward)

## 스크리도표
tot_withinss <- c()
for (i in 1:20) {
    set.seed(333)
    kmeans_cluster <- kmeans(d.scale, centers = i, iter.max = 1000)
    tot_withinss[i] <- kmeans_cluster$tot.withinss
}

plot(c(1:20), tot_withinss, type = "b",
     main = "Optimal Number of clusters",
     xlab = "Number of clusters",
     ylab = "Total within-cluster sum 0f squares")

################################################################################
################################################################################
# 군집화 실행
## kmeans

cluster_analysis <- function(data.scale) {
    ## 계층적 군집
    set.seed(333)
    dist_data <-dist(data.scale, method = "euclidean")
    hc_ward <- hclust(dist_data, method="ward.D")
    
    plot(hc_ward)
    rect.hclust(hc_ward, k=3)
    hc <- cutree(hc_ward, k=3)
    ## kmeans
    km_cluster <- kmeans(data.scale, centers = 4, iter.max = 1000)
    km <- km_cluster$cluster
    
    fviz_cluster(km_cluster, data=data.scale, stand = T)
   
    # gaussian mixture
    # install.packages("mclustt")
    # library(mclust)
    
    # bic_gm <- mclustBIC(data.scale)
    # bic_gm
    
    # gm_cluster <- Mclust(data.scale, G=1:3)
    # gm <- gm_cluster$classification
    # gm
    
    return(cbind(km, hc))
}
dbscan_analysis <- function(data.scale, eps, minpts) {
    # dbscan
    library(fpc)
    library(ggplot2)
    library(factoextra)
    # install.packages("dbscan")
    # library(dbscan)
    # dbscan::kNNdistplot(data.scale, k=3)
    
    db_cluster <- fpc::dbscan(data.scale, eps = eps, MinPts = minpts)
    str(db_cluster)
    
    fviz_cluster(db_cluster, data.scale, stand = FALSE, frame = FALSE, geom = "point")
    db <- db_cluster$cluster
    
    return(db)
}


dbscan_analysis(scale(df_FA), 1.3, 2)
dbscan_analysis(scale(df_PCA), 1.4, 2)

km_hc_db <- data.frame(cbind(cluster_analysis(scale(df_FA)), dbscan_analysis(scale(df_FA), 1.3, 2)
                             , cluster_analysis(scale(df_PCA)), dbscan_analysis(scale(df_PCA), 1.2, 3)))
names(km_hc_db) <- c("km_FA","hc_FA","db_FA","km_PCA","hc_PCA", "db_PCA")

head(km_hc_db)
################################################################################
################################################################################
gm_ms <- read.csv('./seoul_data/gm_ms.csv', encoding = 'UTF-8')
head(gm_ms)

result_FA <- data.frame(km_hc_db[,1:3], gm_ms[,2:3])
head(result_FA)
result_PCA <- data.frame(km_hc_db[,4:6], gm_ms[,4:5])
head(result_PCA)

library(cluster)
sil_FA = c()
for (i in 1:5) {
    sil <- silhouette(result_FA[,i], dist(scale(df_FA)))
    sil_FA <- c(sil_FA, mean(sil[, 3]))
}

sil_PCA = c()
for (i in 1:5) {
    sil <- silhouette(result_PCA[,i], dist(scale(df_PCA)))
    sil_PCA <- c(sil_PCA, mean(sil[, 3]))
}

result_sil <- data.frame(sil_FA, sil_PCA)
names(result_sil) <- c("FA", "PCA")
rownames(result_sil) <- c("km", "hc", "db", "gm", "ms")
result_sil

fviz_silhouette(sil)

cluster_result <- data.frame(result_FA, result_PCA)
head(cluster_result)
region = c('강남구', '강동구','강북구','강서구','관악구','광진구',
           '구로구','금천구','노원구','도봉구','동대문구','동작구',
           '마포구','서대문구','서초구','성동구','성북구','송파구',
           '양천구','영등포구','용산구','은평구','종로구','중구','중랑구')
rownames(cluster_result) <- region
write.csv(cluster_result, file="./seoul_data/cluster_result.csv")

################################################################################
################################################################################
# show silhouette
library(factoextra)
show_sil <- function(clus, raw_data) {
    s_df <- scale(raw_data, center = TRUE)
    clust <- as.integer(unlist(c(clus)))
    sil <- silhouette(as.integer(unlist(clust)), dist(s_df))
    fviz_silhouette(sil)
}

show_sil(result_PCA[,4], df_PCA)
show_sil(result_FA[,4], df_FA)

