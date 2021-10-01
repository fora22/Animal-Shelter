rm(list=ls())
df <- read.csv('./data/test.csv', encoding = 'UTF-8')
head(df)
row_names <- df[,1]
df <- df[,-1]
rownames(df) <- row_names
df

# install.packages(corrplot)
library(corrplot)

corrplot(cor(df), method = "number")



show_cor <- function(datadf) {
 s_datadf <- scale(datadf)
 corrplot(cor(s_datadf), method = "number")
}
show_cor(df)
proc_df <- df[, c(-2,-5,-7)]
show_cor(proc_df[,c(-2, -6)])

new_df <- proc_df[,c(-2, -6)]

# install.packages("NbClust")
library(NbClust)
s_new_df <- scale(new_df)
nc <- NbClust(s_new_df, min.nc = 2, max.nc = 15, method = "kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")


km <- kmeans(s_new_df, 3)
km$centers


# install.packages("factoextra")
library(factoextra)
fviz_cluster(km, data = s_new_df, stand=F)

km$cluster
write.csv(cbind(new_df,km$cluster), file = "./data/kmeans_RESULT.csv")

##############################################################################################################

# install.packages("flexclust")   #nutrient 데이터셋을 사용하기 위한 패키지

library(flexclust)



str(s_new_df)

head(s_new_df)


d <- dist(s_new_df)

as.matrix(d)[1:4,]         # 거리 계산


fit <- hclust(d, method="average")  # method의 종류보다는 어떤 의도 하에 분류할 것인지에 따라 설명변수를 선택하는 것이 중요

plot(fit, hang=-1 , cex=0.8)

##############################################################################################################
library(fpc)
db <- dbscan(s_new_df, eps = 0.7, MinPts = 2)
fviz_cluster(db, data = s_new_df, frame=FALSE, geom = "point", stand = T)
db$cluster
##############################################################################################################
# Gaussian Mixture, , k-mediods

library("cluster")
sil <- silhouette(km$cluster, dist(s_new_df))
fviz_silhouette(sil)
sil <- silhouette(db$cluster, dist(s_new_df))
fviz_silhouette(sil)

##############################################################################################################
# FA
library(psych)
library(GPArotation)

anal_FA_pre <- function(df.data) {
    scale_df <- scale(df.data, center = TRUE)
    X_cor <- cor(scale_df, use="pairwise.complete.obs")  
    scree(X_cor, factors = FALSE)
    fa.parallel(scale_df, fm = 'minres', fa = 'fa')
}

anal_FA <- function(df.data, nf) {
    scale_df <- scale(df.data, center = TRUE)
    fa.model <- fa(scale_df, nfactors=nf, n.obs=N, rotate="varimax")  
    fa.diagram(fa.model)
    print(fa.model$loadings, cutoff = 0)
    return (fa.model$loadings)
}
anal_FA_pre(new_df)
anal_FA(new_df,2)
##############################################################################################################
##############################################################################################################
# PCA
anal_pca <- function(df.data) {
    pca_data <- prcomp(df.data, center = T, scale. = T)  
    screeplot(pca_data, main="", col="blue", type="lines", pch=3, npcs = length(pca_data$sdev))
    # biplot(pca_data)
    print(pca_data$rotation)
    return(pca_data$rotation)
}
anal_pca(new_df)
##############################################################################################################