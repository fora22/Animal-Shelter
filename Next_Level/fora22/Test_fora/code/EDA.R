rm(list=ls())
df <- read.csv('./data/EDA_TEST.csv', encoding = 'UTF-8')
head(df)
row_names <- df[,1]
df <- df[,-1]
rownames(df) <- row_names
df

resque <- df[1:3]
animal <- df[4:9]
positive <- df[10:11]
negative <- df[12:13]

# install.packages(corrplot)
# library(corrplot)

# show_cor <- function(datadf) {
#  s_datadf <- scale(datadf)
#  corrplot(cor(s_datadf), method = "square")
# }
# plot.new()
# show_cor(resque)

################################################################################
################################################################################
# FA Start
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


head(resque)
anal_FA_pre(resque)
w_resque <- anal_FA(resque, 1)
s_resque <- scale(resque, center = TRUE)
m_resque <- matrix(s_resque, nrow=25)
resque_MR1 <- m_resque  %*% w_resque[,1]
avg_resque_MR1 <- apply(s_resque[,1:3], 1, sum) / ncol(s_resque[,1:3])

head(animal)
anal_FA_pre(animal)
w_animal <- anal_FA(animal, 1)
s_animal <- scale(animal, center = TRUE)
m_animal <- matrix(s_animal, nrow=25)
animal_MR1 <- m_animal  %*% w_animal[,1]
avg_animal_MR1 <- apply(s_animal[,1:6], 1, sum) / ncol(s_animal[,1:6])

head(positive)
anal_FA_pre(positive)
w_positive <- anal_FA(positive, 1)
s_positive <- scale(positive, center = TRUE)
m_positive <- matrix(s_positive, nrow=25)
positive_MR1 <- m_positive  %*% w_positive[,1]
avg_positive_MR1 <- apply(s_positive[,1:2], 1, sum) / ncol(s_positive[,1:2])

head(negative)
anal_FA_pre(negative)
w_negative <- anal_FA(negative, 1)
s_negative <- scale(negative, center = TRUE)
m_negative <- matrix(s_negative, nrow=25)
negative_MR1 <- m_negative  %*% w_negative[,1]
avg_negative_MR1 <- apply(s_negative[,1:2], 1, sum) / ncol(s_negative[,1:2])

# FA End
################################################################################
################################################################################
# PCA Start
library(factoextra)
anal_pca <- function(df.data) {
    pca_data <- prcomp(df.data, center = T, scale. = T)  
    screeplot(pca_data, main="", col="blue", type="lines", pch=3, npcs = length(pca_data$sdev))
    fviz_pca_ind(pca_data, col.ind="cos2")   
    biplot(pca_data)
    print(summary(pca_data))
    
    return(pca_data$rotation)
}

get_PCA <- function(raw_data) {
    w_pca <- anal_pca(raw_data)
    s_pca <-scale(raw_data, center=TRUE)
    m_pca <- matrix(s_pca, nrow=25)
    PC1 <- m_pca  %*% w_pca[,1]    
    
    return(PC1)
}

resque_PCA <- get_PCA(resque)
animal_PCA <- get_PCA(animal)
positive_PCA <- get_PCA(positive)
negative_PCA <- get_PCA(negative)

exportDF = data.frame(resque_MR1, animal_MR1, positive_MR1, negative_MR1,
                    avg_resque_MR1, avg_animal_MR1, avg_positive_MR1, avg_negative_MR1,
                    resque_PCA, animal_PCA, positive_PCA, negative_PCA)
write.csv(exportDF,file="./data/FA_RESULT.csv")