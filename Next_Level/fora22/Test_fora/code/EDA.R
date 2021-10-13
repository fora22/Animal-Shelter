rm(list=ls())
df <- read.csv('./data/EDA_FINAL_Contest.csv', encoding = 'UTF-8')
head(df)
row_names <- df[,1]
df <- df[,-1]
rownames(df) <- row_names
df

resque <- df[1:3]
animal <- df[4:9]
positive <- df[10]
negative <- df[11]

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


head(animal)
anal_FA_pre(animal)
w_animal <- anal_FA(animal, 1)
s_animal <- scale(animal, center = TRUE)
m_animal <- matrix(s_animal, nrow=25)
animal_MR1 <- m_animal  %*% w_animal[,1]

positive_MR1 <- scale(positive)
negative_MR1 <- scale(negative)

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
positive_PCA <- scale(positive)
negative_PCA <- scale(positive)

exportDF = data.frame(resque_MR1, animal_MR1, positive_MR1, negative_MR1,
                    resque_PCA, animal_PCA, positive_PCA, negative_PCA)
names(exportDF) <- c('resque_MR1', 'animal_MR1', 'positive_MR1', 'negative_MR1',
                     'resque_PCA', 'animal_PCA', 'positive_PCA', 'negative_PCA')

write.csv(exportDF,file="./data/DR_result.csv", encoding = "UTF-8")
