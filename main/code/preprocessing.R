library(readxl)
rm(list=ls())
df <- read_excel("./data/EDA_FINAL.xlsx" , na = "NA")
name = df[,1]
df = df[,-1]
df = data.frame(df, row.names = name$구별)
head(df)
scale_df = scale(df)

human = cbind(df[,1:7], df[,12:49])
head(human)
animal <- cbind(df[,8:11], df[,50:52], df[,55:66])
head(animal)
infra <- cbind(df[,53:54])  # 동물보호소는 제외 : 상관성이 너무 낮음


########################################################################
# 상관행렬
show_cor <- function(df.data) {
  library(corrplot)
  s_data = scale(df.data, center = TRUE)
  cor_h = cor(s_data)
  corrplot(cor_h, is.corr = FALSE, method = "square")
}

show_cor(human)
discor_human_1 = cbind(human[,c(-3,-5,-7,-10,-11, -45)])  # 단독주택, 연립주택, 월소득, 기타, 농림어업 숙련 종사자, 100세 이상
show_cor(discor_human_1)

result <- discor_human_1
home <- cbind(result[,3:4])
job <- cbind(result[,5:12])
gagu <- cbind(result[,13:19])
age <- cbind(result[,20:39])

show_cor(animal)
result_anim <- animal[, c(-5, -6, -7, -15, -16)]
show_cor(result_anim)

home_anim <- cbind(result_anim[,1:4])
state_anim <- cbind(result_anim[,5:11])
another_anim <- cbind(result_anim[,12:14])

########################################################################
########################################################################
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

# n인가구 : 2개
head(gagu)
anal_FA_pre(gagu)
w_gagu <- anal_FA(gagu, 2)
s_gagu <- scale(gagu, center = TRUE)
m_gagu <- matrix(s_gagu, nrow=25)
gagu_MR1 <- m_gagu  %*% w_gagu[,1]  # 다인가구
gagu_MR2 <- m_gagu  %*% w_gagu[,2]  # 1인가구
avg_gagu_MR1 <- apply(s_gagu[,2:7], 1, sum) / ncol(s_gagu[,2:7])  # 다인가구
avg_gagu_MR2 <- s_gagu[,1] # 1인가구

# 직업 : 2개
anal_FA_pre(job)
w_job <- anal_FA(job, 2)
s_job <- scale(job, center = TRUE)
m_job <- matrix(s_job, nrow=25)
job_MR1 <- m_job  %*% w_job[,1]  # 단순노무종사자, 기능원 및 관련기능종사자, 장치기계조작 및 조립종사자, 판매종사자, 서비스종사자
job_MR2 <- m_job  %*% w_job[,2]  # 전문가 및 관련 종사자, 관리자, 사무종사자
avg_job_MR1 <- apply(s_job[,c(-1,-4,-7)],1,sum) / ncol(s_job[,c(-1,-4,-7)])
avg_job_MR2 <- apply(s_job[,c(1,4,7)],1,sum) / ncol(s_job[,c(1,4,7)])


# 연령 : 3개
anal_FA_pre(age)
w_age <- anal_FA(age, 3)
s_age <- scale(age, center = TRUE)
m_age <- matrix(s_age, nrow=25)
age_MR1 <- m_age  %*% w_age[,1]  # 그 이외
age_MR2 <- m_age  %*% w_age[,2]  # , 60~80 소비력있는 고령층
age_MR3 <- m_age  %*% w_age[,3]  # 20~25 , 25~40 소비력있는 젊은층
avg_age_MR1 <- apply(s_age[, -c(5:8, 13:18)],1,sum) / ncol(s_age[, -c(5:8, 13:18)])
avg_age_MR2 <- apply(s_age[,c(13:18)],1,sum) / ncol(s_age[,c(13:18)])  # 60 ~ 89
avg_age_MR3 <- apply(s_age[,c(5:8)],1,sum) / ncol(s_age[,c(5:8)])   # 20 ~ 39


# 주택 : 1개
anal_FA_pre(home)
w_home <- anal_FA(home, 1)
s_home <- scale(home, center = TRUE)
m_home <- matrix(s_home, nrow=25)
home_MR1 <- m_home  %*% w_home[,1]  # 아파트, 다세대 주택
avg_home_MR1 <- apply(s_home,1,sum) / ncol(s_home)


# 반려동물 주택 수
anal_FA_pre(home_anim)
w_home_anim <- anal_FA(home_anim, 2)
s_home_anim <- scale(home_anim, center = TRUE)
m_home_anim <- matrix(s_home_anim, nrow=25)
home_anim_MR1 <- m_home_anim  %*% w_home_anim[,1]  # 다세대, 연립 주택 / 주택 형태에 사람이 다수 사는 곳
home_anim_MR2 <- m_home_anim  %*% w_home_anim[,2]  # 아파트, 단독주택
avg_home_anim_MR1 <- apply(s_home_anim[, -c(1:2)],1,sum) / ncol(s_home_anim[, -c(1:2)])
avg_home_anim_MR2 <- apply(s_home_anim[, c(1:2)],1,sum) / ncol(s_home_anim[, c(1:2)]) 


# 반려동물 보호 현황
anal_FA_pre(state_anim)
w_state_anim <- anal_FA(state_anim, 2)
s_state_anim <- scale(state_anim, center = TRUE)
m_state_anim <- matrix(s_state_anim, nrow=25)
state_anim_MR1 <- m_state_anim  %*% w_state_anim[,1]  # 입양, 반환, 안락사 / 사람의 적극 개입
state_anim_MR2 <- m_state_anim  %*% w_state_anim[,2]  # 방사, 자연사, 보호중  / 사람의 소극 개입 및 방치
avg_state_anim_MR1 <- apply(s_state_anim[, -c(1,5,6,7)],1,sum) / ncol(s_state_anim[, -c(1,5,6,7)])  # 기증은 제외
avg_state_anim_MR2 <- apply(s_state_anim[, c(1, 6,7)],1,sum) / ncol(s_state_anim[, c(1, 6,7)])    # 방사, 자연사, 보호중


# 그 외
anal_FA_pre(another_anim)
w_another_anim <- anal_FA(another_anim, 1)
s_another_anim <- scale(another_anim, center = TRUE)
m_another_anim <- matrix(s_another_anim, nrow=25)
another_anim_MR1 <- m_another_anim  %*% w_another_anim[,1] # 동물 등록수, 반령동물 인구, 총 유기동물수
avg_another_anim_MR1 <- apply(s_another_anim[, c(1:3)],1,sum) / ncol(s_another_anim[, c(1:3)])  # 기증은 제외

new_df <- cbind(df[,1:2], 
                gagu_MR1, gagu_MR2,
                job_MR1, job_MR2,
                age_MR1, age_MR2, age_MR3,
                home_MR1, 
                home_anim_MR1, home_anim_MR2,
                state_anim_MR1, state_anim_MR2,
                another_anim_MR1)
new_df_human <- cbind(df[,1:2], 
                gagu_MR1, gagu_MR2,
                job_MR1, job_MR2,
                age_MR1, age_MR2, age_MR3,
                home_MR1)
new_df_anim <- cbind(home_anim_MR1, home_anim_MR2,
                state_anim_MR1, state_anim_MR2,
                another_anim_MR1)


avg_new_df <- cbind(df[,1:2], 
                avg_gagu_MR1, avg_gagu_MR2,
                avg_job_MR1, avg_job_MR2,
                avg_age_MR1, avg_age_MR2, avg_age_MR3,
                avg_home_MR1, 
                avg_home_anim_MR1, avg_home_anim_MR2,
                avg_state_anim_MR1, avg_state_anim_MR2,
                avg_another_anim_MR1)

avg_new_df_human <- cbind(df[,1:2], 
                avg_gagu_MR1, avg_gagu_MR2,
                avg_job_MR1, avg_job_MR2,
                avg_age_MR1, avg_age_MR2, avg_age_MR3,
                avg_home_MR1)

avg_new_df_anim <- cbind(avg_home_anim_MR1, avg_home_anim_MR2,
                avg_state_anim_MR1, avg_state_anim_MR2,
                avg_another_anim_MR1)

########################################################################
########################################################################

anal_FA_pre(new_df)
w_new_df <- anal_FA(new_df, 3)
s_new_df <- scale(new_df, center = TRUE)
m_new_df <- matrix(s_new_df, nrow=25)
new_df_MR1 <- m_new_df  %*% w_new_df[,1]
new_df_MR2 <- m_new_df  %*% w_new_df[,2]
new_df_MR3 <- m_new_df  %*% w_new_df[,3]


anal_FA_pre(new_df_human)
w_new_df_human <- anal_FA(new_df_human, 1)
s_new_df_human <- scale(new_df_human, center = TRUE)
m_new_df_human <- matrix(s_new_df_human, nrow=25)
new_df_human_MR1 <- m_new_df_human  %*% w_new_df_human[,1]

anal_FA_pre(new_df_anim)
w_new_df_anim <- anal_FA(new_df_anim, 3)
s_new_df_anim <- scale(new_df_anim, center = TRUE)
m_new_df_anim <- matrix(s_new_df_anim, nrow=25)
new_df_anim_MR1 <- m_new_df_anim  %*% w_new_df_anim[,1]
new_df_anim_MR2 <- m_new_df_anim  %*% w_new_df_anim[,2]
new_df_anim_MR3 <- m_new_df_anim  %*% w_new_df_anim[,3]

anal_FA_pre(avg_new_df)
w_avg_new_df <- anal_FA(avg_new_df, 3)
s_avg_new_df <- scale(avg_new_df, center = TRUE)
m_avg_new_df <- matrix(s_avg_new_df, nrow=25)
avg_new_df_MR1 <- m_avg_new_df  %*% w_avg_new_df[,1]
avg_new_df_MR2 <- m_avg_new_df  %*% w_avg_new_df[,2]
avg_new_df_MR3 <- m_avg_new_df  %*% w_avg_new_df[,3]

anal_FA_pre(avg_new_df_human)
w_avg_new_df_human <- anal_FA(avg_new_df_human, 1)
s_avg_new_df_human <- scale(avg_new_df_human, center = TRUE)
m_avg_new_df_human <- matrix(s_avg_new_df_human, nrow=25)
avg_new_df_human_MR1 <- m_avg_new_df_human  %*% w_avg_new_df_human[,1]

anal_FA_pre(avg_new_df_anim)
w_avg_new_df_anim <- anal_FA(avg_new_df_anim, 3)
s_avg_new_df_anim <- scale(avg_new_df_anim, center = TRUE)
m_avg_new_df_anim <- matrix(s_avg_new_df_anim, nrow=25)
avg_new_df_anim_MR1 <- m_avg_new_df_anim  %*% w_avg_new_df_anim[,1]
avg_new_df_anim_MR2 <- m_avg_new_df_anim  %*% w_avg_new_df_anim[,2]
avg_new_df_anim_MR3 <- m_avg_new_df_anim  %*% w_avg_new_df_anim[,3]

infra <- infra[,2]
colnames(infra) <- c("infra")
head(infra)
region = c('강남구', '강동구','강북구','강서구','관악구','광진구',
           '구로구','금천구','노원구','도봉구','동대문구','동작구',
           '마포구','서대문구','서초구','성동구','성북구','송파구',
           '양천구','영등포구','용산구','은평구','종로구','중구','중랑구')

exportDF <- data.frame(new_df_MR1, new_df_MR2, new_df_MR3, infra,
      new_df_human_MR1, new_df_anim_MR1, new_df_anim_MR2, new_df_anim_MR3, infra,
      avg_new_df_MR1, avg_new_df_MR2, avg_new_df_MR3, infra,
      avg_new_df_human_MR1,avg_new_df_anim_MR1, avg_new_df_anim_MR2, avg_new_df_anim_MR3, infra
      )

rownames(exportDF) <- region
head(exportDF)
library(openxlsx)
write.xlsx(exportDF, sheetName = "sheet1", file = "./data/FA_RESULT.xlsx")

########################################################################
########################################################################
# PCA

anal_pca <- function(df.data) {
  library(factoextra)
  pca_data <- prcomp(df.data, center = T, scale. = T)  
  screeplot(pca_data, main="", col="blue", type="lines", pch=3, npcs = length(pca_data$sdev))
  fviz_pca_ind(pca_data, col.ind="cos2")   
  biplot(pca_data)
  print(summary(pca_data))
  
  return(pca_data$rotation)
}

w_pca <- anal_pca(new_df)
s_pca <-scale(new_df, center=TRUE)
m_pca <- matrix(s_pca, nrow=25)
df_PC1 <- m_pca  %*% w_pca[,1]
df_PC2 <- m_pca  %*% w_pca[,2]
df_PC3 <- m_pca  %*% w_pca[,3]
df_PC4 <- m_pca  %*% w_pca[,4]


w_pca_avg <- anal_pca(avg_new_df)
s_pca_avg <-scale(avg_new_df, center=TRUE)
m_pca_avg <- matrix(s_pca_avg, nrow=25)
df_avg_PC1 <- m_pca_avg  %*% w_pca_avg[,1]
df_avg_PC2 <- m_pca_avg  %*% w_pca_avg[,2]



w_pca_human <- anal_pca(new_df_human)
s_pca_human <-scale(new_df_human, center=TRUE)
m_pca_human <- matrix(s_pca_human, nrow=25)
human_PC1 <- m_pca_human  %*% w_pca_human[,1]
human_PC2 <- m_pca_human  %*% w_pca_human[,2]

w_pca_anim <- anal_pca(new_df_anim)
s_pca_anim <-scale(new_df_anim, center=TRUE)
m_pca_anim <- matrix(s_pca_anim, nrow=25)
anim_PC1 <- m_pca_anim  %*% w_pca_anim[,1]
anim_PC2 <- m_pca_anim  %*% w_pca_anim[,2]
anim_PC3 <- m_pca_anim  %*% w_pca_anim[,3]
anim_PC4 <- m_pca_anim  %*% w_pca_anim[,4]

w_pca_avg_human <- anal_pca(avg_new_df_human)
s_pca_avg_human <-scale(avg_new_df_human, center=TRUE)
m_pca_avg_human <- matrix(s_pca_avg_human, nrow=25)
avg_human_PC1 <- m_pca_avg_human  %*% w_pca_avg_human[,1]
avg_human_PC2 <- m_pca_avg_human  %*% w_pca_avg_human[,2]


w_pca_avg_anim <- anal_pca(avg_new_df_anim)
s_pca_avg_anim <-scale(avg_new_df_anim, center=TRUE)
m_pca_avg_anim <- matrix(s_pca_avg_anim, nrow=25)
avg_anim_PC1 <- m_pca_avg_anim  %*% w_pca_avg_anim[,1]
avg_anim_PC2 <- m_pca_avg_anim  %*% w_pca_avg_anim[,2]

export_PCA_DF <- data.frame(df_PC1, df_PC2, df_PC3, df_PC4, infra,
                            human_PC1, human_PC2, anim_PC1, anim_PC2, anim_PC3, anim_PC4, infra,
                            df_avg_PC1, df_avg_PC2, infra,
                            avg_human_PC1, avg_human_PC2, avg_anim_PC1, avg_anim_PC2, infra
)

rownames(export_PCA_DF) <- region
head(export_PCA_DF)
library(openxlsx)
write.xlsx(export_PCA_DF, sheetName = "sheet1", file = "./data/PCA_RESULT.xlsx")

########################################################################