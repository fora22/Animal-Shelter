library(readxl)
rm(list=ls())
FA_Result <- read_excel("./data/FA_RESULT.xlsx" , na = "NA")


weight <- c(0.153145333426747
            , 0.140775127848327
            , 0.270466021730839
            , 0.227260323615609
            , 0.0629030982838324)

weight

FA_B <- FA_Result[,5:9]
FA_D <- FA_Result[,14:18]

s_FA_B <- scale(FA_B, center = TRUE)
s_FA_D <- scale(FA_D, center = TRUE)

s_FA_B * weight

sw_FA_B <- s_FA_B * weight
r_FA_B <-apply(sw_FA_B, 1, sum)   # result_FA_B
r_FA_B

sw_FA_D <- s_FA_D * weight
r_FA_D <-apply(sw_FA_D, 1, sum)  # result_FA_D
r_FA_D

region = c('강남구', '강동구','강북구','강서구','관악구','광진구',
           '구로구','금천구','노원구','도봉구','동대문구','동작구',
           '마포구','서대문구','서초구','성동구','성북구','송파구',
           '양천구','영등포구','용산구','은평구','종로구','중구','중랑구')

score_AHP_B <- r_FA_B
score_AHP_D <- r_FA_D
AHP_RESULT <- data.frame(cbind(score_AHP_B, score_AHP_D))
rownames(AHP_RESULT) <- region
head(AHP_RESULT)

write.csv(AHP_RESULT,file = "./data/AHP_RESULT.csv")
