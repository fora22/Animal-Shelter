rm(list=ls())
df <- read.csv("./data/최종_그리드.csv")
df[is.na(df)] <- 0
X <- df[, c(6:10, 12)]
s_X <- scale(X, center = TRUE)

result <- apply(s_X, 1, sum)

export_df <- cbind(df[,1], result, scale(result, center = TRUE))



colnames(export_df) <- c("id", "Score", "ScaleScore")
write.csv(x=export_df,file='./data/all_score.csv')

Score <- result
ScaleScore <- scale(result, center = TRUE)
report <- cbind(df[,c(1:10, 12)], Score, ScaleScore)
head(report)
write.csv(x=report,file='./data/all_score_for_report.csv')
