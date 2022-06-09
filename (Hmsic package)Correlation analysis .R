data<-read.csv("arg_test1.csv",row.names = 1, header=TRUE)
data.matrix<-as.matrix(data)
library(Hmisc)
corrx <- rcorr(data.matrix,type= 'spearman')
corr<-as.list(corrx)
corr$P[corr$P >= 0.05] <- -1
corr$P[corr$P < 0.05 & corr$P >= 0] <- 1
corr$P[corr$P == -1] <- 0
#我們先輸出一次有顯著相關但相關性未必足夠的矩陣
corr_significiant<-corr$r * corr$P
write.csv(corr_significiant, 'corr_p0.05.csv', quote = FALSE)
#接著將計算出來之相關性大於0.7且p小於0.05者留下
corr$r[corr$r < 0.8] <- 0
corr_final <-corr$r * corr$P
#因為計算相關性只會有半邊的矩陣(上面是多餘的)所以我們只會需要下三角矩陣，且不需要對角矩陣(都為1)
corr_final[!lower.tri(corr_final)] <- 0
#有些數據因為是0所以算不出相關性(na)，我們把她去除
corr_final[is.na(corr_final)]<-0
write.csv(corr_final, 'arg_test1_r0.8p0.05.csv', quote = FALSE)
