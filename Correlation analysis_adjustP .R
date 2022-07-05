library("openxlsx")
#這邊要在特別注意一下輸入的參數，要調整你的欄名跟列名，sep.names是在設定你要用什麼取代data中的空格
data<-read.xlsx("C:/Users/USER/Desktop/台水計畫raw data for 相關性分析for r.xlsx",sheet=4,rowNames=T,sep.names=" ")
#相關性檢驗
library(Hmisc)
#因為rcorr()他的input要是matrix
data.matrix<-as.matrix(data)
corr<-rcorr(data.matrix,type= 'pearson')
corr_P_adj <- p.adjust(corr$P, method = 'BH')
matrix_corr_P_adj <- matrix(corr_P_adj,nrow=(length(corr$P)**0.5))
colnames(matrix_corr_P_adj)<-colnames(data)
rownames(matrix_corr_P_adj) <- colnames(data)
matrix_corr_P_adj[matrix_corr_P_adj>= 0.05] <- -1
matrix_corr_P_adj[matrix_corr_P_adj < 0.05 & matrix_corr_P_adj >= 0] <- 1
matrix_corr_P_adj[matrix_corr_P_adj == -1] <- 0
#我們先輸出一次有顯著相關但相關性未必足夠的矩陣
corr_significiant<-corr$r * matrix_corr_P_adj
#因為write.xlsx之output要是dataframe
corr_significiant.dataframe<-as.data.frame(corr_significiant)
write.xlsx(corr_significiant.dataframe, 'C:/Users/USER/Desktop/小型testp0.05.xlsx',rowNames=T,colNames=T,keepNA=T)
#接著將計算出來之相關性大於0.8且p小於0.05者留下
corr$r[corr$r < 0.8] <- 0
corr_final <-corr$r * matrix_corr_P_adj
#因為計算相關性只會有半邊的矩陣(上面是多餘的)所以我們只會需要下三角矩陣，且不需要對角矩陣(都為1)
corr_final[!lower.tri(corr_final)] <- 0
#有些數據因為是0所以算不出相關性(na)，我們把她去除
corr_final[is.na(corr_final)]<-0
corr_final.dataframe<-as.data.frame(corr_final)
write.xlsx(corr_final.dataframe, 'C:/Users/USER/Desktop/小型test.xlsx',rowNames=T,colNames=T)
