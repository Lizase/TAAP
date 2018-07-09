library(readr)
library(dplyr)

data3 <- data.frame(read_csv('SWS_CASE_SEARCH_V.csv'),stringsAsFactors = FALSE)
data3 <- select(data3,-(70:99))
data3 <- select(data3,-(66:69))
data3 <- mutate(data3,Parent_CATEGORY_ID = substr(data3$CATEGORY_ID,1,4))
other <- filter(data3,data3$CATEGORY_NAME_TW == '其他事項')
category_table <- read.table('Cate_refer.csv',header = TRUE,sep = ",",stringsAsFactors = FALSE)
#category_table[[2]][22] <- '07.橋梁、隧道、車( 人) 行地下道及涵洞積淹水'
#category_table[[2]][36] <- '06.道路側溝內溝牆(底)破損'
#category_table[[2]][38] <- '08.道路側溝溝蓋(含周邊)損壞遺失'
#category_table[[2]][39] <- '09.人孔蓋(含周邊)破損、遺失處理'
#category_table[[2]][54] <- '03.交通號誌電纜線垂落及設施損壞'
category_table <- slice(category_table,1:111)
category_table[[1]][2:15] <- category_table[[1]][1]
category_table[[1]][17:30] <- category_table[[1]][16]
category_table[[1]][32:42] <- category_table[[1]][31]
category_table[[1]][44:51] <- category_table[[1]][43]
category_table[[1]][53:63] <- category_table[[1]][52]
category_table[[1]][65:77] <- category_table[[1]][64]
category_table[[1]][79:95] <- category_table[[1]][78]
category_table[[1]][97:111] <- category_table[[1]][96]
category_table[[1]][1:95] <-substr(category_table[[1]][1:95],3,nchar(category_table[[1]][1:95]))
category_table[[1]][96:111] <-substr(category_table[[1]][96:111],4,nchar(category_table[[1]][96:111]))
category_table[[2]][1:111] <-substr(category_table[[2]][1:111],4,nchar(category_table[[2]][1:111]))
category_table<-filter(category_table,category_table$次類別 != '其他事項')
install.packages("jiebaR")
install.packages("jiebaRD")
library(jiebaR)
library(jiebaRD)
mixseg = worker()
Keyword <- paste(category_table$主類別, category_table$次類別)
Creat_keyword_table <- function(X){
  Keyword_A <- mixseg <= Keyword[X]
  Keyword_A <- unique(Keyword_A)
  Keyword_A <- Keyword_A[nchar(Keyword_A[1:length(Keyword_A)])>=2]#

}
Keyword_table <- sapply(1:length(category_table$次類別) ,Creat_keyword_table)
library(stringr)
library(parallel)
Process_cate <- function(Not_category,Keyword_table){
  cl = makeCluster(3)
  clusterEvalQ(cl, library(stringr))
  clusterExport(cl, c("Not_category", "Keyword_table"),envir=environment())
  E <- parLapply(cl, Not_category$APPLY_CONTENT[1:nrow(Not_category)],function(applyContent){
    as.numeric(lapply(Keyword_table,function(keywordTable){
      sum(str_count(applyContent,keywordTable),na.rm = TRUE)
      #mean(str_count(applyContent,keywordTable),na.rm = TRUE)
    }))
  })
  return(E)
  stopCluster(cl)
}
E3 <- Process_cate(dat,Keyword_table)
E3_index <- lapply(E3,function(X){
  if(max(X) > 0){
    if(length(which(X == max(X))) == 1){#內文match到的次類別為一個
      which.max(X)
    }
    else{
      return(which(X == max(X))[1])
    }
  }else{
    return(0)
  }
})
