install.packages("readr")
install.packages("dplyr")
library(readr)
library(dplyr)

data3 <- data.frame(read_csv('SWS_CASE_SEARCH_V.csv'),stringsAsFactors = FALSE)
data3 <- select(data3,-(70:99))
data3 <- select(data3,-(66:69))
data3 <- mutate(data3,Parent_CATEGORY_ID = substr(data3$CATEGORY_ID,1,4))
#Par_ID <- read.csv(file = 'C:\\Users\\Weiting\\Desktop\\主類別對應表.csv',sep = ',',header = TRUE,stringsAsFactors = FALSE)
#data3 <- left_join(data3,Par_ID,by = 'Parent_CATEGORY_ID')
#data3$主類別[which(data3$CATEGORY_ID == 'AC0907')] <- '非屬前述各類之其他事項'


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

##建立keyword_table
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

#process category
install.packages("stringr")
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
E <- Process_cate(data3,Keyword_table)
  E_index <- lapply(E,function(X){
    if(max(X) > 1){
      if(length(which(X == max(X))) == 1){#內文match到的次類別為一個
        which.max(X)
      }
      else{
        min_k <- which.min(sapply(which(X == max(X)),function(x){
          length(Keyword_table[[x]])
        }))
        return(which(X == max(X))[min_k])
      }
    }else{
      return(0)
    }
  })


F_main<-sapply(E_index,function(x){
  if(x != 0)
    category_table$主類別[x]
})
F_sub<-sapply(E_index,function(x){
  if(x != 0)
    category_table$次類別[x]
})
F_main <- as.character(F_main)
F_sub <- as.character(F_sub)
#

data3 <- mutate(data3,New_PARENT_CATEGORY_NAME_TW = F_main,New_CATEGORY_NAME_TW = F_sub)
data3$New_PARENT_CATEGORY_NAME_TW[data3$New_PARENT_CATEGORY_NAME_TW == 'NULL'] <- '非屬前述各類之其他事項'
data3$New_CATEGORY_NAME_TW[data3$New_CATEGORY_NAME_TW == 'NULL'] <- '其他事項'
#

statis <- as.data.frame(table(data3$CATEGORY_NAME_TW))
colnames(statis) <- c('次類別','數量')
statis <- left_join(statis,select(category_table,1,2),by = '次類別')
statis <- select(statis,3,1,2)
#

Not_category <- filter(data3,New_PARENT_CATEGORY_NAME_TW == "非屬前述各類之其他事項")
key_82 <- read_csv('keyword.csv')
key_82 <- select(key_82,2:7,9:31,33:57,60:82)
E2 <- Process_cate(Not_category,key_82)
#

manual_cate <- read.csv(file = 'manual_cate.csv',sep = ',',header = TRUE,stringsAsFactors = FALSE)
colnames(manual_cate)[4] <- '主類別'
colnames(manual_cate)[5] <- '次類別'
manual_cate$次類別[manual_cate$次類別 == '山坡地管理'] <- '山坡地問題'
manual_cate$次類別[manual_cate$次類別 == '文化事務（古蹟文資維護、表演藝術、文化場館、藝文推廣）'] <- '文化事務（文化資產、表演藝術、文化場館、藝文推廣）'
manual_cate$次類別[manual_cate$次類別 == '交通標誌及設施物損壞(含汙損)、傾斜'] <- '交通標誌牌面損壞(含汙損)、傾斜'
manual_cate$次類別[manual_cate$次類別 == '污水下水道維護(含孔蓋冒水)'] <- '污水下水道維護(含孔蓋冒水)、污水管申接、污水排放稽查'
manual_cate$次類別[manual_cate$次類別 == '行人違規、自行車違規或違規行駛'] <- '行人違規、自行車違規及道路障礙違規'
manual_cate$次類別[manual_cate$次類別 == '產業發展(含工商業、農業、市場、公用事業、動物保護等)'] <- '產業發展(含工商業、農業、市場、公用事業等)'
manual_cate$次類別[manual_cate$次類別 == '無牌廢棄車(含汽、機車及自行車)、廢棄物處理(含醫療廢棄物)或資源回收'] <- '無牌廢棄車(含汽、機車及自行車)、廢棄物處理(含醫療廢棄物)及資源回收'
manual_cate$次類別[manual_cate$次類別 == '罰單問題'] <- '違規罰單問題'


cate <- distinct(manual_cate, manual_cate$次類別)
cate <- slice(cate,2:82)
colnames(cate) <- '次類別'
cate <- left_join(cate,select(category_table,1,2),by = '次類別')
cate$主類別[1] <- '交通運輸'
cate$主類別[5] <- '道路、山坡地、路樹及路燈'
cate$主類別[13] <- '警政、消防、政風及法律'
cate$主類別[21] <- '垃圾、噪音、污染及資源回收'
cate$主類別[23] <- '文化、教育、體育、觀光、媒體及資訊'
cate$主類別[31] <- '警政、消防、政風及法律'
cate$主類別[33] <- '產業、都市發展、建管、地政及財稅'
cate$主類別[42] <- '自來水、下水道、排水溝及公園'
cate <- filter(cate,!is.na(cate$主類別))

Category_result <- select(Not_category,APPLY_CONTENT,ASSIGN_OUID)
Category_result <-mutate(Category_result,CATEGORY_NAME_TW = F_sub,PARENT_CATEGORY_NAME_TW = F_main)
Category_result$PARENT_CATEGORY_NAME_TW[Category_result$PARENT_CATEGORY_NAME_TW == "NULL"] <- "非屬前述各類之其他事項"
Category_result$CATEGORY_NAME_TW[Category_result$CATEGORY_NAME_TW == "NULL"] <- "其他事項"
#將DATA中的CATEGORY_NAME_TW更新
path <- file("Category_result.csv",encoding = "UTF-8")
write.csv(DATA,file = path,row.names = FALSE)
#
index <- which(data3$New_PARENT_CATEGORY_NAME_TW == "非屬前述各類之其他事項")
data3$New_CATEGORY_NAME_TW[index[1:length(index)]] <- Category_result$CATEGORY_NAME_TW[1:nrow(Category_result)]
data3$New_PARENT_CATEGORY_NAME_TW[index[1:length(index)]] <- Category_result$PARENT_CATEGORY_NAME_TW[1:nrow(Category_result)]
statis <- as.data.frame(table(data3$New_CATEGORY_NAME_TW))
colnames(statis) <- c('次類別','數量')
statis <- left_join(statis,select(category_table,1,2),by = '次類別')
statis <- select(statis,3,1,2)



manual_pick <- read_csv(file = 'C:\\Users\\Weiting\\Desktop\\keyword.csv')
