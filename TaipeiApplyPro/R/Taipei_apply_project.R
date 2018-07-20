install.packages("readr")
install.packages("dplyr")
library(readr)
library(dplyr)
#讀取44620陳情資料
data <- data.frame(read_csv('SWS_CASE_SEARCH_V.csv'),stringsAsFactors = FALSE)
data <- select(data,-(70:99))
data <- select(data,-(66:69))
data <- mutate(data,Parent_CATEGORY_ID = substr(data$CATEGORY_ID,1,4))

#讀取分類表
library(readxl)
category_table <- read_excel("Cate_refer.xlsx",sheet = 1,range = "A2:B113")

#處理分類表的文字(將主類別補齊去除編號)
na_index <- which(!is.na(category_table$主類別))
count <- sapply(1:8,function(x){
  if(x == 8)
    nrow(category_table) - na_index[x] + 1
  else
   na_index[x+1] - na_index[x]
})
PC <- sapply(1:8, function(x){
  sapply(1:count[x],function(y){
    return(category_table$主類別[na_index[x]])
  })
})
PC <- unlist(PC)
category_table$主類別[1:nrow(category_table)] <- PC[1:length(PC)]
category_table[[1]][1:95] <-substr(category_table[[1]][1:95],3,nchar(category_table[[1]][1:95]))
category_table[[1]][96:nrow(category_table)] <-substr(category_table[[1]][96:nrow(category_table)],4,nchar(category_table[[1]][96:nrow(category_table)]))
category_table[[2]][1:nrow(category_table)] <-substr(category_table[[2]][1:nrow(category_table)],4,nchar(category_table[[2]][1:nrow(category_table)]))
category_table <- filter(category_table,category_table$次類別 != '其他事項')

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
  Keyword_A <- Keyword_A[nchar(Keyword_A[1:length(Keyword_A)])>=2]#取字數2以上的單詞

}
Keyword_table <- sapply(1:length(category_table$次類別) ,Creat_keyword_table)

#每筆內文中每種keyword的出現數
install.packages("stringr")
library(stringr)
library(parallel)
Process_cate <- function(Not_category,Keyword_table){
  cl = makeCluster(3)
  clusterEvalQ(cl, library(stringr))
  clusterExport(cl, c("Not_category", "Keyword_table"),envir=environment())
  KeyNum <- parLapply(cl, Not_category$APPLY_CONTENT[1:nrow(Not_category)],function(applyContent){
    as.numeric(lapply(Keyword_table,function(keywordTable){
      sum(str_count(applyContent,keywordTable),na.rm = TRUE)
      #mean(str_count(applyContent,keywordTable),na.rm = TRUE)
    }))
  })
  return(KeyNum)
  stopCluster(cl)
}

#找出keyword出現數最多的次類別
Set_cate_index <- function(KeyNum){
  lapply(KeyNum,function(X){
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
}
KeyNum <- Process_cate(data,Keyword_table)
MaxIndex <- Set_cate_index(KeyNum)

#以index找出相應的主、次類別
MainCate<-sapply(MaxIndex,function(x){
  if(x != 0)
    category_table$主類別[x]
})
SubCate<-sapply(MaxIndex,function(x){
  if(x != 0)
    category_table$次類別[x]
})
MainCate <- as.character(MainCate)
SubCate <- as.character(SubCate)

#重新歸類447620筆陳情內文的類別
data <- mutate(data,New_PARENT_CATEGORY_NAME_TW = MainCate,New_CATEGORY_NAME_TW = SubCate)
data$New_PARENT_CATEGORY_NAME_TW[data$New_PARENT_CATEGORY_NAME_TW == 'NULL'] <- '非屬前述各類之其他事項'
data$New_CATEGORY_NAME_TW[data$New_CATEGORY_NAME_TW == 'NULL'] <- '其他事項'

#選出主類別仍為非屬前述各類之其他事項的陳情
NotCate <- filter(data,New_PARENT_CATEGORY_NAME_TW == "非屬前述各類之其他事項")

#讀取人工斷詞的keyword
Key_82 <- read_csv('keyword.csv')
Key_82 <- select(Key_82,2:8,10:50,52:58,61:82)

#以人工斷詞的keyword對Notcate再次比對
KeyNum_2 <- Process_cate(NotCate,Key_82)
MaxIndex_2 <- Set_cate_index(KeyNum_2)

#從 manual_cate.xlsx 3232筆市府人工歸類的資料中製作歸類表cate
manual_cate <- read_excel('manual_cate.xlsx',sheet = 1,range = "C3:E3235")
colnames(manual_cate)[2] <- '主類別'
colnames(manual_cate)[3] <- '次類別'
manual_cate$次類別[manual_cate$次類別 == '山坡地管理'] <- '山坡地問題'
manual_cate$次類別[manual_cate$次類別 == '文化事務（古蹟文資維護、表演藝術、文化場館、藝文推廣）'] <- '文化事務（文化資產、表演藝術、文化場館、藝文推廣）'
manual_cate$次類別[manual_cate$次類別 == '污水下水道維護(含孔蓋冒水)'] <- '污水下水道維護(含孔蓋冒水)、污水管申接、污水排放稽查'
manual_cate$次類別[manual_cate$次類別 == '行人違規、自行車違規或違規行駛'] <- '行人違規、自行車違規及道路障礙違規'
manual_cate$次類別[manual_cate$次類別 == '產業發展(含工商業、農業、市場、公用事業、動物保護等)'] <- '產業發展(含工商業、農業、市場、公用事業等)'
manual_cate$次類別[manual_cate$次類別 == '無牌廢棄車(含汽、機車及自行車)、廢棄物處理(含醫療廢棄物)或資源回收'] <- '無牌廢棄車(含汽、機車及自行車)、廢棄物處理(含醫療廢棄物)及資源回收'
manual_cate$次類別[manual_cate$次類別 == '罰單問題'] <- '違規罰單問題'
cate <- distinct(manual_cate, manual_cate$次類別)
cate <- slice(cate,2:nrow(cate))
colnames(cate) <- '次類別'
cate <- left_join(cate,select(category_table,1,2),by = '次類別')
cate <- filter(cate,!is.na(cate$主類別))

#第二次分類的主次類別
MainCate<-sapply(MaxIndex_2,function(x){
  if(x != 0)
    cate$主類別[x]
})
SubCate<-sapply(MaxIndex_2,function(x){
  if(x != 0)
    cate$次類別[x]
})
MainCate <- as.character(MainCate)
SubCate <- as.character(SubCate)

#將第二次分類的主次類別設定到仍無法分類的1611171筆資料中
Category_result <- select(NotCate,APPLY_CONTENT,ASSIGN_OUID)
Category_result <-mutate(Category_result,CATEGORY_NAME_TW = SubCate,PARENT_CATEGORY_NAME_TW = MainCate)
Category_result$PARENT_CATEGORY_NAME_TW[Category_result$PARENT_CATEGORY_NAME_TW == "NULL"] <- "非屬前述各類之其他事項"
Category_result$CATEGORY_NAME_TW[Category_result$CATEGORY_NAME_TW == "NULL"] <- "其他事項"

#把161117筆挑選的資料設定回446720筆資料中
index <- which(data$New_PARENT_CATEGORY_NAME_TW == "非屬前述各類之其他事項")
data$New_CATEGORY_NAME_TW[index[1:length(index)]] <- Category_result$CATEGORY_NAME_TW[1:nrow(Category_result)]
data$New_PARENT_CATEGORY_NAME_TW[index[1:length(index)]] <- Category_result$PARENT_CATEGORY_NAME_TW[1:nrow(Category_result)]

#統計分類後的結果
statis <- as.data.frame(table(data$New_CATEGORY_NAME_TW))
colnames(statis) <- c('次類別','數量')
statis <- left_join(statis,select(category_table,1,2),by = '次類別')
statis <- select(statis,3,1,2)



manual_pick <- read_csv(file = 'C:\\Users\\Weiting\\Desktop\\keyword.csv')
