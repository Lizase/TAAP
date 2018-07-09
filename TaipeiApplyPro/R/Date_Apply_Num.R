data3$APPLY_DTE <- as.POSIXct(data3$APPLY_DTE)
data3 <- mutate(data3,DTE = as.Date.POSIXct(data3$APPLY_DTE))
Date <- data3$DTE
次類別 <- data3$CATEGORY_NAME_TW
Dte_Num <- data.frame(次類別,Date)
test <- sapply(T_cate,function(Tabl_cate){
  eve_kind  <- which(Dte_Num$次類別 == Tabl_cate)#每一種次類別有幾個位置在哪
  unlist(sapply(T_date, function(Tabl_date){
    Dte <- Dte_Num$Date[eve_kind]
    length(Dte[Dte == Tabl_date])
  }))
})
test <- as.data.frame(test)
row.names(test)[1:length(T_date)] <- as.character(T_date[1:length(T_date)])
old_cate_tabl <- as.data.frame(T_cate)
colnames(old_cate_tabl) <- '次類別'
