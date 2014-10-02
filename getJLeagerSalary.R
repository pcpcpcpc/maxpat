get.salary <- function(url.str, sum.action=TRUE){
	#内部関数　億除去
	nkf_remove_ahm <- function(num.str=c("","")){
		fun1 <- function(num.str="1億"){
			num.str <- as.character(num.str="1億")
			if(!is.na(as.integer(num.str))){
				return(as.integer(num.str))
			}else if(num.str=="年俸記載なし" || num.str=="記載なし"){
				return(NA)
			}else{
				chars <- strsplit(num.str,"")[[1]]
				if(any(chars %in% "億") == length(chars)){
					if(which(chars %in% "億") == length(chars)){
						return(as.integer(twobyte2onebyte(chars[which(chas %in% "億") - 1])) * 10000)
					}else{
						return(
							   as.integer(twobyte2onebyte(chars[which(chars %in% "億") - 1])) * 10000
							   +
							   as.integer(nkf(substr(num.str,(which(chars %in% "億") + 1),length(chars))))
							   )
					}
				}else{
					return(nkf(num.str))
				}
			}
		}
		return(apply(as.matrix(num.str),1,fun1))
	}
	#内部関数_全角数字→半角数字に変換
	nkf <- function(num.str = "3000"){
		chars <- strsplit(num.str,"")[[1]]
		ret.num <- paste(apply(matrix(chars),1,twobyte2onebyte),sep="",collapse="")
		return(ifelse(ret.num=="",0,as.integer(ret.num)))
	}
	#内部関数_2byte_to_1byte
	twobyte2onebyte <- function(x="3"){
		switch(x, 0 = return("0"), 1 = return("1"), 2 = return("2"),
			   3 = return("3"), 4 = return("4"), 5 = return("5"),
			   6 = return("6"), 7 = return("7"), 8 = return("8"),
			   9 = return("9"), return(""))
	}
	library(XML)
	library(plyr)
	tbl <- readHTMLTable(url.str)[[1]]
	if(any(colnames(tbl) %in% "年俸")){
		ret.list <- dlply(.data=tbl,.variables="ポジション",.fun=function(x){nkf_remove_ahm(x[,"年俸"])})
	}else if(any(colnames(tbl) %in% "年俸(万円")){
		ret.list <- dlply(.data=tbl,.variables="ポジション",.fun=function(x){nkf_remove_ahm(x[,"年俸(万円)"])})
	}else{
		ret.list <- dlply(.data=tbl,.variables="ポジション",.fun=function(x){nkf_remove_ahm(x[,4])})
	}
	if(sum.action){
		sapply(ret.list,sum)
	}else{
		return(ret.list)
	}
}
