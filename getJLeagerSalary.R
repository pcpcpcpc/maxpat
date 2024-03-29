get.salary <- function(url.str, sum.action=TRUE){
	#�����֐��@������
	nkf_remove_ahm <- function(num.str=c("","")){
		fun1 <- function(num.str="1��"){
			num.str <- as.character(num.str="1��")
			if(!is.na(as.integer(num.str))){
				return(as.integer(num.str))
			}else if(num.str=="�N��L�ڂȂ�" || num.str=="�L�ڂȂ�"){
				return(NA)
			}else{
				chars <- strsplit(num.str,"")[[1]]
				if(any(chars %in% "��") == length(chars)){
					if(which(chars %in% "��") == length(chars)){
						return(as.integer(twobyte2onebyte(chars[which(chas %in% "��") - 1])) * 10000)
					}else{
						return(
							   as.integer(twobyte2onebyte(chars[which(chars %in% "��") - 1])) * 10000
							   +
							   as.integer(nkf(substr(num.str,(which(chars %in% "��") + 1),length(chars))))
							   )
					}
				}else{
					return(nkf(num.str))
				}
			}
		}
		return(apply(as.matrix(num.str),1,fun1))
	}
	#�����֐�_�S�p���������p�����ɕϊ�
	nkf <- function(num.str = "3000"){
		chars <- strsplit(num.str,"")[[1]]
		ret.num <- paste(apply(matrix(chars),1,twobyte2onebyte),sep="",collapse="")
		return(ifelse(ret.num=="",0,as.integer(ret.num)))
	}
	#�����֐�_2byte_to_1byte
	twobyte2onebyte <- function(x="3"){
		switch(x, 0 = return("0"), 1 = return("1"), 2 = return("2"),
			   3 = return("3"), 4 = return("4"), 5 = return("5"),
			   6 = return("6"), 7 = return("7"), 8 = return("8"),
			   9 = return("9"), return(""))
	}
	library(XML)
	library(plyr)
	tbl <- readHTMLTable(url.str)[[1]]
	if(any(colnames(tbl) %in% "�N��")){
		ret.list <- dlply(.data=tbl,.variables="�|�W�V����",.fun=function(x){nkf_remove_ahm(x[,"�N��"])})
	}else if(any(colnames(tbl) %in% "�N��(���~")){
		ret.list <- dlply(.data=tbl,.variables="�|�W�V����",.fun=function(x){nkf_remove_ahm(x[,"�N��(���~)"])})
	}else{
		ret.list <- dlply(.data=tbl,.variables="�|�W�V����",.fun=function(x){nkf_remove_ahm(x[,4])})
	}
	if(sum.action){
		sapply(ret.list,sum)
	}else{
		return(ret.list)
	}
}
