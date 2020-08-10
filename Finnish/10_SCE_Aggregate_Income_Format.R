## Editing aggregate-level info as part of application TK-52-306-20, prepared by Kaisa Herranen <kaisa.herranen@stat.fi> from Statistic Finland

# Input
# Output

# We made three changes in original input EXCEL files: 
# (1) "Socioeconomic group: 54 Other workers" of the sheet "59" of the file "Income_1990_1993.xlsx" has changed to "Socioeconomic group: 59 Other workers"
# (2) "The mean income subject to state taxation" was added to row 2 of the sheet "41,f" of the file "Income_1995_2017.xlsx"    # all other sheets have this row
# (3) 1st column and 9th row of the sheet "70, f" of the file "Income_1995_2017.xlsx" should be changed from "10" to "19"




################################
#    Set working environment   #
################################


setwd("/Users/aoxliu/Documents/LRS/Data/FIN/SCE/")
library("readxl")
library(dplyr)



# Parameters
files <- c("Income_1990_1993.xlsx", "Income_1995_2017.xlsx")
year <- list("p1"=c(1990,1993), "p2"=c(1995, 2000, 2004:2017))


code <- list("p1"=c(10,20,31:34,41:44,51:54,59,60,70,91:93,99), 
             "p2"=c(paste0(rep(c(10,20,31:34,41:44,51:54,60,70,81:82,99), each=2), c("-f","-m"))))
             
print(setdiff(code$p1, substr(code$p2,1,2)))   # 59 91 92 93 specific for period 1
print(setdiff(substr(code$p2,1,2), code$p1))   # "81" "82" specific for period 2



# Functions
fmt <- function(x, f="123") {
    if(f=="123") {
    	x <- as.numeric(as.character(x))
    } else if (f=="abc"){
    	x <- as.character(x)
    }
    return(x)
}




plt <- function(dat, var, y_lab, m_lab, t_lab){
	y <- c(dat[ ,paste0(var,"_Male")], dat[ ,paste0(var,"_Female")])
	tiff(filename=paste0(t_lab,"_FINLAND.tiff"), res = 100, width = 1200, height = 800)
	plot(dat[ ,"Year"], dat[ ,paste0(var,"_Male")], ylim=c(min(y), max(y)), xlim=c(1990, 2020), type="b", col="Blue", lwd=2, main=paste0(m_lab, " for males (blue) and females (red) with statistical year"), xlab="Statistical year", ylab=y_lab)
	points(dat[ ,"Year"], dat[ ,paste0(var,"_Female")], type="b", col="red", lwd=2)
	dev.off()
}





#####################################################
#    Merge all info from EXCEL into one text file   #
#####################################################

#--------------------------------------------------
# Extract info (N and Eur) for each occupation-sex-age-year

AVG_INCOME <- NULL

# loop for EXCEL
for (f in 1:length(files)){       
	print(paste0("File ", f, "/", length(files)))
	file_name <- files[f]
	
	# loop for sheet
	n_sht <- length(excel_sheets(file_name)) -1 
	for (n in 1:n_sht){          
		print(paste0("Sheet ", n, "/", n_sht))
		s <- as.data.frame(read_excel(file_name, sheet=n+1, skip=6))
		
		# create matrix to store one set of information
		s_k <- matrix(NA, ncol=7, nrow=nrow(s))
		colnames(s_k) <- c("Age", "N", "Eur", "Year", "Sex", "Code", "Text")
		
		sce <- gsub("Socioeconomic group: ", "", read_excel(file_name, sheet=n+1, skip=1)[1,1])	
		s_k[ ,"Code"] <- strsplit(sce, " ")[[1]][1]
		s_k[ ,"Text"] <- gsub(paste0(strsplit(sce, " ")[[1]][1]," "), "", sce)	

		# loop for occupation-sex-age-year
		n_set <- (ncol(s)-1)/2   
		for (k in 1:n_set){      
			print(paste0("Set ", k, "/", n_set))
			s_k[ ,"Age"] <- s[ ,1]
			s_k[ ,"N"] <- s[, 2*k]
			s_k[ ,"Eur"] <- s[, 2*k+1]
					
			if (f==1){		
				s_k[ ,"Year"] <- ifelse(k %in% 1:2, year_1[1], year_1[2])
				s_k[ ,"Sex"]  <- ifelse(k %in% c(1,3), "Male", "Female")
			} else {
				s_k[ ,"Year"] <- year_2[k]
				s_k[ ,"Sex"]  <- ifelse(n%%2==1, "Female", "Male")
			}	
			
			# format s_k
			s_k <- as.data.frame(s_k)
			s_k[, c("Age","Sex","Code","Text")] <- lapply(s_k[, c("Age","Sex","Code","Text")], fmt, "abc")
			s_k[, c("N","Eur","Year")] <- lapply(s_k[, c("N","Eur","Year")], fmt, "123")
			
			AVG_INCOME <- rbind(AVG_INCOME, s_k)
		}
	
	}
} 

sum(is.na(AVG_INCOME$Eur))  # 1,660
write.table(AVG_INCOME, "AVG_INCOME.txt", append=F, quote=F, sep="\t", row.names=F, col.names=T)





########################################################################
#   Re-code occupation to be consistent across 1990-1993 & 1995-2017   #
########################################################################


# N for each code within each period
for (f in 1:2){
	AVG_IN_f <- subset(AVG_INCOME, Year %in% unlist(year[f]))
	print(paste0("Period ",f,": ",nrow(AVG_IN_f), " rows"))
	print(aggregate(N ~ Code, AVG_IN_f, sum))
}



# Recode by defintions ################
#######################################
#######################################

within(AVG_INCOME, {
	recode <- ifelse(Year %in% year[f] &  %in% Code)
	
})

Age     N   Eur Year  Sex Code                     Text
1 0-18 31227 31227 1990 Male   10 Farmer etc. entrepeneurs
2   19   289   289 1990 Male   10 Farmer etc. entrepeneurs






#############################################################
#   N and Eur for two time periods (1990-1993, 1995-2017)   #
#############################################################


# N and Average income for each statistical year

AVG_YS <- matrix(NA, nrow=length(c(year_1, year_2)), ncol=5)
colnames(AVG_YS) <- c("Year","Eur_Male", "Eur_Female", "N_Male", "N_Female")

n <- 1
for (y in c(year$p1, year$p2)){
	print(y)
	AVG_YS[n, "Year"] <- y
	
	for (sex in c("Male", "Female")){
		print(sex)
		ys <- AVG_INCOME[AVG_INCOME[,"Year"]==y & AVG_INCOME[,"Sex"]==sex, ]
		AVG_YS[n, paste0("N_",sex)] <- sum(ys$N, na.rm=T)
		AVG_YS[n, paste0("Eur_",sex)] <- weighted.mean(ys$Eur, ys$N, na.rm=T)		
	}
	print(AVG_YS[n, ]) 
	n <- n +1
}

print(AVG_YS)



# Plots

plt(dat=AVG_YS, var="Eur", y_lab="Income (Euro)", m_lab="Average income", t_lab="Average_income")
plt(dat=AVG_YS, var="N", y_lab="Sample size (#)", m_lab="Sample size", t_lab="Sample_size")





################################################
#   Plot N and Eur with years for each code    #
################################################

# sum of N and Euro by sex with statistical years

AVG_YSC <- matrix(NA, nrow=length(c(year_1, year_2)), ncol=6)
colnames(AVG_YSC) <- c("Year", "Code", "Eur_Male", "Eur_Female", "N_Male", "N_Female")


kod <- unique(c(code$p1, substr(code$p2,1,2)))
for (k in kod){
	print(paste0("Code ", which(kod==k), "/", length(kod)))
	n <- 1
	
	for (year in c(year_1, year_2)){
		AVG_YSC[n, "Year"] <- year
		AVG_YSC[n, "Code"] <- k
	
		for (sex in c("Male", "Female")){
			ys <- AVG_INCOME[AVG_INCOME[,"Year"]==year & AVG_INCOME[,"Sex"]==sex & AVG_INCOME[,"Code"]==k, ]
			AVG_YSC[n, paste0("N_",sex)] <- sum(ys[,"N"], na.rm=T)
			AVG_YSC[n, paste0("Eur_",sex)] <- weighted.mean(ys[,"Eur"], ys[,"N"], na.rm=T)		
		}
		n <- n +1
	}
	
	AVG_YSC <- as.data.frame(AVG_YSC)
	AVG_YSC[,colnames(AVG_YSC)] <- lapply(AVG_YSC[,colnames(AVG_YSC)], fmt, "123")
	AVG_YSC[is.na(AVG_YSC)] <- 0
	
	# Plots
	plt(dat=AVG_YSC, var="Eur", y_lab="Income (Euro)", m_lab=paste0("Average income of code ",k), t_lab=paste0("Average_income_Code",k))
	plt(dat=AVG_YSC, var="N", y_lab="Sample size (#)", m_lab=paste0("Sample size of code ",k), t_lab=paste0("Sample_size_Code",k))
}





########################################
#   Check individual level SCE group   #
########################################

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


sce <- as.data.frame(get(load(paste0(r_dir, sose_u1477_a.Rdata))))

# N for each code within each period
for (f in 1:2){
	sce_f <- subset(sce, vuosi %in% unlist(year[f]))
	print(paste0("Period ",f,": ",nrow(sce_f), " rows"))
	print(as.data.frame(table(sce_f$sose)))
}






