## Plot average income and sample size along with statistical year for the full population and for each SCE code

# Input: "AVG_INCOME.txt"
# Output: "Average_income_Code**_FINLAND.tiff" and "Sample_size_Code**_FINLAND.tiff"

# Comments: From the plot, we can check whether the definition of SCE code is consistent across statistical years



################################
#   Setup working environment  #
################################

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


# Parameters
year <- list("p1"=c(1990,1993), "p2"=c(1995, 2000, 2004:2017))
code <- list("p1"=c(10,20,31:34,41:44,51:54,59,60,70,91:93,99), "p2"=c(paste0(rep(c(10,20,31:34,41:44,51:54,60,70,81:82,99), each=2), c("-f","-m"))))
             
print(setdiff(code$p1, substr(code$p2,1,2)))   # 59 91 92 93 specific for period 1
print(setdiff(substr(code$p2,1,2), code$p1))   # "81" "82" specific for period 2


# Functions
fmt <- function(x, f="123") {        # Format columns as character or numeric 
    if(f=="123") {
    	x <- as.numeric(as.character(x))
    } else if (f=="abc"){
    	x <- as.character(x)
    }
    return(x)
}


plt <- function(dat, var, y_lab, m_lab, t_lab){   # plot aggregated income with statistical year
	y <- c(dat[ ,paste0(var,"_Male")], dat[ ,paste0(var,"_Female")])
	
	tiff(filename=paste0(t_lab,"_FINLAND.tiff"), res = 100, width = 1200, height = 800)
	plot(dat[ ,"Year"], dat[ ,paste0(var,"_Male")], ylim=c(min(y), max(y)), xlim=c(1990, 2020), type="b", col="Blue", lwd=2, main=paste0(m_lab, " for males (blue) and females (red) with statistical year"), xlab="Statistical year", ylab=y_lab)
	points(dat[ ,"Year"], dat[ ,paste0(var,"_Female")], type="b", col="red", lwd=2)
	dev.off()
}



#############################################################
#   N and Eur for two time periods (1990-1993, 1995-2017)   #
#############################################################

AVG_INCOME <- read.table("AVG_INCOME.txt", sep="/t", header=T)

# N and Average income for each statistical year
AVG_YS <- matrix(NA, nrow=length(c(year$p1, year$p2)), ncol=5)
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



#################################################
#  Plot N and Eur with years for each SCE code  #
#################################################

# sum of N and Euro by sex with statistical years
AVG_YSC <- matrix(NA, nrow=length(c(year$p1, year$p2)), ncol=6)
colnames(AVG_YSC) <- c("Year", "Code", "Eur_Male", "Eur_Female", "N_Male", "N_Female")

kod <- unique(c(code$p1, substr(code$p2,1,2)))
for (k in kod){
	print(paste0("Code ", which(kod==k), "/", length(kod)))
	n <- 1
	
	for (year in c(year$p1, year$p2)){
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

