## Format the aggregate-level SCE info (N and income (Eur) for each occupation-sex-age-year group) prepared by Statistics Finland (application TK-52-306-20)

# Input: "Income_1990_1993.xlsx" and "Income_1995_2017.xlsx"
# Output: "AVG_INCOME.txt" and "A_INCOME.txt"

# Comments: We revised three typo of the original EXCEL files
# (1) "Socioeconomic group: 54 Other workers" of the sheet "59" of the file "Income_1990_1993.xlsx" has changed to "Socioeconomic group: 59 Other workers"
# (2) "The mean income subject to state taxation" was added to row 2 of the sheet "41,f" of the file "Income_1995_2017.xlsx"    # all other sheets have this row
# (3) (1st column, 9th row) of the sheet "70, f" of the file "Income_1995_2017.xlsx" should be changed from "10" to "19"



################################
#    Set working environment   #
################################

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library("readxl")


# Parameters
files <- c("Income_1990_1993.xlsx", "Income_1995_2017.xlsx")
year <- list("p1"=c(1990,1993), "p2"=c(1995, 2000, 2004:2017))
code <- list("p1"=c(10,20,31:34,41:44,51:54,59,60,70,91:93,99), "p2"=c(paste0(rep(c(10,20,31:34,41:44,51:54,60,70,81:82,99), each=2), c("-f","-m"))))
             
print(setdiff(code$p1, substr(code$p2,1,2)))   # 59 91 92 93 specific for period 1
print(setdiff(substr(code$p2,1,2), code$p1))   # "81" "82" specific for period 2


# Functions
fmt <- function(x, f="123") {            # Format columns as character(abc) or numeric (123)
    if(f=="123") {
    	x <- as.numeric(as.character(x))
    } else if (f=="abc"){
    	x <- as.character(x)
    }
    return(x)
}

'%!in%' <- function(x,y)!('%in%'(x,y))    # not in



#####################################################
#    Merge all info from EXCEL into one text file   #
#####################################################

## Extract info (N and Eur) for each occupation-sex-age-year
AVG_INCOME <- NULL

# loop for EXCEL file
for (f in 1:length(files)){       
	print(paste0("File ", f, "/", length(files)))
	file_name <- paste0("/homes/aliu/DSGE_LRS/input/", files[f])
	
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
				s_k[ ,"Year"] <- ifelse(k %in% 1:2, year$p1[1], year$p1[2])
				s_k[ ,"Sex"]  <- ifelse(k %in% c(1,3), "Male", "Female")
			} else {
				s_k[ ,"Year"] <- year$p2[k]
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

print(sum(is.na(AVG_INCOME$Eur)))  # 1,708
print(as.data.frame(table(AVG_INCOME$Age)))

AVG_INCOME <- within(AVG_INCOME, {Age <- ifelse(Age=="-18", "0-18", ifelse(Age=="95->", "95-", Age))})   # Format the Age column
print(as.data.frame(table(AVG_INCOME$Age)))

write.table(AVG_INCOME, "AVG_INCOME.txt", append=F, quote=F, sep="\t", row.names=F, col.names=T)



###############################################
#  One occupation-sex-age-year group per row  #
###############################################

print(AVG_INCOME[AVG_INCOME$Age %!in% 0:105, "Age"])    # Ages are not numbers within 0:105

# Expand ages 0-18
ADD_18 <- NULL
for (i in c("0-18")){
	d <- AVG_INCOME[AVG_INCOME$Age==i, ]
	
	for (k in 0:18){
		d$Age <- k
		ADD_18 <- rbind(ADD_18, d)
	}
}
ADD_18 <- ADD_18[is.na(ADD_18$Sex)==F,]   # remove rows with NA
dim(ADD_18)    # 13,148


# Expand ages 25- and 95-
ADD_105 <- NULL
for (i in c("25-", "95-")){
	d <- AVG_INCOME[AVG_INCOME$Age==i, ]
	n <- ifelse(i=="25-", 25, 95)
	
	for (k in n:105){
		d$Age <- k
		ADD_105 <- rbind(ADD_105, d)
	}
}
ADD_105 <- ADD_105[is.na(ADD_105$Sex)==F,]   # remove rows with NA
dim(ADD_105)    # 2,988


# Combine and format
A_INCOME <- rbind(AVG_INCOME[AVG_INCOME$Age %in% 0:105, ], ADD_18, ADD_105)
dim(A_INCOME)   # 54,008     7
print(as.data.frame(table(A_INCOME$Age)))

A_INCOME[, c("Age","Code")] <- lapply(A_INCOME[, c("Age","Code")], fmt, "123")
write.table(A_INCOME, "A_INCOME.txt", append=F, quote=F, sep="\t", row.names=F, col.names=T)



