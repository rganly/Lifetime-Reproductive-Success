## Add Eur (average income by occupation-sex-age-year) and N to the individual-level SCE data 

# Input: "sose_u1477_a.Rdata", "demo.Rdata", and "A_INCOME.txt", 
# Output: "sce_inc.Rdata"
# Comments: create an individual-level yearly income data



################################
#   Setup working environment  #
################################

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library("readxl")
library(dplyr)


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



##################################################
#   Add Eur and N to individual-level SCE data   #
##################################################

# Individual level occupation data
sce <- as.data.frame(get(load(paste0(r_dir, "sose_u1477_a.Rdata"))))
dim(sce)           # 88220702        4


# N for each occupation code within each period
for (f in 1:2){
	sce_f <- subset(sce, vuosi %in% unlist(year[f]))
	print(paste0("Period ",f,": ",nrow(sce_f), " rows"))
	print(as.data.frame(table(sce_f$sose)))
}


# Add sex and birth year to individual-level occupation data 
demo <- get(load(paste0(paste0(r_dir, "demo.Rdata"))))
demo <- demo[,c("SUKULAISEN_TNRO","SUKUPUOLI","b_year")]
nrow(demo)          # 6752171     

sce_demo <- merge(sce, demo, by.x="TNRO", by.y="SUKULAISEN_TNRO")
nrow(sce_demo)      # 88008334      
rm(demo, sce)

sce_demo <- within(sce_demo, {
    b_year <- fmt(b_year, "123")
    Age <- vuosi - b_year
})


# Add Eur and N to individual-level occupation data 
A_INCOME <- read.table("A_INCOME.txt", sep="\t", header=T)
nrow(A_INCOME)    # 54008    

A_INCOME <- within(A_INCOME, {
    Sex <- ifelse(Sex=="Male", 1, 2)
    Age <- fmt(Age, "123")
    Code <- fmt(Code, "abc")
})

sce_inc <- left_join(sce_demo, A_INCOME, by = c("vuosi"="Year", "sose"="Code", "SUKUPUOLI"="Sex", "Age"="Age")) 
nrow(sce_inc)   # 88008334

save(sce_inc, file=paste0(r_dir, "sce_inc.Rdata"))


