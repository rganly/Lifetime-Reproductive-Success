## Summarize education registry and convert to ISCED97 and years of education


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


library(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")
library(dplyr, lib.loc="/homes/aliu/anaconda3/lib/R/library")
require(gridExtra)



'%!in%' <- function(x,y)!('%in%'(x,y))

# FR (fertility ratio for each education level (ISCED97))
fr <- function(lrs, pop, sex){
	lrs_sex <- lrs[lrs$SUKUPUOLI==sex, ]
	gen_lrs <- mean(lrs_sex[ ,pop])	
	lrs_sex[ ,"fr"] <- lrs_sex[ ,pop]/gen_lrs
	#lrs_sex[ ,"ISCED97"] <- substr(lrs_sex$kaste_t2,1,1)
	lrs_sex[ ,"ISCED97"] <- as.character(lrs_sex[ ,"ISCED97"])
	
	kaste_t2_lst <- sort(unique(lrs_sex[ ,"ISCED97"]))
	lrs_eduh_m <- matrix(NA, nrow=length(kaste_t2_lst), ncol=8)
	colnames(lrs_eduh_m) <- c("sex","ISCED97","pop","n","avg","se","lower25", "upper975")
	lrs_eduh_m[,"sex"] <- sex
	lrs_eduh_m[,"pop"] <- pop
	lrs_eduh_m[,"ISCED97"] <- kaste_t2_lst
	
	for (i in 1:length(kaste_t2_lst)){
		lrs_sex_eduh <- lrs_sex[lrs_sex$ISCED97 == kaste_t2_lst[i], "fr"]
		
		lrs_eduh_m[i ,"n"] <- length(lrs_sex_eduh)
		lrs_eduh_m[i ,"avg"] <- mean(lrs_sex_eduh)
		lrs_eduh_m[i ,"se"] <- qt(0.975, df=length(lrs_sex_eduh)-1)*sd(lrs_sex_eduh)/sqrt(length(lrs_sex_eduh))
		lrs_eduh_m[i ,"lower25"] <-  as.numeric(lrs_eduh_m[i ,"avg"]) - as.numeric(lrs_eduh_m[i ,"se"])
		lrs_eduh_m[i ,"upper975"] <- as.numeric(lrs_eduh_m[i ,"avg"]) + as.numeric(lrs_eduh_m[i ,"se"])
	}
	
    return(lrs_eduh_m)
}



# FR (fertility ratio for each education orientation)
fr_cla <- function(lrs, pop, sex){
	lrs_sex <- lrs[lrs$SUKUPUOLI==sex, ]
	gen_lrs <- mean(lrs_sex[ ,pop])	
	lrs_sex[ ,"fr"] <- lrs_sex[ ,pop]/gen_lrs
	
	kaste_t2_lst <- sort(unique(lrs_sex[ ,"educ_code"]))
	lrs_eduh_m <- matrix(NA, nrow=length(kaste_t2_lst), ncol=8)
	colnames(lrs_eduh_m) <- c("sex","educ_code","pop","n","avg","se","lower25", "upper975")
	lrs_eduh_m[,"sex"] <- sex
	lrs_eduh_m[,"pop"] <- pop
	lrs_eduh_m[,"educ_code"] <- kaste_t2_lst
	
	for (i in 1:length(kaste_t2_lst)){	
		lrs_sex_eduh <- lrs_sex[lrs_sex$educ_code == kaste_t2_lst[i], "fr"]
		
		lrs_eduh_m[i ,"n"] <- length(lrs_sex_eduh)
		lrs_eduh_m[i ,"avg"] <- mean(lrs_sex_eduh)
		lrs_eduh_m[i ,"se"] <- qt(0.975, df=length(lrs_sex_eduh)-1)*sd(lrs_sex_eduh)/sqrt(length(lrs_sex_eduh))
		lrs_eduh_m[i ,"lower25"] <-  as.numeric(lrs_eduh_m[i ,"avg"]) - as.numeric(lrs_eduh_m[i ,"se"])
		lrs_eduh_m[i ,"upper975"] <- as.numeric(lrs_eduh_m[i ,"avg"]) + as.numeric(lrs_eduh_m[i ,"se"])
		
	}

    return(lrs_eduh_m)
}






#######################################
#   2 batches of education registry   #
#######################################

## education from census files 1970-1985
# In the database of Statistics Finland  
edu_1 <- as.data.frame(get(load(paste0(r_dir, "koulutus_ala_aste_u1477_al.Rdata"))))
nrow(edu_1)      # 4,178,723
length(unique(edu_1$TNRO))    # 1,566,388, no NA


# Not in the database of Statistics Finland 
edu_1no <- get(load(paste0(r_dir, "ei_hnroa_u1477_al.Rdata")))
nrow(edu_1no)    # 242 not in the database of Statistics Finland 



#-----------------------------------------------------------------------------------------
## yearly registry of education since 1987
# In the database of Statistics Finland  
edu_2 <- as.data.frame(get(load(paste0(r_dir, "koulutus_ala_aste_u1477_a.Rdata"))))
nrow(edu_2)      # 81,604,045
length(unique(edu_2$TNRO))    # 6,770,049
length(unique(edu_2[edu_2$kaste_t2!="", "TNRO"]))  # 3,612,004


# Not in the database of Statistics Finland 
edu_2no <- get(load(paste0(r_dir, "ei_hnroa_u1477_a.Rdata")))
nrow(edu_2no)     # 473 not in the database of Statistics Finland 



#-----------------------------------------------------------------------------------------
## Combine: 1970-2018
length(intersect(edu_1$TNRO, unique(edu_2[edu_2$kaste_t2!="", "TNRO"])))   # 1,523,335 people from census files already covered by population registry
length(setdiff  (edu_1$TNRO, unique(edu_2[edu_2$kaste_t2!="", "TNRO"])))   # 43,053 

death <- as.data.frame(get(load(paste0(r_dir, "kuolemansyyt_u1477_a.Rdata"))))  
length(intersect(setdiff(edu_1$TNRO, unique(edu_2[edu_2$kaste_t2!="", "TNRO"])), death[death$kvuosi<=1987, "TNRO"] ))  # 25,930 people in edu_1 (census) but not in edu_2 were dead before 1987


edu <- rbind(edu_1, edu_2)

nrow(edu)         # 85,782,76
save(edu, file=paste0(r_dir, "ei_hnroa_u1477_1970_2018.Rdata"))






###########################################
#            ISCED97 & EduYears           #
###########################################

# highest education level for individuals with education info
edu_e <- edu[edu$kaste_t2!="", c("TNRO","kaste_t2")]
nrow(edu_e)      # 82,624,723

edu_e <- unique(edu_e)
nrow(edu_e)      # 5,558,448

edu_e <- edu_e[order(edu_e$TNRO, edu_e$kaste_t2), ]     # order by id and education level
edu_h <- by(edu_e, edu_e["TNRO"], tail, n=1)                           
edu_hy <- do.call("rbind", edu_h)                    
nrow(edu_hy)     # 3,655,057  



#-----------------------------------------------------------------------------------------
# set those without education level as 2 (according to discusssion with SF, the inviduals which are not available neither in koulutus_ala_aste_u1477* nor in ei_hnroa_u1477* are having the lowest eduation level.)
edu_na <- edu[edu$TNRO %!in% edu_hy$TNRO, c("TNRO","kaste_t2")]
edu_na$kaste_t2 <- 2  
nrow(edu_na)     # 3,114,992



#-----------------------------------------------------------------------------------------
# combine and add ISCED97 & EduYears
edu_high <- rbind(edu_hy, edu_na)
nrow(edu_high)   # 6,770,049


Edu_lst <- matrix(c(2,  "No education info registered", 2, "No education info registered", 2, "Lower secondary or second stage of basic education", 10,
                          31, "General upper secondary education", 3, "Upper secondary education", 3,"(Upper) secondary education", 13,
                          32, "Vocational upper secondary education and training", 3, "Upper secondary education", 3,"(Upper) secondary education", 13,
                          33, "Further qualification", 3, "Upper secondary education", 3,"(Upper) secondary education", 13,
                          41, "Specialist qualification", 4, "Post-secondary non-tertiary education", 4, "Post-secondary non-tertiary education", 15,
                          51, "Post-secondary non-higher vocational education", 5, "Short-cycle tertiary education", 5, "First stage of tertiary education (not leading directly to an advanced research qualification)", 19,
                          61, "Professional tertiary education", 6, "Bachelor's or equivalent level", 5,"First stage of tertiary education (not leading directly to an advanced research qualification)", 19,
                          62, "Polytechnic bachelor's degree", 6, "Bachelor's or equivalent level", 5,"First stage of tertiary education (not leading directly to an advanced research qualification)", 19,
                          63, "University bachelor degree", 6, "Bachelor's or equivalent level", 5,"First stage of tertiary education (not leading directly to an advanced research qualification)", 19,
                          71, "Higher polytechnic degree (Master)", 7, "Master's or equivalent level", 5,"First stage of tertiary education (not leading directly to an advanced research qualification)", 19,
                          72, "Higher university degree (Master)", 7, "Master's or equivalent level", 5,"First stage of tertiary education (not leading directly to an advanced research qualification)", 19,
                          73, "Professional specialisation in medicine, veterinary and dentistry", 7, "Master's or equivalent level", 5,"First stage of tertiary education (not leading directly to an advanced research qualification)", 19,
                          81, "Licentiate's degree", 8, "Doctoral or equivalent level", 5,"First stage of tertiary education (not leading directly to an advanced research qualification)", 19,
                          82, "Doctoral or equivalent level", 8, "Doctoral or equivalent level", 6, "Second stage of tertiary education (leading to an advanced research qualification, e.g. a Ph.D.)", 22), byrow=T, ncol=7, nrow=14)
                          
colnames(Edu_lst) <- c("kaste_t2","text", "edu_code", "edu_level", "ISCED97", "Definition", "EduYears")
Edu_lst <- Edu_lst[,c("kaste_t2","ISCED97","EduYears")]
Edu_lst



edu_high <- merge(edu_high, Edu_lst, by="kaste_t2")
nrow(edu_high)        # 6,770,049
edu_high <- edu_high[,c("TNRO","ISCED97","EduYears","kaste_t2")]
save(edu_high, file=paste0(r_dir,"edu_high.Rdata"))



#-----------------------------------------------------------------------------------------
# Distribution of education level with b_year for the full population
demo <- get(load(paste0(r_dir, "demo.Rdata")))
edu_high_demo <- merge(edu_high, demo, by.x="TNRO", by.y="SUKULAISEN_TNRO")
nrow(edu_high_demo)    # 6,751,930


edu_high_f <- aggregate(edu_high_demo$TNRO, list(edu_high_demo$b_year, edu_high_demo$SUKUPUOLI, edu_high_demo$ISCED97), length)
colnames(edu_high_f) <- c("b_year", "sex", "ISCED97", "count")
nrow(edu_high_f)       #  988
write.table(edu_high_f, "edu_high_f.csv", append=F, quote=F, sep="\t", row.names=F, col.names=T)



#-----------------------------------------------------------------------------------------
# lrs of each education level for index person
lrs_all <- get(load(paste0(r_dir, "index_lrs_all.Rdata")))                             
nrow(lrs_all)        # 2,365,707 

index_lrs_edu <- merge(edu_high, lrs_all, by.x="TNRO", by.y="KANTAHENKILON_TNRO")
nrow(index_lrs_edu)  # 2,365,560


for (sex_n in 1:2){
	for (pop_n in c("n_child", "n_gchild")){
		dat <- fr(index_lrs_edu, pop_n,  sex_n)
		index_lrs_edu_f <- ifelse(sex_n==1 & pop_n=="n_child", dat, rbind(index_lrs_edu_f,dat))
	}	
}

write.table(index_lrs_edu_f, "index_lrs_edu_f.csv", append=F, quote=F, sep="\t", row.names=F, col.names=T)



#-----------------------------------------------------------------------------------------
# add education level for index person's parent[, c("id", "father_id", "mother_id")] 
index_ped <- as.data.frame(get(load(paste0(r_dir,"index_ped.Rdata"))))   
nrow(index_ped)         # 2,365,834

index_ped <- index_ped[index_ped$id %!in% index_ped[duplicated(index_ped$id),"id"] ,]
nrow(index_ped)         # 2,365,584


index_edu <- merge(index_ped, edu_high[,c("TNRO","ISCED97","EduYears")], by.x="id", by.y="TNRO", all.x=T)
index_edu <- merge(index_edu, edu_high[,c("TNRO","ISCED97","EduYears")], by.x="father_id", by.y="TNRO", all.x=T)
index_edu <- merge(index_edu, edu_high[,c("TNRO","ISCED97","EduYears")], by.x="mother_id", by.y="TNRO", all.x=T)


colnames(index_edu) <- c("mother_id", "father_id", "id", "sex", "b_year", "ISCED97.Far", "EduYears.Far", "ISCED97.Mor", "EduYears.Mor", "ISCED97", "EduYears")  
index_edu[] <- lapply(index_edu, as.character)


index_edu <- within(index_edu, {
	ISCED97 <- ifelse(is.na(ISCED97), "2", ISCED97)
	ISCED97.Far <- ifelse(is.na(ISCED97.Far), "2", ISCED97.Far)
	ISCED97.Mor <- ifelse(is.na(ISCED97.Mor), "2", ISCED97.Mor)
	ISCED97.Parent <- ifelse(as.numeric(ISCED97.Far) > as.numeric(ISCED97.Mor), ISCED97.Far, ISCED97.Mor)
	
	EduYears <- ifelse(is.na(EduYears), "10", EduYears)
	EduYears.Far <- ifelse(is.na(EduYears.Far), "10", EduYears.Far)
	EduYears.Mor <- ifelse(is.na(EduYears.Mor), "10", EduYears.Mor)	
	EduYears.Parent <- ifelse(as.numeric(EduYears.Far) > as.numeric(EduYears.Mor), EduYears.Far, EduYears.Mor)
})


index_edu <- index_edu[,c("id","mother_id","father_id",
                          "ISCED97","ISCED97.Far","ISCED97.Mor","ISCED97.Parent",
                          "EduYears","EduYears.Far","EduYears.Mor","EduYears.Parent")]


save(index_edu, file=paste0(r_dir, "index_edu.Rdata"))

	
length(unique(index_ped$id))  # 2,365,584






###########################################
#         education orientation           #
###########################################

# education orientation for the highest education level for individuals with education info
edu_o <- edu[edu$iscfi2013!="", c("TNRO","iscfi2013")]
nrow(edu_o)      # 82624723

edu_o <- unique(edu_o)
nrow(edu_o)            # 5516594
length(unique(edu_o$TNRO))



#-----------------------------------------------------------------------------------------
# lrs of each education level for index person
lrs_educ_index <- merge(edu_o, lrs_all, by.x="TNRO", by.y="KANTAHENKILON_TNRO")
nrow(lrs_educ_index)   # 2922595
length(unique(lrs_educ_index$TNRO))  # 1741961


lrs_educ_index[,"b_year"] <- substr(lrs_educ_index[,"SUKULAISEN_SYNTYMAPV"],1,4)
lrs_educ_index[,"educ_code"] <- substr(lrs_educ_index[,"iscfi2013"],1,2)
lrs_educ_index <- unique(lrs_educ_index[ ,c("TNRO","educ_code","n_child", "n_gchild","SUKUPUOLI","b_year")])
nrow(lrs_educ_index)   # 2754990
length(unique(lrs_educ_index$TNRO))  # 1741961

lrs_educh_index <- merge(lrs_educ_index, edu_high_all, by="TNRO")  # 2754990
nrow(lrs_educh_index)


edu_class_lst <- matrix(c(00, "Generic programmes and qualifications (00)", 0,
                          01, "Education (01)", -1,
                          02, "Arts and humanities (02)", -2,
                          03, "Social sciences, journalism and information (03)", -3,
                          04, "Business, administration and law (04)", -4,
                          05, "Natural sciences, mathematics and statistics (05)", -5, 
                          06, "Information and Communication Technologies (ICT) (06)", -6,
                          07, "Engineering, manufacturing and construction (07)", -7,
                          08, "Agriculture, forestry, fisheries and veterinary (08)", -8,
                          09, "Health and welfare (09)", -9,
                          10, "Services (10)", -10,
                          99, "Unknown (99)", -11), byrow=T, ncol=3, nrow=12)
colnames(edu_class_lst) <- c("educ_code","text", "educ_pcode")


index_educ_lrs[ ,"educ_pcode"] <- as.numeric(ifelse(index_educ_lrs[ ,"educ_code"]==99,-11,-index_educ_lrs[ ,"educ_code"]))


for (sex_n in 1:2){

	for (pop_n in c("n_child", "n_gchild")){
	
		dat <- fr_cla(lrs_educh_index, pop_n,  sex_n)
		index_lrs_educ_f <- ifelse(sex_n==1 & pop_n=="n_child", dat, rbind(index_lrs_educ_f,dat))
		
	}
	
}

write.table(index_lrs_educ_f, "index_lrs_educ_f.csv", append=F, quote=F, sep="\t", row.names=F, col.names=T)





#######################################
#                 Plot                #
#######################################

# setwd("/Users/aoxliu/Downloads/Files/")
library(ggplot2)
library(dplyr)
# install.packages("webshot", lib="/homes/aliu/anaconda3/lib/R/library")
install.packages("streamgraph", lib="/homes/aliu/anaconda3/lib/R/library")
library(streamgraph)
library(webshot)
 
 
 
#-----------------------------------------------------------------------------------------
# Stacked area chart

edu_high_f <- read.table("edu_high_f.csv",header=T)
sex_abb <- c("male", "female")

for (sex_n in 1:2){
	tiff(paste0("Plot_ISCED97_", sex_abb[sex_n], ".tiff"), width=10, height=3.5, units='in', res=300)    
	edu_high_fm <- edu_high_f[edu_high_f$sex==sex_n & (edu_high_f$b_year>=1900 & edu_high_f$b_year<=2018), ]
	streamgraph(edu_high_fm, key="ISCED97", value="count", date="b_year", height="300px", width="1000px") 
	dev.off()
}




#-----------------------------------------------------------------------------------------
# hihgest education level achieved and LRS


# "Plot_eduh_male_LRS.tiff"   # 867, 691

for (sex_n in 1:2){
	index_eduh_lrs_mc <- index_eduh_lrs[index_eduh_lrs$pop=="n_child" & index_eduh_lrs$sex==1, ]
	index_eduh_lrs_mg <- index_eduh_lrs[index_eduh_lrs$pop=="n_gchild" & index_eduh_lrs$sex==1, ]

	plot(index_eduh_lrs_mc$avg, index_eduh_lrs_mc$edu_code, 
    	xlim=c(0,2),
    	xlab="Fertility ratio", ylab="Highest education level achieved", 
    	main=paste0("Fertility ratio for males with different highest education level"),
    	pch=19, type="b")

	abline(v=1, col="yellowgreen", lwd=3)
	arrows(x0=index_eduh_lrs_mc$lower25, x1=index_eduh_lrs_mc$upper975, y0=index_eduh_lrs_mc$edu_code, length=0.05, angle=90, code=3)

	points(index_eduh_lrs_mg$avg, index_eduh_lrs_mg$edu_code, col="blue", pch=16, type="p")
	arrows(x0=index_eduh_lrs_mg$lower25, x1=index_eduh_lrs_mg$upper975, y0=index_eduh_lrs_mg$edu_code, length=0.05, angle=90, code=3, col="blue")
	legend("topright", inset=.05, title="LRS", legend=c("No. of children", "No. of grandchildren"), fill=c("black", "blue"), horiz=F, cex=0.6)
}



#-----------------------------------------------------------------------------------------
# hihgest education level achieved and LRS

# "Plot_educ_male_LRS.tiff"   # 1108, 800

for (sex_n in 1:2){
	index_educ_lrs_mc <- index_educ_lrs[index_educ_lrs$pop=="n_child" & index_educ_lrs$sex==sex_n, ]
	index_educ_lrs_mg <- index_educ_lrs[index_educ_lrs$pop=="n_gchild" & index_educ_lrs$sex==sex_n, ]

	plot(x=index_educ_lrs_mc$avg, y=index_educ_lrs_mc$educ_pcode, 
		xlim=c(0,2),
		xlab="Fertility ratio", ylab="", 
		main=paste0("Fertility ratio for males with different education classification"),
		pch=19, type="b", yaxt="n")

	axis(side=2, at=edu_class_lst[,"educ_pcode"], labels=edu_class_lst[,"text"],las=2,cex=0.5)
	abline(v=1, col="yellowgreen", lwd=3)
	arrows(x0=index_educ_lrs_mc$lower25, x1=index_educ_lrs_mc$upper975, y0=index_educ_lrs_mc$educ_pcode, length=0.05, angle=90, code=3)

	points(index_educ_lrs_mg$avg, index_educ_lrs_mg$educ_pcode, col="blue", pch=16, type="p")
	arrows(x0=index_educ_lrs_mg$lower25, x1=index_educ_lrs_mg$upper975, y0=index_educ_lrs_mg$educ_pcode, length=0.05, angle=90, code=3, col="blue")
	legend("bottomright", inset=.05, title="LRS", legend=c("No. of children", "No. of grandchildren"), fill=c("black", "blue"), horiz=F, cex=0.6)
}




