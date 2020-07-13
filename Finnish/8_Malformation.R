
## 8. This script is to summarize Anomalies Malformation Registry (AMR) as children and as mother


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


library(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")
library(dplyr, lib.loc="/homes/aliu/anaconda3/envs/r_env/lib/R/library/")
# system("source $HOME/.bashrc")
# system("conda install -c conda-forge r-hrbrthemes")
library(hrbrthemes, lib.loc="/homes/aliu/anaconda3/envs/r_env/lib/R/library/")
# hrbrthemes::import_roboto_condensed()
# system("conda install -c conda-forge r-viridis")
library(viridis, lib.loc="/homes/aliu/anaconda3/envs/r_env/lib/R/library/")     
# system("conda install -c conda-forge r-eulerr")
library(eulerr, lib.loc="/homes/aliu/anaconda3/envs/r_env/lib/R/library/")       

# conda install -c conda-forge r-scatterpie
library(scatterpie, lib.loc="/homes/aliu/anaconda3/lib/R/library")

fr <- function(lrs, pop){
	gen_lrs <- mean(lrs[lrs$SUKUPUOLI==2,pop])
	lrs_mbr <- lrs[lrs$KANTAHENKILON_TNRO %in% setdiff(mbr_m$TNRO, mal_m_dg$TNRO), pop]/gen_lrs   # divide the general population
	lrs_mal <- lrs[lrs$KANTAHENKILON_TNRO %in% mal_m_dg$TNRO, pop]/gen_lrs 	
	print(paste0(length(lrs_mbr)," index person with MBR and ", length(lrs_mal) ," with ABR"))
	
	dat <- data.frame(file = c("MBR","CMR"),
                  avg = c(mean(lrs_mbr), mean(lrs_mal)),
                  se = c(qt(0.975, df=length(lrs_mbr)-1)*sd(lrs_mbr)/sqrt(length(lrs_mbr)), qt(0.975, df=length(lrs_mal)-1)*sd(lrs_mal)/sqrt(length(lrs_mal)))) 

    return(dat)
}




#########################
##    Read in data      #
#########################

# Comments from Mika
#
# Data for 2017 will be ready in late 2020 and data for 2018 next year. 
# The reason for the lag is the follow-up time until the age of one year and the process to confirm all unclear cases from the hospitals, if necessary.



## 7.1.1 as children

# basic (id is unique)
mal_c_2015 <- get(load(paste0(r_dir, "anomalies_children_1987_2015.Rdata")))
mal_c_2016 <- get(load(paste0(r_dir, "anomalies_children_2016.Rdata")))
mal_c <- rbind(mal_c_2015, mal_c_2016)
save(mal_c, file=paste0(r_dir, "anomalies_children_1987_2016.Rdata"))


# dianose (each individual may have multiple records)
mal_c_dg_2015 <- get(load(paste0(r_dir, "anomalies_children_1987_2015_dg.Rdata")))
mal_c_dg_2016 <- get(load(paste0(r_dir, "anomalies_children_2016_dg.Rdata")))
mal_c_dg <- rbind(mal_c_dg_2015, mal_c_dg_2016)
save(mal_c_dg, file=paste0(r_dir, "anomalies_children_1987_2016_dg.Rdata"))



##7.1.2 as mother

# basic (id is unique)
mal_m_2015 <- get(load(paste0(r_dir, "anomalies_mothers_1987_2015.Rdata")))
mal_m_2016 <- get(load(paste0(r_dir, "anomalies_mothers_2016.Rdata")))
mal_m <- rbind(mal_m_2015, mal_m_2016)
save(mal_m, file=paste0(r_dir, "anomalies_mothers_1987_2016.Rdata"))


# dianose (each individual may have multiple records)
mal_m_dg_2015 <- get(load(paste0(r_dir, "anomalies_mothers_1987_2015_dg.Rdata")))
mal_m_dg_2016 <- get(load(paste0(r_dir, "anomalies_mothers_2016_dg.Rdata")))
mal_m_dg <- rbind(mal_m_dg_2015, mal_m_dg_2016)
save(mal_m_dg, file=paste0(r_dir, "anomalies_mothers_1987_2016_dg.Rdata"))





###################################################
#  Malformation and MBR (medical Birth Registry)  #
###################################################

# Comments from Mika
#
# RASKAUSNRO is missing from some cases in the malformation register, since RASKAUSNRO is given for all newborns (still- and livebirths), but ERNO_ID is given also for all terminations. 
# You can find the terminations from the variable manner_of_birth – codes 4 to 12. 
# Code 3 is for miscarriages, but they have been removed since these cases are not systematically been collected into the register (their number is 455 between 1987 and 2015, 0.6% of all reports).


## 7.2.1 as children

mbr_c <- get(load(paste0(r_dir, "mbr_children_1987_2018.Rdata")))
dim(mbr_c)     # 
length(intersect(mbr_c$RASKAUSNRO, mal_c$RASKAUSNRO))  # 65,380
length(setdiff(mbr_c$RASKAUSNRO, mal_c$RASKAUSNRO))    # 1796299
length(setdiff(mal_c$RASKAUSNRO, mbr_c$RASKAUSNRO))    # 1


##7.2.2 as mother

mbr_m <- get(load(paste0(r_dir, "mbr_mothers_1987_2018.Rdata")))
dim(mbr_m)     # 
length(intersect(mbr_m$RASKAUSNRO, mal_m$RASKAUSNRO))  # 66,458
length(setdiff(mbr_m$RASKAUSNRO, mal_m$RASKAUSNRO))    # 1801004
length(setdiff(mal_m$RASKAUSNRO, mbr_m$RASKAUSNRO))    # 2





##################################
#    Summarize by ICD codes      #
##################################

# 7.3.1 list of malformation category

mal_lst <- matrix(c("Q","00","07","Nervous system (Q00-Q07)",11,
                    "Q","10","18","Eye, ear, face and neck  (Q10-Q18)",10,
                    "Q","20","28","Circulatory system  (Q20-Q28)",9,
                    "Q","30","34","Respiratory system  (Q30-Q34)",8,
                    "Q","35","37","Cleft lip and cleft palate  (Q35-Q37)",7,
                    "Q","38","45","Digestive system  (Q38-Q45)",6,
                    "Q","50","56","Genital organs  (Q50-Q56)",5,
                    "Q","60","64","Urinary system  (Q60-Q64)",4,
                    "Q","65","79","Musculoskeletal system (Q65-Q79)",3,
                    "Q","80","89","Other congenital malformations (Q80-Q89)",2,
                    "Q","90","99","Chromosomal abnormalities (Q90-Q99)",1),byrow=T, ncol=5, nrow=11)
colnames(mal_lst) <- c("cat","start","end","text","o")
mal_lst



# 7.3.2 full list for each 3-position ICD codes

for (i in 1:nrow(mal_lst)){
	mal_a_lst <- matrix(NA, nrow=length(seq(mal_lst[i,"start"], mal_lst[i,"end"],1)), ncol=2)
	mal_a_lst[,1] <- paste0(mal_lst[i, "cat"], sprintf("%02d", seq(mal_lst[i,"start"], mal_lst[i,"end"],1)))    
	mal_a_lst[,2] <- mal_lst[i, "text"]
	
	if (i==1){
		mal_all_lst <- mal_a_lst  
	} else{
		mal_all_lst <- rbind(mal_a_lst, mal_all_lst)
	}	
	
}
colnames(mal_all_lst) <- c("icd10_3","text")
mal_all_lst



# 7.3.3 add 3-position ICD codes and malformation category of child

mal_m_dg$icd10_3 <- substr(mal_m_dg$ICD10,1,3)
mal_m_dg <- merge(mal_m_dg, mal_all_lst, by="icd10_3")
nrow(mal_m_dg)      # 83607 

                    
demo <- get(load(paste0(r_dir,"demo.Rdata")))
mal_m_dg <- merge(mal_m_dg, demo, by.x="TNRO", by.y="SUKULAISEN_TNRO", all.left=T) 
dim(mal_m_dg)       # 83425  
mal_m_dg[ ,"AITI_IKA"] <- mal_m_dg[ ,"YEAR_OF_BIRTH"] - as.numeric(as.character(mal_m_dg[ ,"b_year"]))  # age of pregnant





##################################
#            Plot                #
##################################

# 7.4.1 average age of being mother for each category
# without variable for termination or not

mal_age_n <- aggregate(mal_m_dg$AITI_IKA, list(mal_m_dg$text), length)        
colnames(mal_age_n) <- c("text","n")
mal_age_m <- aggregate(mal_m_dg$AITI_IKA, list(mal_m_dg$text), mean, na.rm=T) 
colnames(mal_age_m) <- c("text","m")
mal_age_nm <- merge(mal_age_n, mal_age_m, by="text")
mal_age_nm$prop <- paste0(round(100*mal_age_nm$n/sum(mal_age_nm$n),1),"%")     


mal_age_nm_o <- merge(mal_age_nm, mal_lst[,c("text","o")],by="text")
mal_age_nm_o$o <- as.numeric(as.character(mal_age_nm_o$o))
mal_age_nm_o$text <- with(mal_age_nm_o, reorder(text, o))       # order categories by ICD code


tiff("Plot_AMR_overview_1.tiff", width = 9, height = 7, units = 'in', res = 300)
sp <- ggplot(mal_age_nm_o, aes(y=text, x=m, size=n, fill=text)) +
    geom_point(alpha=0.8, shape=21, color="black") +
    scale_size(range = c(1, 30), name="N of individuals") +
    scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
    theme_ipsum() +
    xlab("Age at delivery (year)") +
    ylab("Category based on ICD10") +
    theme(legend.position = "none") 
sp + geom_text(aes(label = prop), size = 3, position = position_dodge(0.9)) + geom_vline(xintercept = 29.37, linetype="dotted", color = "yellowgreen", size=1) + theme_grey(base_size = 10,) +  theme_bw() + theme(axis.title = element_text(face="bold", size=12)) + theme(axis.text = element_text(face="bold", size=8))
dev.off()



###########
###########
# with variable for termination or not

mal_m_dg$Termination <- ifelse(mal_m_dg$MANNER_OF_BIRTH %in% c(1,2,99), "no", "yes")
mal_age_n <- aggregate(mal_m_dg$AITI_IKA, list(mal_m_dg$text,mal_m_dg$Termination), length)        
colnames(mal_age_n) <- c("text","Termination","n")
mal_age_m <- aggregate(mal_m_dg$AITI_IKA, list(mal_m_dg$text,mal_m_dg$Termination), mean, na.rm=T) 
colnames(mal_age_m) <- c("text","Termination","m")
mal_age_nm <- merge(mal_age_n, mal_age_m, by="text")
mal_age_nm <- mal_age_nm[mal_age_nm$Termination.x == mal_age_nm$Termination.y, ]
mal_age_nm$Termination <- mal_age_nm$Termination.x

mal_age_nm$prop <- paste0(round(100*mal_age_nm$n/sum(mal_age_nm$n),1),"%")     
mal_age_nm_o <- merge(mal_age_nm, mal_lst[,c("text","o")],by="text")
mal_age_nm_o$o <- as.numeric(as.character(mal_age_nm_o$o))
mal_age_nm_o$text <- with(mal_age_nm_o, reorder(text, o))       # order categories by ICD code


tiff("Plot_AMR_overview_2.tiff", width = 9, height = 7, units = 'in', res = 300)
sp <- ggplot(mal_age_nm_o, aes(y=text, x=m, size=n, fill=Termination)) +
    geom_point(alpha=0.8, shape=21, color="black") +
    scale_size(range = c(1, 25), name="N of individuals") +
    #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
    theme_ipsum() +
    xlab("Age at delivery (year)") +
    ylab("Category based on ICD10") +
    theme(legend.position = "none") 
sp + geom_text(aes(label = prop), size = 2, position = position_dodge(0.9)) + geom_vline(xintercept = 29.37, linetype="dotted", color = "yellowgreen", size=1) + theme_grey(base_size = 10,) +  theme_bw() + theme(axis.title = element_text(face="bold", size=10)) + theme(axis.text = element_text(face="bold", size=8))
dev.off()


###########
###########


               
# 7.4.2 Venn plot to show proprotion of MBR being malformation

mbr_m_event <- unique(mbr_m$RASKAUSNRO)
mal_m_event <- unique(mal_m_dg$RASKAUSNRO)
fit <- euler(c(MBR=length(setdiff(mbr_m_event,mal_m_event)), AMR=length(setdiff(mal_m_event,mbr_m_event)), "MBR&AMR"=length(intersect(mbr_m_event,mal_m_event))))

tiff("Plot_AMR_MBR_VENN.tiff", width=8, height=8, units='in', res=300)
plot(fit, fills = c("yellowgreen","pink"), fontsize=12, labels=c("Medical Birth Registry (MBR)", "Malformation"))
dev.off()




# 7.4.3 LRS for mother with malformation children compared with all mothers (both with and without malformation children)

lrs_all <- get(load(paste0(r_dir, "index_lrs_all.Rdata")))                             
dat <- fr(lrs=lrs_all, pop="n_child")         # "628156 index person with MBR and 56087 with ABR"
dat_g <- fr(lrs=lrs_all, pop="n_gchild")      # "628156 index person with MBR and 56087 with ABR"

tiff("Plot_AMR_MBR_LRS.tiff", width = 5, height = 5, units = 'in', res = 300)  
plot(c(1,2), dat$avg,
    ylim=c(0.9,2),
    xlim=c(0,3),
    pch=19, xlab="Registry", ylab="Fertility ratio",
    main="Fertility ratio for MBR and CMR",
    xaxt = "n"
)

arrows(1, dat[1,"avg"]-dat[1,"se"], 1, dat[1,"avg"]+dat[1,"se"], length=0.05, angle=90, code=3)
arrows(2, dat[2,"avg"]-dat[2,"se"], 2, dat[2,"avg"]+dat[2,"se"], length=0.05, angle=90, code=3)


axis(side=1, at=c(1,2), labels=dat$file)
abline(h=1, col="yellowgreen", lwd=3)

points(1, as.numeric(dat_g[1,"avg"]), col="blue", pch=16)
points(2, as.numeric(dat_g[2,"avg"]), col="blue", pch=16)

arrows(1, dat_g[1,"avg"] - dat_g[1,"se"], 1, dat_g[1,"avg"] + dat_g[1,"se"], length=0.05, col="blue", angle=90, code=3)
arrows(2, dat_g[2,"avg"] - dat_g[2,"se"], 2, dat_g[2,"avg"] + dat_g[2,"se"], length=0.05, col="blue", angle=90, code=3)

legend("topleft", inset=.05, title="LRS", legend=c("No of children", "Number of grandchildren"), fill=c("black", "blue"), horiz=F, cex=0.6)

dev.off()


