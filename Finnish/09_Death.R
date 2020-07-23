## 7.  Death registry from 1971 and list of death across all years


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


library(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")
library(viridis, lib.loc="/homes/aliu/anaconda3/envs/r_env/lib/R/library/")
library(hrbrthemes, lib.loc="/homes/aliu/anaconda3/envs/r_env/lib/R/library/")


'%!in%' <- function(x,y)!('%in%'(x,y))


fr <- function(lrs, pop, sex){
	lrs_sex <- lrs[lrs$SUKUPUOLI==sex, ]
	gen_lrs <- mean(lrs_sex[ ,pop])
	lrs_alive <- lrs_sex[lrs_sex$KANTAHENKILON_TNRO %!in% death$TNRO, pop]/gen_lrs   
	lrs_dead <- lrs_sex[lrs_sex$KANTAHENKILON_TNRO %in% death$TNRO, pop]/gen_lrs 	
	print(paste0(length(lrs_alive)," index person are alive and ", length(lrs_dead) ," are dead before 2019"))
	
	dat <- data.frame(file = c("Alive","Dead"),
                  avg = c(mean(lrs_alive), mean(lrs_dead)),
                  se = c(qt(0.975, df=length(lrs_alive)-1)*sd(lrs_alive)/sqrt(length(lrs_alive)), qt(0.975, df=length(lrs_dead)-1)*sd(lrs_dead)/sqrt(length(lrs_dead)))) 

    return(dat)
}

	
#############################################
#  Death registry include all dead people?  #
#############################################

# death registry
death <- get(load(paste0(r_dir, "kuolemansyyt_u1477_a.Rdata")))  
nrow(death)        # 901,081 


# people with death date from population registry
demo <- get(load(paste0(r_dir,"demo.Rdata")))
nrow(demo)         # 6,752,171
demo <- as.data.frame(demo)


# overlap between death registry and people with death data in population registry
dd_lst <- unlist(demo[is.na(demo$SUKULAISEN_KUOLINPV)==F,"SUKULAISEN_TNRO"])
length(dd_lst)     # 959,619
length(intersect(dd_lst, death$TNRO))    # 899,315
length(setdiff(dd_lst, death$TNRO))      # 60,304 not appear in death registry
length(setdiff(death$TNRO, dd_lst))      # 1,766 probably only in marriage registry


demo[is.na(demo[,"SUKULAISEN_KUOLINPV"]),"SUKULAISEN_KUOLINPV"] <- "20181232"
demo$age <- as.numeric(substr(demo$SUKULAISEN_KUOLINPV,1,4)) - as.numeric(substr(demo$SUKULAISEN_SYNTYMAPV,1,4))

demo$status <- "Alive"
demo[demo$SUKULAISEN_TNRO %in% death$TNRO, "status"] <- "Death registry"
demo[demo$status == "Alive" & demo$SUKULAISEN_KUOLINPV!="20181232", "status"] <- "Dead but not in death registry"
table(demo$status)



###########################################
#  Hist of status in 2018 for birth_year  #
###########################################

demo_p <- demo[demo$b_year >= 1900 & demo$b_year <= 2018, ]
demo_p$b_year <- as.numeric(demo_p$b_year)

tiff("Plot_Death_status.tiff", width = 8, height = 6, units = 'in', res = 300)  

ggplot(demo_p, aes(b_year, fill = status)) +
    geom_histogram(binwidth = 1) +
    ggtitle("Current status for people born from 1990 to 2018") + 
    xlab("Birth year") +
    ylab("N") +
    scale_fill_manual(values=c("yellowgreen","blue","pink")) +
    theme_bw() + 
    theme(axis.title = element_text(face="bold", size=14)) + 
    scale_x_continuous(breaks = seq(1900, 2020, 10)) +
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=16)) +
    labs(fill = "Status in year 2018") +
    theme(legend.position="top")
 dev.off()
 
table(demo[demo$b_year >= 1900 & demo$b_year <= 2018, "status"])




#################################
#  Cause of death and dead age  #
#################################

c_lst <- matrix(c("01","03", "Tartunta- ja loistauteja",  "Certain infectious and parasitic diseases (A00-B99)", 15,
                      "04","22", "Kasvaimet", "Neoplasms (C00-D48)", 14,
                      "23","24", "Umpieritys-, ravitsemus- ja aineenvaihduntasairaudet", "Endocrine, nutritional and metabolic diseases (E00-E90)", 13, 
                      "25","25", "Dementia, Alzheimerin tauti", "Dementia, Alzheimer's disease (F01, F03, G30, R54)", 12, 
                      "26","26", "Muut hermoston ja aistimien sairaudet pl. alkoholiperäiset", "Other diseases of the nervous system and senses (G00-H95)", 11, 
                      "27","30", "Verenkiertoelinten sairaudet pl. alkoholiperäiset", "Diseases of the circulatory system (I00-I99)", 10, 
                      "31","35", "Hengityselinten sairaudet", "Diseases of the respiratory system (J00-J99)", 9, 
                      "36","36", "Ruuansulatuselinten sairaudet pl. alkoholiperäiset", "Diseases of the digestive system (K00-K93)",8, 
                      "37","37", "Virtsa- ja sukupuolielinten sairaudet","Diseases of the genitourinary system (N00-N99)", 7, 
                      "38","38", "Synnynnäiset epämuodostumat ja kromosomipoikkeavuudet", "Congenital malformations, deformations and chromosomal abnormalities (Q00-Q99)", 6, 
                      "39","39", "Muut sairaudet pl. alkoholiperäiset", "Other diseases", 5, 
                      "40","40", "Tuntemattomat ja epätäydellisesti määritetyt kuolemansyyt","Ill-defined and unknown causes of mortality (R96-R99)",4, 
                      "41","41", "Alkoholiperäiset taudit ja tapaturmainen alkoholimyrkytys", "Alcohol-related diseases and accidental alcohol poisoning",3, 
                      "42","53", "Tapaturmat ja väkivalta pl. tapaturmainen alkoholimyrkytys", "External causes of morbidity and mortality (V01-Y89)",2, 
                      "54","54", "Ei kuolintodistusta", "No death certificate (R999)", 1), byrow=T, ncol=5, nrow=15)

colnames(c_lst) <- c("start","end","text_FIN","text","o")


for (i in 1:nrow(c_lst)){
	cau_lst <- matrix(NA, nrow=length(seq(c_lst[i,"start"], c_lst[i,"end"],1)), ncol=3)
	cau_lst[,1] <- sprintf("%02d", seq(c_lst[i,"start"], c_lst[i,"end"],1)) 
	cau_lst[,2] <- c_lst[i, "text"]	
	cau_lst[,3] <- c_lst[i, "o"]
	
	if (i==1){
		cause_lst <- cau_lst  
	} else{
		cause_lst <- rbind(cause_lst, cau_lst)
	}	
}
colnames(cause_lst) <- c("tpksaika","text","o")
cause_lst



death_sex <- merge(death, demo[,c("SUKULAISEN_TNRO","SUKUPUOLI","age")], by.x="TNRO", by.y="SUKULAISEN_TNRO")
dim(death)        # 901081     10
dim(death_sex)    # 899339     11


cause_f_text <- merge(death_sex, cause_lst, by="tpksaika")
cause_f_count <- aggregate(cause_f_text$TNRO, list(cause_f_text$text, cause_f_text$SUKUPUOLI), length)
cause_f_age <- aggregate(cause_f_text$age, list(cause_f_text$text,cause_f_text$SUKUPUOLI), mean, na.rm=T)
cause_f_summ <- cbind(cause_f_count,cause_f_age)[,c(1,2,3,6)]
colnames(cause_f_summ) <- c("text", "sex", "count", "age")

cause_fs <- merge(cause_f_summ, c_lst[,c("text","o")], by="text")
cause_fs$oo <- cause_fs[ ,"sex"] * as.numeric(as.character(cause_fs[ ,"o"]))
cause_fs$text <- with(cause_fs, reorder(text, oo))
cause_fs$sex_p <- ifelse(cause_fs$sex==1, "male", "female")


tiff("Plot_Death_cause.tiff", width = 12, height = 6, units = 'in', res = 300)  
sp <- ggplot() +
      geom_point(alpha=0.8, aes(x = age, y = text, size=count, colour = 'Male'), data=cause_fs[cause_fs$sex==1,]) +
      geom_point(alpha=0.8, aes(x = age, y = text, size=count, colour = 'Female'), data=cause_fs[cause_fs$sex==2,]) +
      xlab(label = 'Age (year)') +
      ylab(label = 'Rate') +
      scale_size(range = c(1, 15), name="N of individuals") 
sp + geom_vline(xintercept = mean(death_sex$age,na.rm=T) , linetype="dotted", color = "yellowgreen", size=1) + theme_grey(base_size = 12) + theme_bw() + theme(axis.title = element_text(face="bold", size=12), axis.text = element_text(face="bold", size=8)) + labs(color='Sex') 
dev.off()

aggregate(cause_fs$count, list(cause_fs$sex), sum)    # 551607 with 1 and 331568 with 2



######################################################
#   LRS for people still alive or dead before 2019   #
######################################################

lrs_all <- get(load(paste0(r_dir, "index_lrs_all.Rdata")))                             
sex_abb <- c("male", "female")


for (sex_n in 1:2){
	dat <- fr(lrs=lrs_all, sex=sex_n, pop="n_child")         # "628156 index person with MBR and 56087 with ABR"
	dat_g <- fr(lrs=lrs_all, sex=sex_n, pop="n_gchild")      # "628156 index person with MBR and 56087 with ABR"

tiff(paste0("Plot_alive_dead_LRS_",sex_abb[sex_n],".tiff"), width = 5, height = 5, units = 'in', res = 300)  
plot(c(1,2), dat$avg,
    ylim=c(0.5,2),
    xlim=c(0,3),
    pch=19, xlab="Status", ylab="Fertility ratio",
    main=paste0("Fertility ratio for ",sex_abb[sex_n],"s still alive or dead before 2019"),
    xaxt = "n",
    cex.axis=0.6,
    cex.lab=0.6,
    cex.main=0.7
    
)

arrows(1, dat[1,"avg"]-dat[1,"se"], 1, dat[1,"avg"]+dat[1,"se"], length=0.05, angle=90, code=3)
arrows(2, dat[2,"avg"]-dat[2,"se"], 2, dat[2,"avg"]+dat[2,"se"], length=0.05, angle=90, code=3)


axis(side=1, at=c(1,2), labels=dat$file)
abline(h=1, col="yellowgreen", lwd=3)

points(1, as.numeric(dat_g[1,"avg"]), col="blue", pch=16)
points(2, as.numeric(dat_g[2,"avg"]), col="blue", pch=16)

arrows(1, dat_g[1,"avg"] - dat_g[1,"se"], 1, dat_g[1,"avg"] + dat_g[1,"se"], length=0.05, col="blue", angle=90, code=3)
arrows(2, dat_g[2,"avg"] - dat_g[2,"se"], 2, dat_g[2,"avg"] + dat_g[2,"se"], length=0.05, col="blue", angle=90, code=3)

legend("topleft", inset=.05, title="LRS", legend=c("No. of children", "No. of grandchildren"), fill=c("black", "blue"), horiz=F, cex=0.6)
   
dev.off()

}



