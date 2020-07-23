
## 7. This script is to summarize Medical Birth Register (MBR) as children and as mother



setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


library(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")
require(gridExtra)
# system("source $HOME/.bashrc")
# system("conda install -c r r-igraph")
# install.packages("treemap")
library(treemap, lib.loc="/homes/aliu/anaconda3/envs/r_env/lib/R/library/")



########################
#     Read in data     #
########################

# Mother
mbr_m <- get(load(paste0(r_dir, "mbr_mothers_1987_2018.Rdata")))
dim(mbr_m)           # 1867462     137
event_m <- mbr_m$RASKAUSNRO 
length(event_m)      # 1867462



# Children  
mbr_c <- get(load(paste0(r_dir, "mbr_children_1987_2018.Rdata")))
dim(mbr_c)           # 1867462     137
event_c <- mbr_c$RASKAUSNRO 
length(event_c)      # 1861679



length(intersect(event_c,event_m))    # 1,854,621 events available for both as child and as mother
length(setdiff(event_c,event_m))      # 7,058 event only appear for as child, but missing for as mother 
length(setdiff(event_m,event_c))      # 12,841 event only appear for as mother, but missing for as child 




################################
#  variables relevent to pain  #
################################

mbr_pain <- mbr_m[,c("EPIDURAALI", "SPINAALI", "SPINAALI_EPIDUR", "PARASERVIKAALI", "PUDENDAALI", "ILOKAASU", "MUULAAKLIEV", "MUULIEVITYS")]


pain <- matrix(NA, ncol=5, nrow=8)
colnames(pain) <- c("N=0","N=1","N","P(N=0)","P(N=1)")
rownames(pain) <- colnames(mbr_pain)


for (i in 1:8){
	pain[i,"N=0"] <- sum(mbr_pain[,i]==0)
	pain[i,"N=1"] <- sum(mbr_pain[,i]==1)
	pain[i,"N"] <- pain[i,"N=0"] + pain[i,"N=1"]
	pain[i,"P(N=0)"] <- round(100*pain[i,"N=0"]/pain[i,"N"],1)
	pain[i,"P(N=1)"] <- round(100*pain[i,"N=1"]/pain[i,"N"],1)
	print(i)
}
pain


mbr_pain[] <- lapply(mbr_pain, function(x) {if(is.character(x)) as.numeric(as.character(x)) else x})
sapply(mbr_pain, class)
pain_c <- rowSums(mbr_pain)
pain_count <- as.data.frame(table(pain_c))
pain_count$Percent <- round(100*pain_count$Freq/sum(pain_count$Freq),1)
pain_count




###########################################
#  weight and length of child and mother  #
###########################################

plot1 <- ggplot(mbr_m, aes(x=APAINO, y= SYNTYMAPAINO) ) +
         labs(x = "Pre-pregnancy weight of the mother (kg)") +
         labs(y = "Birth weight of child (g)") +
         geom_hex(bins = 70) +
         scale_fill_continuous(type = "viridis") +
         theme_bw()

plot2 <- ggplot(mbr_m, aes(x=APITUUS, y= SYNTYMAPITUUS) ) +
         labs(x = "Length of the mother (cm)") +
         labs(y = "Birth length of child (cm)") +
         geom_hex(bins = 70) +
         scale_fill_continuous(type = "viridis") +
         theme_bw()
  
plot3 <- ggplot(mbr_m, aes(x=APAINO, y= APITUUS) ) +
         labs(x = "Pre-pregnancy weight of the mother (kg)") +
         labs(y = "Length of the mother (cm)") +
         geom_hex(bins = 70) +
         scale_fill_continuous(type = "viridis") +
        theme_bw()

plot4 <- ggplot(mbr_m, aes(x= SYNTYMAPITUUS, y=SYNTYMAPAINO) ) +
         labs(x = "Birth length of child (cm)") +
         labs(y = "Birth weight of child (g)") +
         geom_hex(bins = 70) +
         scale_fill_continuous(type = "viridis") +
         theme_bw()

#grid.arrange(plot1, plot2, plot3, plot4, ncol=2)


tiff("Plot_MBR_weight4.tiff", width = 5, height = 4, units = 'in', res = 300)
plot4
dev.off()

tiff("Plot_MBR_weight1.tiff", width = 5, height = 4, units = 'in', res = 300)
plot1
dev.off()



###################################
#  treemap for mode of delivery   #
###################################

dat <- as.data.frame(table(mbr_m[,"SYNNYTYSTAPATUNNUS"]))
colnames(dat) <- c("category","count")

dat[1,"count"]
dat$cate <- c("Unknown","Spontaneous vaginal delivery","Vaginal breech delivery","Forceps","Vacuum extractor","Planned CS","Urgent CS","Emergency CS","Other CS","Unknown")
dat$group <- c("Unknown","Vaginal","Vaginal","Vaginal","Vaginal","Caesarean section","Caesarean section","Caesarean section","Caesarean section","Unknown")

dat$cc <- c("Unknown"," ","Vaginal breech","Forceps or Vacuum extractor","Forceps or Vacuum extractor","Planned CS","Urgent CS","Emergency CS","Other CS","Unknown")
dat$gg<- c(" ","Spontaneous vaginal delivery", " ", " ", " ", " ", " ", " ", " ", " ")



tiff("Plot_MBR_delivery.tiff", width = 4, height = 4, units = 'in', res = 300)
treemap(dat[dat$category %in%  seq(1,8,1),], index=c("gg","cc"), vSize="count", type="index", 
        fontcolor.labels=c("white","white"), bg.labels=c("transparent"), palette = "Set1", title="Mode of delivery", fontsize.title=12, fontface.labels=c(2,2), fontsize.labels=c(18,10))
dev.off()

round(100*dat[dat$gg=="Spontaneous vaginal delivery","count"]/sum(dat$count),1)  # 76.5

# 1 Spontaneous vaginal delivery
# 2 Vaginal breech delivery (since 1996)
# 3 Forceps
# 4 Vacuum extractor
# 5 Planned CS (since 1991)
# 6 Urgent CS (since 2004, some hospitals since Autumn 2005)
# 7 Emergency CS (since 2004, some hospitals since Autumn 2005)
# 8 Other CS
# 9 Unknown




