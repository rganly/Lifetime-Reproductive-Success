
## 6. This script is to plot summary for cancer registry (see "6_Cancer_registry.sh" for data editing process)


setwd("/homes/aliu/DSGE_LRS/output/registry_edit/")
in_dir <- "/homes/aliu/DSGE_LRS/input/"


######################
#   load library     #
######################

# install.packages("ggalluvial", lib="/homes/aliu/DSGE_LRS/software/Rlib")
library(ggalluvial, lib.loc="/homes/aliu/DSGE_LRS/software/Rlib")
llibrary(ggplot2, lib.loc="/homes/aliu/anaconda3/lib/R/library")




######################
#  data preparation  #
######################

dat <- read.table(paste0("fcr_data_freq.lst"))
dim(dat)    # 346   4
colnames(dat) <- c("N","sex","exs","ICD_10")
head(dat)


group <- read.table(paste0(in_dir,"fcr_icd_group.lst"), sep="\t", header=T)
dat_g <- merge(dat, group, by="ICD_10")


dat_gp <- aggregate(N ~ Type_abb + sex + exs, dat_g, sum)
dat_gp$sex <- ifelse(dat_gp$sex=="0","Male","Female")
dat_gp$exs <- ifelse(dat_gp$exs=="0","Alive",ifelse(dat_gp$exs=="2","Dead","Lost"))
dat_gp_nolost <- dat_gp[dat_gp$exs!="Lost",]




######################
#        Plot        #
######################

tiff("Plot_Cancer_overview.tiff", width = 10, height = 6, units = 'in', res = 300)

sp <- ggplot(as.data.frame(dat_gp_nolost), aes(y = N, axis1 = exs, axis2 = Type_abb, axis3 = sex)) +
      geom_alluvium(aes(fill = sex), width = 0, knot.pos = 0, reverse = FALSE) +
      guides(fill = FALSE) +
      geom_stratum(width = 1/8, reverse = FALSE) +
      geom_text(stat = "stratum", infer.label = TRUE, reverse = FALSE, size = 5) +
      scale_x_continuous(breaks = 1:3, labels = c("Status", "Type", "Sex")) +
      coord_flip() +
      ggtitle("Cancer Registry by Sex, Type of ICD10, and Status") +
      theme(plot.title = element_text(hjust = 0.5, face="bold", size=12))
  
sp + theme(panel.background = element_blank(), axis.title = element_text(face="bold", size=10), axis.text.x = element_blank(), axis.text.y = element_text(face="bold", size=10))

dev.off()




