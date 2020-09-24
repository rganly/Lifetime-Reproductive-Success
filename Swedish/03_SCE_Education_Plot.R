## This script is to plot correlation between education levels of parents


setwd("/home/aoxing/DSGE_LRS/out/registry_edit/")
r_dir <- "/home/aoxing/DSGE_LRS/input/r_files/"

require(gridExtra)
library(tidyverse)

'%!in%' <- function(x,y)!('%in%'(x,y))


tiff("Plot_EducationLevel_parents.tiff", width = 9, height = 7, units = 'in', res = 300)
    ggplot(edu, aes(y=ISCED97.Far, x=ISCED97.Mor, size=Freq)) +
    geom_point(alpha=0.5) +
    theme_bw() +
    scale_size(range = c(1, 20), name="N of parent pairs") +
    xlab("Mother") +
    ylab("Father") +    
    ggtitle("ISCED97 level of index person's parents") + 
    theme(axis.title = element_text(face="bold", size=14)) + 
    theme(plot.title = element_text(hjust = 0.5, face="bold", size=16)) +
    theme(axis.text = element_text(face="bold", size=12)) + 
    geom_abline(intercept = 0, slope = 1, color = "#51A0D5", linetype="dashed", size=0.5)    
dev.off()    
    






