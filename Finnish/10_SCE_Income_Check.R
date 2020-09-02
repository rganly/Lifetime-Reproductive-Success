# The main body of this script was written by Andrea Ganna

library(tidyverse)
library(ggplot2)
library(viridis)

r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"


sce_inc <- get(load(paste0(r_dir, "sce_inc.Rdata")))
edu_high <- get(load(paste0(r_dir, "edu_high.Rdata")))


## Some summary information

# N. of rows
nrow(sce_inc)   # 88,008,334

# N. of individuals
length(unique(sce_inc$TNRO))  # 6,699,435

# Classification included
distinct(sce_inc, sose, Text)

# Statistical years
unique(sce_inc$vuosi)  

# Check year of birth for those with no information - does not explain why for some SES is missing
mean(sce_inc[sce_inc$sose=="","b_year"])  # 1969.635

# Cell with highest income in statistical year 2017
min(sce_inc[sce_inc$vuosi==2017,"Eur"],na.rm=T)     # 0
max(sce_inc[sce_inc$vuosi==2017,"Eur"],na.rm=T)     # 1,306,871   
median(sce_inc[sce_inc$vuosi==2017,"Eur"],na.rm=T)  # 23,834.22
mean(sce_inc[sce_inc$vuosi==2017,"Eur"],na.rm=T)    # 25238.07

# Occyupation with highest median income
sce_inc %>% group_by(Text) %>% summarise(mean_Eur=median(Eur,na.rm=T)) %>% print(n=100)

# Median education level for each profession
sce_inc_sorted <- sce_inc %>% arrange(TNRO,-Eur) %>% distinct(TNRO, .keep_all=T)

dm <- merge(sce_inc_sorted, edu_high, by="TNRO")

dm %>% group_by(Text) %>% summarise(mean_edu=mean(as.numeric(as.character(EduYears)),na.rm=T)) %>% print(n=100)

