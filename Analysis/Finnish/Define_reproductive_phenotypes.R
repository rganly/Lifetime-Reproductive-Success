## Define reproductive phenotypes, in order to use the same definitions and QC rules across diferent analyses
# Data dictionary: https://docs.google.com/spreadsheets/d/16E40mY9Avffnq6u5DhtQbisohG3ctzmU/edit#gid=947231392

setwd("/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/")
r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"

library(dplyr)
'%!in%' <- function(x,y)!('%in%'(x,y))



################################################
#              read in data                    # 
################################################

indexW_fullsib <- data.frame(get(load(paste0(r_dir, "indexW_fullsib.Rdata"))))
indexW_fullsib_child <- data.frame(get(load(paste0(r_dir, "indexW_fullsib_child.Rdata"))))
indexW_fullsib_endpoint <- data.frame(get(load(paste0(r_dir, "indexW_fullsib_endpoint.Rdata"))))
indexW_fullsib_marriage <- data.frame(get(load(paste0(r_dir, "indexW_fullsib_marriage.Rdata"))))




################################################
#                 childless                    # 
################################################

# childless --------------------
define_childless <- indexW_fullsib %>% mutate(childless=ifelse(id %in% indexW_fullsib_child$id, 0, 1))
table(define_childless$childless)   # 201,103 with 0 and 649,017 with 1


# childless, excluding children dead before 28 days --------------------
indexW_fullsib_child %>% filter(child_death_before28days==0) %>% select(id) %>% unique() %>% mutate(childless_NoDeathBefore28days=0) %>% 
        right_join(indexW_fullsib, by="id") %>% mutate(childless_NoDeathBefore28days=ifelse(is.na(childless_NoDeathBefore28days),1,0)) %>% 
        group_by(childless_NoDeathBefore28days) %>% count()   # 201,251 with 1 and 648,869 with 0


# childless, excluding children using ART (???how to deal with is.na(child_ART)), which are children not registered in medical birth registry) --------------------
indexW_fullsib_child %>% filter(child_ART==0 | is.na(child_ART)) %>% select(id) %>% unique() %>% mutate(childless_NoART=0) %>% 
       right_join(indexW_fullsib, by="id") %>% mutate(childless_NoART=ifelse(is.na(childless_NoART),1,0)) %>% 
       group_by(childless_NoART) %>% count()   # 206,656 with 1 and 643,464 with 0


# childless, excluding children using ART or dead before 28 days --------------------
indexW_fullsib_child %>% filter(child_death_before28days==0 & (child_ART==0 | is.na(child_ART))) %>% select(id) %>% unique() %>% mutate(childless_NoDeathBefore28daysNoART=0) %>% 
       right_join(indexW_fullsib, by="id") %>% mutate(childless_NoDeathBefore28daysNoART=ifelse(is.na(childless_NoDeathBefore28daysNoART),1,0)) %>% 
       group_by(childless_NoDeathBefore28daysNoART) %>% count()   # 206,801 with 1 and 643,319 with 0




################################################
#  parity (only for individuals with children) # 
################################################

# parity --------------------
indexW_fullsib_child %>% group_by(id) %>% count() %>% rename(parity="n") %>% 
       group_by(parity) %>% count()  # from 1 to 19


# parity, excluding children dead before 28 days --------------------
indexW_fullsib_child %>% filter(child_death_before28days==0) %>% group_by(id) %>% count() %>% rename(parity_NoDeathBefore28days="n") %>% 
       group_by(parity_NoDeathBefore28days) %>% count()  # from 1 to 19


# parity, excluding children dead before 28 days using ART (???how to deal with is.na(child_ART)), which are children not registered in medical birth registry) --------------------


# parity, excluding children using ART or dead before 28 days --------------------




################################################
#  number of children (including 0 children)   # 
################################################

# number of children --------------------


# number of children, excluding children dead before 28 days --------------------


# number of children, excluding children dead before 28 days using ART (???how to deal with is.na(child_ART)), which are children not registered in medical birth registry) --------------------


# number of children, excluding children using ART or dead before 28 days --------------------




################################################
#            Age at first birth                #
################################################



################################################
#             Finding a spouse                 #
################################################



################################################
#             Preterm births                   #
################################################



################################################
#         Congenital anomalies                 #
################################################




