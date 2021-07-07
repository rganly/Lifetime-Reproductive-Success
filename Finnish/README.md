# Finnish registers


## 0. Atlas server and file info  
000_Tips_using_atlas.sh  
Aim: Tips to use Atlas server  

00_Files_info.R  
Aim: convert all registry data to .Rdata format and comment on each file  
Input: all original files  
Output: "Data_comments_FIN.csv" (See "Data_comments_FIN.xlsx", which was adapted from "Data_comments_FIN.csv")  


## 1. Indexperson/Siblings and their LRS
01_Indexperson_LRS.R  
Aim: calculate LRS (N of children and N of grandchildren), childless, and age of having the first/last child for each index person   

01_Sibling_LRS.R  
Aim: calculate LRS (N of children), childless, and age of having the first/last child for each sibling   
  


## 2. Spouses
02_Spouse.R  
Aim: check/combine yearly spouse registry from 1977 to 2017   
Input: "tove_lev_koppl_sys_part_{1977..2017}.Rdata", "tove_lev_koppl_sys_part_{1977..2017}.Rdata"    
Output: "index_spouse_1977_2017.RData", "sib_spouse_1977_2017.RData"    


## 3. Socioeconomic status (income and education)
03_SCE_Income_Aggregate_Format.R  
Aim: Format the aggregate-level SCE info (N and income (Eur) for each occupation-sex-age-year group)  
Input: "Income_1990_1993.xlsx" and "Income_1995_2017.xlsx" prepared by Statistics Finland  
Output: "AVG_INCOME.txt" and "A_INCOME.txt"  

03_SCE_Income_Aggregate_Plot.R  
Aim: Plot average income and sample size with statistical years for the full population and for each SCE code, to check whether the definition of SCE code keep consistent across statistical years  
Input: "AVG_INCOME.txt"  
Output: "Average_income_Code**_FINLAND.tiff" and "Sample_size_Code**_FINLAND.tiff"  

03_SCE_Income_Individual.R  
Aim: create an individual-level yearly income data  
Input: "sose_u1477_a.Rdata", "demo.Rdata", and "A_INCOME.txt"  
Output: "sce_inc.Rdata"  

03_SCE_Impute.R  
Aim: 

03_SCE_Education.R  
Aim: summarize education registry and convert to ISCED97 and years of education


## 4. Demographic info for everyone in the population register
04_Pedigree.R  
Aim: pedigree (id, father, mother, sex, and birth_date), which is an efficient way to store all familial relationships. 

04_Demo.R  
Aim: Demographic info for every one appear in the population register    



## 5. ICD codes and Endpoints  
Aim: Convert from ICD codes to endpoints using HILMO, CANCER, and DEATH registry
Input: "thl2019_804_poisto_6986_COMPLETE", "thl2019_804_poisto_8793_COMPLETE", "thl2019_804_hilmo_9495_COMPLETE", "THL2019_804_hilmo_COMPLETE", "kuolemansyyt_u1477_a.Rdata", "fcr.Rdata", and "HILMO_UPDATED_FIN.lst"
Output: "HILMO_ICD_LONG_COMPLETE.Rdata", "CANCER_long.Rdata", "DEATH_long.Rdata",



## 13. Regression analysis
13_Data_preparation_QC.R  
Aim: 

### childless --------------------
13_REG_childless_SEX_GEE_EVERYONE_SIBS.R     
Aim: GEE model for childless in the full population, using disease diagnoses before age 45/50 

13_REG_childless_SEX_LOGIT_SIB4550.R   
Aim: conditional logitic regression model for childless in sibling pairs disconcordant on outcomes, using disease diagnoses before age 45/50

13_REG_childless_SEX_LOGIT_SIBMATCH_SIBS.R   
Aim: conditional logitic regression model for childless in sibling pairs disconcordant on outcomes, using a sibling-match design

13_REG_childless_SEX_COXVARY_SIBMATCH_SIBS.R   
Aim: cox HR model stratified by full-sibling families for childless in sibling pairs disconcordant on outcomes, using a sibling-match design and considering disease status as time-varing covariable



### parity --------------------

13_REG_MODEL_TRAIT_SEX.R    
Aim: Regression analysis for disease status using everyone (for glm and gee) or all sibs (for cond model), including sick and unsick

13_REG_PLOT.R   
Aim: (Interactive) Plot for effects of all disease endpoints for fertility phenotype in a specific sex


