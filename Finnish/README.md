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

03_SCE_Education.R  


## 4. Demographic info for everyone in the population register
04_Pedigree.R  
Aim: pedigree (id, father, mother, sex, and birth_date) 

04_Demo.R  
Aim: Demographic info for every one appear in the population register    



## 5. ICD codes and Endpoints  
05_Endpoint_ICD_Main.R  
Aim: convert ICD codes from HILMO and DEATH registers to endpoints    
Input: "HILMO_UPDATED_SWE.lst", "ut_par_sv_27035_2018_COMPLETE.Rdata", "ut_par_ov_27035_2018.Rdata", "ut_dors_indexpers_27035_2018.Rdata", "ut_dors_b_bbarn_27035_2018.Rdata", "tove_lev_index.Rdata", "indexW_LRS.Rdata"       
Output: "huff_all.lst", "ICD_ASK_Count.tsv","SWE_ENDPOINT_Prevalence.tsv",  "HILMO_long_COMPLETE.Rdata", "DEATH_long.Rdata", "rl_COMPLETE.Rdata", "rl_index_COMPLETE.Rdata", "rl_indexW_COMPLETE.Rdata", "ry_long_COMPLETE.Rdata", "ry_long_index_COMPLETE.Rdata", "ry_long_indexW_COMPLETE.Rdata", "ry_first_COMPLETE.Rdata", "ry_first_index_COMPLETE.Rdata", "ry_first_indexW_COMPLETE.Rdata"       

05_Endpoint_ICD_Check.R  
Aim: check ICD codes from HILMO and DEATH registers: whether what we got are what we asked & whether we got all we asked?     

05_Endpoint_ICD_Plot.R  
Aim: plot prevalence of endpoints from two datasets   



## 6. Regression analysis
06_Regression_Main.R  
Aim: regression analysis    
