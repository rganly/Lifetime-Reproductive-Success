# Swedish registers


## 0. bianca server and files info
00_Tips_using_bianca.sh  
Aim: Tips to use bianca server 

00_Files_info.R  
Aim: convert all registry data to .Rdata format and comment on each file  
Input: all original files from release 1, 2, 3 & 4  
Output: "Data_comments_SWE.csv" (See "Data_comments_SWE.xlsx", which was adapted from "Data_comments_SWE.csv")  


## 1. Indexperson/Siblings and their LRS
01_Indexperson_LRS.R  
Aim: calculate LRS (N of children and N of grandchildren), childless, and age of having the first/last child for each index person   
Input: "tove_lev_index.Rdata", "tove_lev_koppl_index_barn.Rdata", "tove_lev_koppl_index_barnbarn.Rdata"   
Output: "indexW.Rdata", "indexW_delivery.Rdata", "indexW_LRS.Rdata", "indexW_lrs_summary", "indexW_lrs_count_summary", "indexW_age_at_having_child_count", "indexW_age_at_having_child_summary"  

01_Sibling_LRS.R  
Aim: calculate LRS (N of children), childless, and age of having the first/last child for each sibling   
Input: "tove_lev_koppl_index_syskon.Rdata", "tove_lev_koppl_index_sysbarn.Rdata"     
Output: "sib_uniq.Rdata", "sib_sibchild_uniq.Rdata", "Sib_delivery.Rdata", "sib_lrs_summary"  


## 2. Spouses and children/grandchildren of Indexperson/Siblings 
02_Spouse.R  
Aim: check/combine yearly spouse registry from 1977 to 2017   
Input: "tove_lev_koppl_sys_part_{1977..2017}.Rdata", "tove_lev_koppl_sys_part_{1977..2017}.Rdata"    
Output: "index_spouse_1977_2017.RData", "sib_spouse_1977_2017.RData"    


## 3. Socioeconomic status (income and education)
03_SCE_Income.R  
Aim: Check the income data  
Input:  
Output:  

03_SCE_Education.R  
Aim:  
Input:  
Output:  


## 4. Demographic info for everyone in the population register
04_Pedigree.R  
Aim: pedigree (id, father, mother, sex, and birth_date)  
Input: ""   
Output: ""   

04_Demo.R  
Aim: Demographic info for every one appear in the population register    
Input:  
Output:  


## 5. Health registers
05
Aim:   
Input: ""   
Output: ""   


## 6. ICD codes and Endpoints  
06_Endpoint_ICD.R  
Aim:    
Input: ""   
Output: ""  


## 7. Regression analysis
07
Aim:   
Input: ""   
Output: ""   


