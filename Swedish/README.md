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


## 2. Spouses
02_Spouse.R  
Aim: check/combine yearly spouse registry from 1977 to 2017   
Input: "tove_lev_koppl_sys_part_{1977..2017}.Rdata", "tove_lev_koppl_sys_part_{1977..2017}.Rdata"    
Output: "index_spouse_1977_2017.Rdata", "sib_spouse_1977_2017.Rdata", Spouse_1977_2017.Rdata  


## 3. Socioeconomic status (income and education)
03_SCE_Income_Main.R   
Aim: summarize income data and extract max/mean income at age 25-35 and age 50-60   
Input: "tove_lev_dispink_{1977..2017}.Rdata", "demo.Rdata", "CPI_2017.csv"  
Output: "Income_1977_2017.Rdata", "lisa_expM.Rdata", "Income_Age2535_Age5060.Rdata" 

03_SCE_Income_Plot.R  
Aim: check raw income data and max/mean income at age 25-35 and age 50-60  
Input: "lisa_expM.Rdata", "Income_Age2535_Age5060.Rdata", "indexW.Rdata"  
Output: "SCE_Income_Raw.pdf", "SCE_Income_Normalization.pdf", "SCE_Income_Age2535_5060.pdf", "stats_by_year.csv"     

03_SCE_Education_Main.R  
Aim: summarize education registry and extract the highest education level    
Input: "tove_lev_hog_gymn_1989_1977.Rdata", "tove_lev_lisa_{1990..2017}.Rdata", "tove_lev_koppl_index_foraldrar.Rdata"     
Output: "Education_1977_2017.Rdata", "edu_high.Rdata", "index_edu.Rdata"     

03_SCE_Education_Plot.R  


## 4. Demographic info for everyone in the population register
04_Pedigree.R  
Aim: create full pedigree, including parents (id, father, mother, gender, birth_year) for children, grandchildren, sibling's children, indexperson, and sibling   
Input: "tove_lev_koppl_barn_foraldrar.Rdata", "tove_lev_koppl_index_barn.Rdata", "tove_lev_koppl_index_barnbarn.Rdata", "tove_lev_koppl_sysbarn_foraldr.Rdata", "tove_lev_koppl_index_sysbarn.Rdata", "tove_lev_koppl_index_foraldrar.Rdata", "tove_lev_index.Rdata", "sib_uniq.Rdata","tove_lev_koppl_index_syskon.Rdata"      
Output: "Pedigree.Rdata"   
Comments: "Pedigree.Rdata" only covers samples with parent info, and therefore doesn't include all samples in the population registry  


04_Demographic.R  
Aim: generate one file which includes demographic and SCE (income and education) info for everyone appear in the population registry   
Input: "tove_lev_index.Rdata", "indexW.Rdata", "tove_lev_koppl_index_barn.Rdata", "tove_lev_koppl_index_barnbarn.Rdata", "sib_uniq.Rdata", "sib_sibchild_uniq.Rdata", "tove_lev_koppl_index_foraldrar.Rdata", "tove_lev_koppl_barn_foraldrar.Rdata", "tove_lev_koppl_sysbarn_foraldr.Rdata", "Spouse_1977_2017.Rdata", "tove_lev_doddatum.Rdata", "tove_lev_migrationer.Rdata", "Pedigree.Rdata", "edu_high.Rdata", "Income_Age2535_Age5060.Rdata"    
Output: "demo.Rdata", "Demographic.Rdata", "DEMO_index.Rdata", "DEMO_child.Rdata", "DEMO_grandchild.Rdata", "DEMO_sib.Rdata", "DEMO_sibchild.Rdata", "DEMO_parent_index.Rdata", "DEMO_parent_child.Rdata", "DEMO_parent_sib.Rdata", "DEMO_spouse.Rdata"     


## 5. ICD codes and Endpoints  
05_Endpoint_ICD_Main.R  
Aim:    
Input: ""   
Output: ""  

05_Endpoint_ICD_Plot.R  
Aim:    
Input: ""   
Output: ""  


## 7. Regression analysis
07
Aim:   
Input: ""   
Output: ""   


