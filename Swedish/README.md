# Swedish registers


## 0. bianca server and files info
000_Tips_using_bianca.sh  
Aim: Tips to use bianca server 


00_Files_info.R  
Aim: convert all registry data to .Rdata format and comment on each file  
Input: all original files from release 1, 2, 3 & 4  
Output: "Data_comments_SWE.csv" (See "Data_comments_SWE.xlsx", which was adapted from "Data_comments_SWE.csv")  



## 1. Index person
01_Indexperson.R  
Aim: calculate LRS (N of children and N of grandchildren) and age of having the first/last child for each index person   
Input: "tove_lev_index.Rdata", "tove_lev_koppl_index_barn.Rdata", "tove_lev_koppl_index_barnbarn.Rdata"   
Output: "indexW.Rdata", "indexW_delivery.Rdata", "index_lrs_summary", "index_lrs_count_summary"  



## 10. SCE and income
10_SCE_Income_Check.R  
Aim: Check the income data  
Input:  
Output:  








