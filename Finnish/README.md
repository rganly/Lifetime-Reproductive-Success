# Finnish registers


## 0. atlas server and files info  
000_Tips_using_atlas.sh  
Aim: Tips to use atlas server  

00_Files_info.R  
Aim: convert all registry data to .Rdata format and comment on each file  
Input: all original files  
Output: "Data_comments_FIN.csv" (See "Data_comments_FIN.xlsx", which was adapted from "Data_comments_FIN.csv")  


## 1. Index person  
01_Indexperson.R  
Aim:  
Input:  
Output:  


## 2. Siblings of index person   
02_Sibling.R
Aim: calculate LRS (N of children and N of grandchildren) and age of having the first/last child for each sibling of index person   
Input: ""   
Output: ""  


## 3. Child, grandchild, and sibchild of index person  
03_Child_grandchild_sibchild.R  
Aim:    
Input: ""   
Output: ""  


## 4. Spouses of index person and index person's siblings  
04_Spouse.R
Aim:    
Input: ""   
Output: ""  


## 5. Pedigree   
05_Pedigree.R  
Aim:    
Input: ""   
Output: ""  


## 6. Cancer
06_Cancer_1.R   
Aim:    
Input: ""   
Output: ""  

06_Cancer_2.R. 
Aim:    
Input: ""   
Output: ""  


## 7. Medical birth registers
07_Medical_birth.R  
Aim:    
Input: ""   
Output: ""  


## 8. Malformation registers
08_Malformation.R  
Aim:    
Input: ""   
Output: ""  


## 9. Death registers
09_Death.R  
Aim:    
Input: ""   
Output: ""  


## 10. SCE and income
10_SCE_Income_Aggregate_Format.R  
Aim: Format the aggregate-level SCE info (N and income (Eur) for each occupation-sex-age-year group)  
Input: "Income_1990_1993.xlsx" and "Income_1995_2017.xlsx" prepared by Statistics Finland  
Output: "AVG_INCOME.txt" and "A_INCOME.txt"  

10_SCE_Income_Aggregate_Plot.R  
Aim: Plot average income and sample size with statistical years for the full population and for each SCE code, to check whether the definition of SCE code keep consistent across statistical years  
Input: "AVG_INCOME.txt"  
Output: "Average_income_Code**_FINLAND.tiff" and "Sample_size_Code**_FINLAND.tiff"  

10_SCE_Income_Individual.R  
Aim: create an individual-level yearly income data  
Input: "sose_u1477_a.Rdata", "demo.Rdata", and "A_INCOME.txt"  
Output: "sce_inc.Rdata"  


## 11.  
 
Aim:    
Input: ""   


## 12. Endpoint 
12_Endpoint_ICD.R  
Aim:    
Input: ""   
Output: ""  


## 13. Regression analysis
13_Regression_1_pop.R  
Aim:    
Input: ""   
Output: ""  

13_Regression_2_sib.R  
Aim:    
Input: ""   
Output: ""  

13_Regression_SummaryPlot.R  
Aim:    
Input: ""   
Output: ""  

