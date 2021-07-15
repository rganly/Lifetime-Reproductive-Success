## Impact of disease on lifetime childless, parity, and age at first birth 
Method descriptions: https://docs.google.com/document/d/1mOqFSQv8Y9_LxhBRvH_SOIcAgK7H76B36JrdZO7pmEk/edit


# 00_Data_preparation.R   
Aim: prepare lists of index person for different regression analyisis (e.g. population-based/sibling-design) and QCed children/marriage/endpoint registry for the entire Finnish population born in 1956-1982.

Examples of outputs:   
"indexW_5682_everyone.Rdata" for birth year 1956-1982, not imigrated/emigrated from Finland, alive until age 16, and with both father_id and mother_id available;    
"indexW_5682_fullsib.Rdata" for "indexW_5682_everyone.Rdata" with same-sex full sibs;    
"indexW_4550_everyone.Rdata" for women/men in "indexW_5682_everyone.Rdata" born in 1956-1973/1956-1968 (45/50 years of follow-up for women/men);    
"indexW_4550_fullsib.Rdata" for "indexW_4550_everyone.Rdata" with same-sex full sibs.     

"indexW_5682_everyone_child.Rdata" are children registry for "indexW_5682_everyone.Rdata";   
"indexW_5682_everyone_marriage.Rdata" are marriage registry for "indexW_5682_everyone.Rdata";    
"indexW_5682_everyone_endpoint.Rdata" are endpoint registry for "indexW_5682_everyone.Rdata".   


# 00_Define_reproductive_phenotypes.R    
Aim: Define reproductive phenotypes (id, date) for samples in "indexW_5682_everyone.Rdata", used as input for association analyses

Examples of outputs: 
"indexW_5682_everyone_childless.Rdata" for 
"indexW_5682_everyone_AgeFirstBirth.Rdata" for  
"indexW_5682_everyone_parity.Rdata" for 
"indexW_5682_everyone_childless_NoART.Rdata" for 
"indexW_5682_everyone_AgeFirstBirth_NoART.Rdata" for 
"indexW_5682_everyone_parity_NoART.Rdata" for 
"indexW_5682_everyone_childless_NoDeath28.Rdata" for 
"indexW_5682_everyone_AgeFirstBirth_NoDeath28.Rdata" for 
"indexW_5682_everyone_parity_NoDeath28.Rdata" for .


## 01_REG_childless_main.R


## 01_REG_childless_main.R


## 

##



