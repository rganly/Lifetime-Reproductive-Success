
00_Files_info
Finnish
00_Files_info.R
Aim: convert all registry data to .Rdata format and comment on each file

Input: all original files

Output: "Data_comments_FIN.csv" (See "Data_comments_FIN.xlsx", which was adapted from "Data_comments_FIN.csv")

01_Indexperson.R
Aim: Input: Output:

10_SCE_Income
Finnish
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

10_SCE_Income_Check.R
Aim: Check the income data

Swedish
