## Impute missing income from education and experience using "Mincer regression" 



##############################################################################
#       Email from Lombardi Stefano (VATT) <stefano.lombardi@vatt.fi>        #
##############################################################################

# FYI, I have run the "Mincer regression" version of what I mentioned in the previous email (with a slight variation): 
# for each year in 1995-2015:
# - keep if age is in [16, 65]
# - replace income = 0 if income == . (might want to drop this and condition on having positive earnings, though this would lead to a positively selected sample)
# - keep if educ >= 3 (which for me corresponds to drop if educ == missing instead of relassifying this as a 0; might want to check what happens when lifting this later)
# - experience = age - year_educ - 6
# - deflate income in 2015 (later can change this to 2017 if needed, but qualitatively results will be the same)
# - do OLS:  income = c + years_educ + experience + experience^2

# Note that a person can enter the sample multiple times across years. I think this is needed in order to compare results to the grouped data (where people enter the group average multiple times across years).
# The results are striking in terms of R2: ~30% (this is in line with other Mincer regressions in the literature).
# The numbers for the returns to education are not far from what I wrote you, just slightly smaller. 


# (in which year was income reported for Finland? I have raw data, so I have “manually” deflated using 2015).
# What I did was to look for each year between 1995 and 2015, and I regressed earnings on years of schooling using the mapping that Aoxing sent me in the last email 
# (I also simply described earnings in the population).
# To summarize the numbers, I kept only the row for which educ >= 3 (out of 3,4,5,6,missing), and age between 16 and 60.

 
# As expected, looking across all years, education is an extremely strong predictor of earnings.
# Taking for instance 2007, the model with education alone has an R2 ~ 14.85%,
# one additional year of education is associated with an increase of earnings of EUR 2,630 (p < .0001),
# Average earnings in the population of reference are ~ EUR 29,777 (median = 27,509).
# The returns to (one additional year of) education for males are higher (EUR 3,484 vs. EUR 2184 for females) if you want the check by group.

# In general, not sure you are interested, but if you want something slightly more refined than this, it would be preferable to run actual “Mincer regressions” of the following type:
# income_(age30-35) = c + educ_years + exp + exp^2
# where income_(age30-35) == average individual income between age 30-35,
# educ_years = maximum education achieved by age 30
# exp (== potential labor market experience) = age - educ_years – 6 = 30 – educ_years – 6

 


 
################################################
#    Run "Mincer regression" in Finnish data   #
################################################

# setwd("/homes/aliu/DSGE_LRS/output/registry_edit/REGRESSION/")
# r_dir <- "/homes/aliu/DSGE_LRS/input/r_files/"
# scp aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/input/r_files/edu_high.Rdata   ~/Documents/Project1_Lifetime_Reproductive_Success/Data/FIN/SCE/
# scp aliu@ssh.fimm.fi:/homes/aliu/DSGE_LRS/input/r_files/sce_inc.Rdata   ~/Documents/Project1_Lifetime_Reproductive_Success/Data/FIN/SCE/

setwd("~/Documents/Project1_Lifetime_Reproductive_Success/Data/FIN/SCE/")
r_dir <- "~/Documents/Project1_Lifetime_Reproductive_Success/Data/FIN/SCE/"



library(tidyverse)
library(dplyr)


##  Load data        
edu <- data.frame(get(load(paste0(r_dir, "edu_high.Rdata"))))
dim(edu)  # 6,770,049       4
head(edu)
#     TNRO ISCED97 EduYears kaste_t2
#1 2719566       2       10        2
#2 2257948       2       10        2


inc <- data.frame(get(load(paste0(r_dir, "sce_inc.Rdata"))))
dim(inc)  # 88,008,334       10
head(inc)



inc <- inc %>% left_join(edu, by="TNRO")
dim(inc)   # 88,008,334       13


## income for age in [18, 65] ----------------------------------------------
inc <- inc %>% mutate(EduYears=as.numeric(as.character(EduYears)), experience=Age-EduYears-6, experience2=experience^2) %>% filter(Age>=19 & Age<=65) 
nrow(inc)  # 55,922,941
length(unique(inc$TNRO))  # 4,713,490

table(inc$EduYears)
#         10       13       15       19       22 
#   10645271 23620127   660176 20426673   570694 

table(inc$ISCED97)
#          2        3        4        5        6 
#   10645271 23620127   660176 20426673   570694 

median(inc$Eur, na.rm=T)  # 28442.69
mean(inc$Eur, na.rm=T)    # 30356.23
sd(inc$Eur, na.rm=T)      # 17629.64



Mincer_Age1865_Edu <- lm(Eur~EduYears, data=inc)
#Call:
#lm(formula = Eur ~ EduYears, data = inc)
#
#Residuals:
#   Min     1Q Median     3Q    Max 
#-37673 -10625  -1742   7891 256484 
#
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.139e+04  9.675e+00    1177   <2e-16 ***
#EduYears    1.287e+03  6.381e-01    2017   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 17020 on 55922921 degrees of freedom
#  (18 observations deleted due to missingness)
#Multiple R-squared:  0.0678,	Adjusted R-squared:  0.0678 
#F-statistic: 4.067e+06 on 1 and 55922921 DF,  p-value: < 2.2e-16



Mincer_Age1865_Exp <- lm(Eur~experience +experience2, data=inc)
#   Call:
#   lm(formula = Eur ~ experience + experience2, data = inc)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -37433 -10052  -1888   6334 259954 
#   
#   Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  1.580e+04  4.771e+00    3311   <2e-16 ***
#   experience   1.670e+03  4.940e-01    3381   <2e-16 ***
#   experience2 -3.201e+01  1.095e-02   -2923   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 15950 on 55922920 degrees of freedom
#     (18 observations deleted due to missingness)
#   Multiple R-squared:  0.1812,	Adjusted R-squared:  0.1812 
#   F-statistic: 6.187e+06 on 2 and 55922920 DF,  p-value: < 2.2e-16



Mincer_Age1865 <- lm(Eur~EduYears +experience +experience2, data=inc)
#   Call:
#   lm(formula = Eur ~ EduYears + experience + experience2, data = inc)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -47663  -8681   -654   6343 262287 
#   
#   Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.532e+04  1.076e+01   -1423   <2e-16 ***
#   EduYears     1.896e+03  5.988e-01    3167   <2e-16 ***
#   experience   1.741e+03  4.554e-01    3822   <2e-16 ***
#   experience2 -2.935e+01  1.012e-02   -2901   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 14690 on 55922919 degrees of freedom
#     (18 observations deleted due to missingness)
#   Multiple R-squared:  0.3057,	Adjusted R-squared:  0.3057 
#   F-statistic: 8.206e+06 on 3 and 55922919 DF,  p-value: < 2.2e-16



Mincer_Age1865_Sex <- lm(Eur~EduYears +experience +experience2 +SUKUPUOLI, data=inc)
#   Call:
#   lm(formula = Eur ~ EduYears + experience + experience2 + SUKUPUOLI, 
#       data = inc)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -54150  -7311   -240   6017 257127 
#   
#   Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -3.043e+03  1.114e+01  -273.1   <2e-16 ***
#   EduYears     2.075e+03  5.682e-01  3651.2   <2e-16 ***
#   experience   1.699e+03  4.294e-01  3957.5   <2e-16 ***
#   experience2 -2.809e+01  9.545e-03 -2942.3   <2e-16 ***
#   SUKUPUOLI   -9.921e+03  3.731e+00 -2658.8   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 13840 on 55922918 degrees of freedom
#     (18 observations deleted due to missingness)
#   Multiple R-squared:  0.3836,	Adjusted R-squared:  0.3836 
#   F-statistic: 8.7e+06 on 4 and 55922918 DF,  p-value: < 2.2e-16



inc_Male <- inc %>% filter(SUKUPUOLI==1)
nrow(inc_Male)  # 2,8143,942
Mincer_Age1865_Male <- lm(Eur~EduYears +experience +experience2, data=inc_Male)
#   Call:
#   lm(formula = Eur ~ EduYears + experience + experience2, data = inc_Male)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -59103  -8739   -408   7063 261277 
#   
#   Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -2.439e+04  1.691e+01   -1442   <2e-16 ***
#   EduYears     2.574e+03  9.602e-01    2681   <2e-16 ***
#   experience   2.151e+03  7.583e-01    2836   <2e-16 ***
#   experience2 -3.634e+01  1.672e-02   -2173   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 16750 on 28143929 degrees of freedom
#     (9 observations deleted due to missingness)
#   Multiple R-squared:  0.3496,	Adjusted R-squared:  0.3496 
#   F-statistic: 5.042e+06 on 3 and 28143929 DF,  p-value: < 2.2e-16


inc_Female <- inc %>% filter(SUKUPUOLI==2)
nrow(inc_Female)  # 27,778,999
Mincer_Age1865_Female <- lm(Eur~EduYears +experience +experience2, data=inc_Female)
#   Call:
#   lm(formula = Eur ~ EduYears + experience + experience2, data = inc_Female)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -34555  -5478   -340   5375  91442 
#   
#   Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.108e+04  1.022e+01   -1084   <2e-16 ***
#   EduYears     1.550e+03  5.594e-01    2771   <2e-16 ***
#   experience   1.306e+03  4.035e-01    3238   <2e-16 ***
#   experience2 -2.137e+01  9.051e-03   -2361   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 9473 on 27778986 degrees of freedom
#     (9 observations deleted due to missingness)
#   Multiple R-squared:  0.3977,	Adjusted R-squared:  0.3977 
#   F-statistic: 6.115e+06 on 3 and 27778986 DF,  p-value: < 2.2e-16






## income_Age2535 ----------------------------------------------

SEX <- inc %>% select(TNRO,SUKUPUOLI) %>% distinct()

inc_Age2535 <- inc %>% filter(Age>=25 & Age<35) %>% select(TNRO,Eur) %>% group_by(TNRO) %>% 
	                   summarize(income_Age2535_mean=mean(Eur,na.rm=T), N_year_2535=sum(is.na(Eur)==F)) %>% filter(N_year_2535!=0) %>% 
                       left_join(edu, by="TNRO") %>% mutate(EduYears=as.numeric(as.character(EduYears)), experience=30-EduYears-6, experience2=experience^2) 
inc_Age2535 <- inc_Age2535 %>% left_join(SEX, by="TNRO")
nrow(inc_Age2535)  # 2,593,849



Mincer_Age2535mean <- lm(income_Age2535_mean~EduYears +experience +experience2, data=inc_Age2535)
#   Call:
#   lm(formula = income_Age2535_mean ~ EduYears + experience + experience2, 
#       data = inc_Age2535)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -22152  -5702   -672   5132 113743 
#   
#   Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 19017.4318   225.2923   84.41   <2e-16 ***
#   EduYears      463.1228    11.1079   41.69   <2e-16 ***
#   experience          NA         NA      NA       NA    
#   experience2    -9.2851     0.6349  -14.62   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 8467 on 2593846 degrees of freedom
#   Multiple R-squared:  0.06129,	Adjusted R-squared:  0.06129 
#   F-statistic: 8.468e+04 on 2 and 2593846 DF,  p-value: < 2.2e-16



Mincer_Age2535mean_Sex <- lm(income_Age2535_mean~EduYears +experience +experience2 +SUKUPUOLI, data=inc_Age2535)
#   Call:
#   lm(formula = income_Age2535_mean ~ EduYears + experience + experience2 + 
#       SUKUPUOLI, data = inc_Age2535)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -26690  -4301    213   4238 110899 
#   
#   Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 30620.9972   198.8763  153.97   <2e-16 ***
#   EduYears      538.9307     9.7836   55.09   <2e-16 ***
#   experience          NA         NA      NA       NA    
#   experience2   -16.3201     0.5592  -29.18   <2e-16 ***
#   SUKUPUOLI   -8138.5576     9.3976 -866.03   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 7457 on 2593845 degrees of freedom
#   Multiple R-squared:  0.2718,	Adjusted R-squared:  0.2718 
#   F-statistic: 3.228e+05 on 3 and 2593845 DF,  p-value: < 2.2e-16



inc_Age2535_Male <- inc_Age2535 %>% filter(SUKUPUOLI==1)
nrow(inc_Age2535_Male)  # 1,324,281
Mincer_Age2535mean_Male <- lm(income_Age2535_mean~EduYears +experience +experience2, data=inc_Age2535_Male)
#   Call:
#   lm(formula = income_Age2535_mean ~ EduYears + experience + experience2, 
#       data = inc_Age2535_Male)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -28189  -5646    529   5491 111513 
#   
#   Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 16981.0540   313.1604  54.225   <2e-16 ***
#   EduYears      855.5374    15.5042  55.181   <2e-16 ***
#   experience          NA         NA      NA       NA    
#   experience2    -7.5372     0.8742  -8.622   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 8934 on 1324278 degrees of freedom
#   Multiple R-squared:  0.1244,	Adjusted R-squared:  0.1244 
#   F-statistic: 9.405e+04 on 2 and 1324278 DF,  p-value: < 2.2e-16



inc_Age2535_Female <- inc_Age2535 %>% filter(SUKUPUOLI==2)
nrow(inc_Age2535_Female)  # 1,269,568
Mincer_Age2535mean_Female <- lm(income_Age2535_mean~EduYears +experience +experience2, data=inc_Age2535_Female)
#   Call:
#   lm(formula = income_Age2535_mean ~ EduYears + experience + experience2, 
#       data = inc_Age2535_Female)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -18154  -3457    -50   3604  33515 
#   
#   Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 16063.7876   225.6052   71.20   <2e-16 ***
#   EduYears      416.4655    11.0841   37.57   <2e-16 ***
#   experience          NA         NA      NA       NA    
#   experience2   -13.6537     0.6429  -21.24   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 5450 on 1269565 degrees of freedom
#   Multiple R-squared:  0.1431,	Adjusted R-squared:  0.1431 
#   F-statistic: 1.06e+05 on 2 and 1269565 DF,  p-value: < 2.2e-16




## income_Age5060 ----------------------------------------------

inc_Age5060 <- inc %>% filter(Age>=50 & Age<60) %>% select(TNRO,Eur) %>% group_by(TNRO) %>% 
	                   summarize(income_Age5060_mean=mean(Eur,na.rm=T), N_year_5060=sum(is.na(Eur)==F)) %>% filter(N_year_5060!=0) %>% 
                       left_join(edu, by="TNRO") %>% mutate(EduYears=as.numeric(as.character(EduYears)), experience=55-EduYears-6, experience2=experience^2) 
inc_Age5060 <- inc_Age5060 %>% left_join(SEX, by="TNRO")
nrow(inc_Age5060)  # 2,320,465



Mincer_Age5060mean <- lm(income_Age5060_mean~EduYears +experience +experience2, data=inc_Age5060)
#   Call:
#   lm(formula = income_Age5060_mean ~ EduYears + experience + experience2, 
#       data = inc_Age5060)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -45267  -9795  -1842   5891 129058 
#   
#   Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -2.020e+05  2.401e+03  -84.15   <2e-16 ***
#   EduYears     8.658e+03  7.481e+01  115.74   <2e-16 ***
#   experience          NA         NA      NA       NA    
#   experience2  9.321e+01  1.093e+00   85.27   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 15440 on 2320462 degrees of freedom
#   Multiple R-squared:  0.2287,	Adjusted R-squared:  0.2287 
#   F-statistic: 3.44e+05 on 2 and 2320462 DF,  p-value: < 2.2e-16



Mincer_Age5060mean_Sex <- lm(income_Age5060_mean~EduYears +experience +experience2 +SUKUPUOLI, data=inc_Age5060)
#   Call:
#   lm(formula = income_Age5060_mean ~ EduYears + experience + experience2 + 
#       SUKUPUOLI, data = inc_Age5060)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -48912  -7931  -1373   6588 123648 
#   
#   Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -187105.10    2240.09  -83.53   <2e-16 ***
#   EduYears       8741.17      69.80  125.24   <2e-16 ***
#   experience          NA         NA      NA       NA    
#   experience2      93.72       1.02   91.89   <2e-16 ***
#   SUKUPUOLI    -11117.13      18.92 -587.50   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 14400 on 2320461 degrees of freedom
#   Multiple R-squared:  0.3286,	Adjusted R-squared:  0.3286 
#   F-statistic: 3.785e+05 on 3 and 2320461 DF,  p-value: < 2.2e-16



inc_Age5060_Male <- inc_Age5060 %>% filter(SUKUPUOLI==1)
nrow(inc_Age5060_Male)  # 1,152,667
Mincer_Age5060mean_Male <- lm(income_Age5060_mean~EduYears +experience +experience2, data=inc_Age5060_Male)
#   Call:
#   lm(formula = income_Age5060_mean ~ EduYears + experience + experience2, 
#       data = inc_Age5060_Male)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -55856 -10342  -1962   6942 125158 
#   
#   Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -3.472e+05  3.783e+03  -91.77   <2e-16 ***
#   EduYears     1.370e+04  1.179e+02  116.17   <2e-16 ***
#   experience          NA         NA      NA       NA    
#   experience2  1.581e+02  1.723e+00   91.78   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 17550 on 1152664 degrees of freedom
#   Multiple R-squared:  0.2664,	Adjusted R-squared:  0.2664 
#   F-statistic: 2.092e+05 on 2 and 1152664 DF,  p-value: < 2.2e-16



inc_Age5060_Female <- inc_Age5060 %>% filter(SUKUPUOLI==2)
nrow(inc_Age5060_Female)  # 1,152,667
Mincer_Age5060mean_Female <- lm(income_Age5060_mean~EduYears +experience +experience2, data=inc_Age5060_Female)
#   Call:
#   lm(formula = income_Age5060_mean ~ EduYears + experience + experience2, 
#       data = inc_Age5060_Female)
#   
#   Residuals:
#      Min     1Q Median     3Q    Max 
#   -33308  -6552  -1184   6187  74450 
#   
#   Coefficients: (1 not defined because of singularities)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -47179.427   2243.635  -21.03   <2e-16 ***
#   EduYears      3394.475     69.910   48.55   <2e-16 ***
#   experience          NA         NA      NA       NA    
#   experience2     23.234      1.022   22.75   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   
#   Residual standard error: 9995 on 1167795 degrees of freedom
#   Multiple R-squared:  0.3078,	Adjusted R-squared:  0.3078 
#   F-statistic: 2.597e+05 on 2 and 1167795 DF,  p-value: < 2.2e-16




