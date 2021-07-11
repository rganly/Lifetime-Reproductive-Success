
## Interpreting model estimates: marginal effects (average predicted probability) for different regression models (from the scale of estimation to the scale of interest)
# Marginal effects is a way of presenting results as differences in probabilities, which is more informative than odds ratios and relative risks.

library(dplyr)


############################################
#         logistic regression              #
############################################

## Estimation: effect on the log-odds (log(p/(1-p))) of the outcome . Target: probability scale -------------------------
# log-odds is not meaningful except for the sign. log-odds could be transfered into odds, which is often misinterpreted (not relative risks/probabilities).
# Example: with no covariates, the probability of death in a control group is 0.40, and the probability of death in the treatment group is 0.20. The odds-ratio is (0.2/(1-0.2))/(0.4/(1-0.4))
#          The treatment reduces the odds of death by a factor of 0.375. Or, the probability of death in the treatment group are 2.67 higher in the control group (1/0.375).
# odds ratio is not relative risk. The relative risk is 0.20/0.40=0.5. The probablity of death is reduced by hald in the treatment group.
# with odds ratios and relative risks, we don't have a sense of the magnitude. Same example but now the probability od death in the control group is 0.0004 and 0.0002 in the treatment group. The odds ratio is still 0.375 and the relative risk is still 0.5, but the magnitudes are of course quite different.
# In the probability scale, all effects are non-linear because conditional on covariate values, the probability must be bounded between 0 and 1.


## logit regression for binary co-variate op ----------------
# for binary factor variable, Marginal effects = difference in probablity of op=0 and op=1 (see https://github.com/cran/mfx/blob/master/R/logitmfx.R)
library(foreign)
mydata <- read.dta("https://dss.princeton.edu/training/Panel101.dta") 
mydata$op <- as.factor(mydata$op)
logit <- glm(y_bin ~ op + x2 + x3, family=binomial(link="logit"), data=mydata) 
summary(logit)
#             Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   1.9901     0.6420   3.100  0.00194 **
# op1          -1.4672     0.7159  -2.049  0.04043 * 
# x2            0.1463     0.2681   0.546  0.58532   
# x3            0.5656     0.4346   1.301  0.19312   



## probablity by hand ----------------
invlogit <- function (x) {1/(1+exp(-x))}

# probability for op==0
invlogit(coef(logit)[1] + coef(logit)[2]*0 + coef(logit)[3]*mean(mydata$x2) + coef(logit)[4]*mean(mydata$x3))  # 0.9198688 

# probability for op==1
invlogit(coef(logit)[1] + coef(logit)[2]*1 + coef(logit)[3]*mean(mydata$x2) + coef(logit)[4]*mean(mydata$x3))  # 0.7257903 
dif_prob_op <- 0.7257903 - 0.9198688   # -0.1940785



## probablity by predict ----------------
allmean <- data.frame(intercept=1, op=c(0,1), x2=rep(mean(mydata$x2),2), x3=rep(mean(mydata$x3),2))
allmean$op <- as.factor(allmean$op)

allmean_1 <- data.frame(op=1, x2=mean(mydata$x2), x3=mean(mydata$x3))
allmean_1$op <- factor(allmean_1$op, levels=c(0,1))

prob <- predict(logit, newdata=allmean, type="response")  # "response" for probability, 0.9198688, 0.7257903 
prob[2] - prob[1]   # -0.1940785 

lp <- predict(logit, newdata=allmean, type="link")  # "link" for linear predictor, 2.440566 0.973368 
invlogit(lp[2]) - invlogit(lp[1])   # -0.1940785 

# plogis(t(be) %*% disx1) - plogis(t(be) %*% disx0)

allmean$op <- as.numeric(as.character(allmean$op))
gr <- as.matrix(invlogit(lp[2]) * allmean[1,] - invlogit(lp[1]) * allmean[2,]) # standard errors
se <- sqrt(gr %*% vcv %*% t(gr))  # 0.6635942


## SE of marginal effects ----------------
xm <- t(data.frame(intercept=1, op=mean(as.numeric(as.character(mydata$op))), x2=mean(mydata$x2), x3=mean(mydata$x3)))
be <- as.matrix(na.omit(coef(logit))) 
vcv <- vcov(logit)

disx0 <- disx1 <- xm
disx0["op",] <- 0
disx1["op",] <- 1
mfx[disch[i],1] <- plogis(t(be) %*% disx1) - plogis(t(be) %*% disx0)  # -0.1940785

gr <- dlogis(t(be) %*% disx1) %*% t(disx1) - dlogis(t(be) %*% disx0) %*% t(disx0)
sqrt(gr %*% vcv %*% t(gr))   # 0.08955073



# package mfx ----------------
# install.packages("mfx") 
library(mfx)
logitmfx(y_bin ~ op+x2+x3, data=mydata)
# Marginal Effects:
#         dF/dx Std. Err.       z   P>|z|  
# op1 -0.194078  0.089551 -2.1672 0.03022 *
# x2   0.019010  0.034614  0.5492 0.58286  
# x3   0.073516  0.051051  1.4401 0.14985  



# package margins (not working) ----------------
# install.packages("margins") 
library("margins")
logit <- glm(y_bin ~ factor(op) + x2 + x3, family=binomial(link="logit"), data=mydata) 
(m <- margins(logit))  # why not right
#      x2      x3     op1
# 0.02082 0.08052 -0.2034




 
############################################
#          poisson regression              #
############################################

mydata$ct <- as.numeric(as.factor(mydata$opinion))
mydata$op <- as.factor(mydata$op)
poi <- glm(ct ~ op + x2 + x3, family="poisson", data=mydata) 
summary(poi)
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.27648    0.11001  11.603  < 2e-16 ***
# op1         -0.90108    0.17049  -5.285 1.25e-07 ***
# x2          -0.01318    0.06466  -0.204    0.839    
# x3          -0.03481    0.06006  -0.580    0.562    



## probablity by hand ----------------
invlogit <- function (x) {1/(1+exp(-x))}

# probability for op==0
exp(coef(poi)[1] + coef(poi)[2]*0 + coef(poi)[3]*mean(mydata$x2) + coef(poi)[4]*mean(mydata$x3))  # 3.484053 

# probability for op==1
exp(coef(poi)[1] + coef(poi)[2]*1 + coef(poi)[3]*mean(mydata$x2) + coef(poi)[4]*mean(mydata$x3))  # 1.414987 
dif_prob_op <- 1.414987  - 3.484053   # -2.069066



## probablity by predict (the same as logit) ----------------
prob <- predict(poi, newdata=allmean, type="response")  # "response" for probability, 3.484053 1.414987
prob[2] - prob[1]   # -2.069066 

lp <- predict(poi, newdata=allmean, type="link")  # "link" for linear predictor, 1.2481963 0.3471206
exp(lp[2]) - exp(lp[1])   # -2.069066  (typically log link)


## SE of marginal effects ----------------
xm <- t(data.frame(intercept=1, op=mean(as.numeric(as.character(mydata$op))), x2=mean(mydata$x2), x3=mean(mydata$x3)))
be <- as.matrix(na.omit(coef(poi))) 
vcv <- vcov(poi)

disx0 <- disx1 <- xm
disx0["op",] <- 0
disx1["op",] <- 1
exp(t(be) %*% disx1) - exp(t(be) %*% disx0)  # -0.1940785
gr <- exp(t(be) %*% disx1) %*% t(disx1) - exp(t(be) %*% disx0) %*% t(disx0)
sqrt(gr %*% vcv %*% t(gr))   # 0.3799423



# package mfx ----------------
# install.packages("margins") 
# library("margins")
library("mfx")
poissonmfx(ct ~ op+x2+x3, data=mydata)
# Marginal Effects:
#         dF/dx Std. Err.       z     P>|z|    
# op1 -2.069066  0.379942 -5.4457 5.159e-08 ***
# x2  -0.029258  0.143548 -0.2038    0.8385    
# x3  -0.077287  0.133272 -0.5799    0.5620    




############################################
#          coxph regression (not working)  #
############################################

library(foreign)
mydata <- read.dta("https://dss.princeton.edu/training/Panel101.dta") 
mydata$op <- as.factor(mydata$op)
library(survival)
cox <- coxph(Surv(rep(1, 70L), y_bin) ~ op + x2 + x3, data=mydata) 
summary(cox)
#         coef exp(coef) se(coef)      z Pr(>|z|)  
# op1 -0.52536   0.59134  0.28014 -1.875   0.0607 .
# x2   0.04929   1.05052  0.11399  0.432   0.6655  
# x3   0.10082   1.10608  0.09955  1.013   0.3112  


## probablity by hand ----------------
invlogit <- function (x) {1/(1+exp(-x))}

# probability for op==0
invlogit(coef(cox)["op1"]*0 + coef(cox)["x2"]*mean(mydata$x2) + coef(cox)["x3"]*mean(mydata$x3))  # 0.52084

# probability for op==1
invlogit(coef(cox)["op1"]*1 + coef(cox)["x2"]*mean(mydata$x2) + coef(cox)["x3"]*mean(mydata$x3))  # 0.3912751 
dif_prob_op <- 0.3912751 - 0.52084    # -0.1295649


## probablity by hand ('arg' should be one of “lp”, “risk”, “expected”, “terms”, “survival”) ----------------
allmean <- data.frame(op=c(0,1), x2=rep(mean(mydata$x2),2), x3=rep(mean(mydata$x3),2))
allmean$op <- as.factor(allmean$op)

lp <- predict(cox, newdata=allmean, type="lp")       # 0.0000000 -0.5253636 
risk <- predict(cox, newdata=allmean, type="risk")   # 1.0000000 0.5913403, risk=exp(lp)
# predict(cox, newdata=allmean, type="expected")   # doesn't work since it needs to provide y_bin
invlogit(lp[2]) - invlogit(lp[1])   # -0.1284011


lp <- predict(cox, newdata=allmean, type="lp", reference="zero")   # 0.08340822 -0.44195535 (reference="zero" is the same value as calculated by hand (average op as reference), otherwise op=0 as reference)
invlogit(0.08340822)   # 0.52084
invlogit(-0.44195535)  # 0.3912751
invlogit(lp[2]) - invlogit(lp[1])   # -0.1284011



## try the same data with poisson regression
poi_bi <- glm(y_bin ~ op + x2 + x3, family="poisson", data=mydata) 
summary(poi_bi)
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.14192    0.21445  -0.662    0.508
# op1         -0.26202    0.27644  -0.948    0.343
# x2           0.02063    0.11372   0.181    0.856
# x3           0.04602    0.10038   0.458    0.647

library(mfx)
poissonmfx(y_bin ~ op+x2+x3, data=mydata)
# Marginal Effects:
#         dF/dx Std. Err.       z  P>|z|
# op1 -0.207712  0.218596 -0.9502 0.3420
# x2   0.016310  0.089867  0.1815 0.8560
# x3   0.036376  0.079234  0.4591 0.6462




###############################################
#    conditional logit regression (clogit )   #
###############################################

## clogit model, and then calculate probability by hands

dat <- data.frame(set=rep(1:50,each=3), status=rep(c(1,0,0),50), x1=rep(c(0,1,0,1,0),30), x2=rnorm(150,7,1.5))
dat$x1 <- as.factor(dat$x1)

fit <- clogit(status~x1+x2+strata(set),dat)
#          coef exp(coef)  se(coef)      z Pr(>|z|)
# x11 -0.003114  0.996891  0.300120 -0.010    0.992
# x2   0.029704  1.030150  0.120426  0.247    0.805


dat.test <- data.frame(set=rep(1:30,each=2), x1=rep(c(0,1),30), x2=mean(dat$x2))
dat.test$x1 <- as.factor(dat.test$x1)

# reference="sample"
dat.test$predict_lp <- predict(fit, newdata=dat.test, type='lp')
dat.test$predict_risk <- predict(fit, newdata=dat.test, type='risk')
dat.test$predict_prob <- invlogit(dat.test$predict_lp)
dat.test %>% group_by(as.character(x1)) %>% summarize(m_predict_prob=mean(predict_prob))
# 1 0                           0.500
# 2 1                           0.500

# reference="zero"
dat.test$predict_lp <- predict(fit, newdata=dat.test, type='lp', reference="zero")
dat.test$predict_risk <- predict(fit, newdata=dat.test, type='risk', reference="zero")
dat.test$predict_prob <- invlogit(dat.test$predict_lp)
dat.test %>% group_by(as.character(x1)) %>% summarize(m_predict_prob=mean(predict_prob))
# 1 0                           0.551
# 2 1                           0.550


# by hand (didn't give value for strata (set))
dat.test <- data.frame(x1=c(0,1), x2=mean(dat$x2))
dat.test[,"predict_lp"] <- fit$coefficients[1]*dat.test[,"x1"] + fit$coefficients[2]*dat.test[,"x2"]
dat.test$predict_risk <- exp(dat.test[,"predict_lp"])
dat.test$predict_prob <- invlogit(dat.test$predict_lp)
dat.test %>% group_by(as.character(x1)) %>% summarize(m_predict_prob=mean(predict_prob))
#   as.character(x1)` m_predict_prob
# 1 0                           0.551
# 2 1                           0.550

dat.test$predict_prob[2] - dat.test$predict_prob[1]   # -0.000341526



## mixed logit model with strata as fixed effects, and use mfx to get the marginal effect for a certain strata and for average strata
dat$x1 <- as.factor(dat$x1)

logit <- glm(status ~ factor(x1) + x2 + factor(set), family=binomial(link="logit"), data=dat) 

logitmfx(status ~ factor(x1) + x2 + factor(set), data=dat)
# Marginal Effects:
#                    dF/dx  Std. Err.       z  P>|z|
# factor(x1)1   -0.0010206  0.0816829 -0.0125 0.9900




## how to calculate SE (following the same way as logistic regression)
xm <- t(data.frame(x1=mean(as.numeric(as.character(dat$x1))), x2=mean(dat$x2)))
be <- as.matrix(na.omit(coef(fit))) 
vcv <- vcov(fit)

disx0 <- disx1 <- xm
disx0["x1",] <- 0
disx1["x1",] <- 1
plogis(t(be) %*% disx1) - plogis(t(be) %*% disx0)  # 0.000341526

gr <- dlogis(t(be) %*% disx1) %*% t(disx1) - dlogis(t(be) %*% disx0) %*% t(disx0)
sqrt(gr %*% vcv %*% t(gr))   # 0.0713484






###############################################
#    conditional poisson regression (gnm )    #
###############################################

# the same as regular poisson 





############################################
#          logit (gee) regression          #
############################################

# the same as regular logit (but use robust SE)
library(gee)
dat <- data.frame(set=rep(1:50,each=3), status=rep(c(1,0,0),50), x1=rep(c(0,1,0,1,0),30), x2=rnorm(150,7,1.5))
dat$x1 <- as.factor(dat$x1)
dat$set <- as.factor(dat$set)
m_gee <- gee(status~x1+x2, data=dat, family="binomial", id=set, corstr="exchangeable") 



############################################
#          poisson (gee) regression        #
############################################

# the same as poisson (but use robust SE)
# save effect of all covariates (and vcv for calculating SE)



#######################
#      Summary        #
#######################

## from log-odds to marginal effects (observational scale) for dummy variable from logistic/poisson regression 

regular logit: (for dummy variable): linear predictor (average for other co-variates) --> probablity (exp/(1+exp))
regular poisson regression: linear predictor (average for other co-variates) --> expected count (exp)

conditional logit: for dummy variable: predict value for each strata and then average compare direct 
# conditional poisson regression: 

gee logit: any difference with regular logit ? (but use robust SE)
# gee poisson regression: any difference with regular poisson ? (but use robust SE)
# then give some examples of estimates

