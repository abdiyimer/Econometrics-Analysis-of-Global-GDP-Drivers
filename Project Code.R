install.packages("readxl")
install.packages("plm")
install.packages("dplyr")
install.packages("stargazer")
install.packages("pglm")

library(pglm)
library(stargazer)
library(readxl)
library(plm)
library(car)
library(dplyr)
library(lmtest)
library(sandwich)
library(lmtest)



setwd("C:\\Users\\asus\\Desktop\\Data Science\\Advanced Econometrics\\Project")
Data=read_excel("pwt100.xlsx", sheet = "Data")

View(Data)
colnames(Data)



## Filtering the Numeric Features... since the Categorical are just Identification, we Don't Need them 
## in the model 

non_numeric <- Data[, !sapply(Data, is.numeric)]
colnames(non_numeric)

numeric_data <- Filter(is.numeric, Data)
# numeric_data <- Data[, sapply(Data, is.numeric)]

ncol(numeric_data)
colnames(numeric_data)

### We Did i lot of Correlation Analysis on all the Features, Since we are Dealing with High Multicolinearity
### We Couldn't fit the General Model for all of the Features.

### Therefore we used our Intuition (to Filter which Features are more Important for our research Question)
### and Correlation Analysis to get Rid of Repeated Features 

## so using the above Procedure we reduced 55 Features to 25

# Fitting Gineral Model
fixed_general_model <- plm(rgdpo ~ pop+avh+emp+hc+ccon+cda+cn+ck+ctfp+cwtfp+rconna+rdana+rnna+rkna
                           +rtfpna+labsh+irr+delta+xr+csh_c+csh_i+csh_g+csh_x+csh_m+csh_r
                           ,data = Data, model = "within",
                           index=c("country","year"))

# Spearman's rank correlation test between variable1 and variable2
cor_test_result <- cor.test(numeric_data$cgdpe, numeric_data$rgdpna, method = "spearman")

cor_test_result
# Collinearity Problems 
# cgdpe,cgdpo,rgdpe with rgdpo(Target),rgdpna



# Collinearity Problems 
# cn and ck
cor.test(numeric_data$cn, numeric_data$ck, method = "spearman") 
   # Highly correlated... we select ck (more appropriate)


# Collinearity Problems 
# ctfp and cwtfp (preferable) -->Welfare-relevant TFP levels at current PPPs
cor.test(numeric_data$ctfp, numeric_data$cwtfp, method = "spearman")

# Collinearity Problems 
# ccon and rconna ,,---> ccon better to compare across countries 
cor.test(numeric_data$ccon, numeric_data$rconna, method = "spearman")

# Collinearity Problems 
# cn and rnna,rkna ,---> cn better to compare across countries 


#  Collinearity Problems 
# ctfp and rtfpna,  --> ctfp better to compare across countries
cor.test(numeric_data$ctfp, numeric_data$rtfpna, method = "spearman")

# ctfp and cwtfp,,, --> ctfp is better 
cor.test(numeric_data$ctfp, numeric_data$cwtfp, method = "spearman")

#  Collinearity Problems 
# ctfp and rtfpna,  --> ctfp better to compare across countries
cor.test(numeric_data$pl_con, numeric_data$pl_da, method = "spearman") 


#  Collinearity Problems 
# pl_con, pl_da and pl_gdpo,  --> pl_gdpo better to compare across countries
cor.test(numeric_data$pl_gdpo, numeric_data$pl_con, method = "spearman") 

#  Collinearity Problems 
# pl_con, pl_da and pl_gdpo,  --> pl_gdpo better to compare across countries
cor.test(numeric_data$csh_r, numeric_data$csh_m, method = "spearman") 


#  Collinearity Problems 
# pl_con, pl_da and pl_gdpo,  --> pl_gdpo better to compare across countries
cor.test(numeric_data$pl_c, numeric_data$pl_g, method = "spearman") 

##  Collinearity Problems 
# pop, emp and pl_gdpo,  --> pl_gdpo better to compare across countries
cor.test(numeric_data$cda, numeric_data$emp, method = "spearman")


cor_matrix <- cor(Data[, c("pop", "avh","hc",'ck',"ccon","ctfp","rtfpna",
                           "labsh","irr","delta","xr","pl_gdpo","csh_c","csh_i","csh_g",
                           "csh_x","csh_r")], use = "pairwise.complete.obs")

cor_matrix


### Doing Further Analysis and Reading Research paper, it turns out some of the Features are still 
### more or less the same they Describe the same thing so we decided to Take one of them by Carefully
### Considering the Economic Meaning  


fixed_model <- plm(rgdpo~pop+emp+avh+hc+xr+ccon+ctfp+rtfpna+labsh+pl_con+pl_gdpo+csh_c+csh_i+csh_x
                   ,data = Data, model = "within",index=c("country","year"))


summary(fixed_model)

ols=lm(rgdpo~pop+emp+avh+hc+ccon+ctfp+rtfpna+labsh+xr+pl_con+pl_gdpo+csh_c+csh_i+csh_x
       ,data = Data)
summary(ols)


Random_Model<-plm(rgdpo ~ pop+emp+avh+hc+ccon+ctfp+rtfpna+labsh+xr+pl_con+pl_gdpo+csh_c+csh_i+csh_x
                  ,data = Data, model = "random",
                  index=c("country","year"))


# Removed Features Due to Redandancy
# irr, cda, ck, csh_g, csh_m, csh_r,cn,cwtfp,rconna, rdana, rnna, rkna, delta, 

summary(Random_Model)


################################################################################################
##########                        HAUSMAN TEST                                    ##############
################################################################################################

phtest(fixed_model, Random_Model)

# Reject H0: Both models are consistent
# p-value < 2.2e-16 WE Reject the Null Hypothesis
# so, FE model is preferable

### TO SEE THE FIXED EFFECT OF EACH COUNTRY
fixef(fixed_model)

################################################################################################
##########                        POOLABILITY TEST                                ##############
################################################################################################

pFtest(fixed_model,ols)  #  ---> Individual Effect
  # p-value < 2.2e-16 We reject the Hypothesis that says both models are equal
  # so FE is more consistent or there is a significant (individual effect)fixed effect 


################################################################################################
##########                        SERIAL CORRELATION TEST                         ##############
################################################################################################

pbgtest(fixed_model)
  # p-value < 2.2e-16, we reject the null hypothesis that says no serial correlation 
  # there is serial correlation in the data (There is autocorrelation of residuals in the model)
  # one of the assumption is Rejected so this model Doesn't Reflect REALITY. we have to do some Measure


################################################################################################
##########                         HOMOCEDASTICITY TEST                           ##############
################################################################################################

# Breusch-Pagan test 
bptest(ols, studentize=T)

bptest(fixed_model, studentize = TRUE)
  # p-value < 2.2e-16, WE reject the null hypothesis that says "there is no heterocedasticity"
  # or the residuals exhibit homogeneous pattern (Homocedasticity)

############
### NOTE ###
############

## Fixed Effects Model: The Fixed Effects model does not require the residuals to be normally distributed 
## for the parameter estimates to be consistent. The normality assumption is primarily relevant for the 
## error term in hypothesis testing. so no need ot conduct the normality test 


################################################################################################
##########                         ROBUST STANDARD ERROR MODEL                    ##############
################################################################################################
summary(fixed_model)
summary(Random_Model)
Robust_std_err_model=coeftest(fixed_model, vcov.=vcovHC(fixed_model, method="white1", type="HC0", cluster="group"))
Robust_std_err_model

## Now we have the General Model to proceed with 

################################################################################
##########     GENERAL TO SPECIFIC MODEL                       #################
################################################################################

Robust_std_err_model
## STEP 1:
## Checking the Joint Insignificance of the Insignificant variables
## The Insignificant Variables are labsh and hc
## Hypothesis the Coefficients of those Variables arre Zero at the same time
## H0 : beta(labsh) = 0
##      beta(hc) = 0

   ## Jointly insignificant remove them at once

linearHypothesis(Robust_std_err_model, c("hc=0", "labsh=0"),vcov.=vcovHC(fixed_model, method="white1", type="HC0", cluster="group"))

## p_value =  0.3427, We fail to Reject the Null Hypothesis 
## So, the insignificant variables have Zero Coefficient (They are Jointly insignificant for the Model)
## Therefore, we can Get rid of them at once from the model 


## FINAL MODEL 
FINAL_FIXED  <- plm(rgdpo~pop+emp+avh+ccon+ctfp+rtfpna+xr+pl_con+pl_gdpo+csh_c+csh_i+csh_x
                     ,data = Data, model = "within",index=c("country","year"))

summary(FINAL_FIXED)

FINAL_ROBUST_STD.ERR_MODEL = coeftest(FINAL_FIXED, vcov = vcovSCC(FINAL_FIXED, type = "HC0", maxlag = 1))
print(FINAL_ROBUST_STD.ERR_MODEL)


####################################################################################
#############################    COMPARING MODELS      #############################
####################################################################################
# aic_Random_Model <- AIC(Random_Model)
# aic_fixed_model <- AIC(fixed_model)
# aic_Robust_std_err_model <- AIC(Robust_std_err_model)
# aic_FINAL_ROBUST_STD.ERR_MODEL <- AIC(FINAL_ROBUST_STD.ERR_MODEL)
stargazer(Random_Model,fixed_model, Robust_std_err_model, FINAL_ROBUST_STD.ERR_MODEL, 
          type = "text", 
          title = "Comparison of Different Models",
          dep.var.labels = "Dependent Variable: GDP",
          
          out = "models_comparison.txt")






# Compute the log-likelihood (using RSS as an approximation)
logLik_Random <- -nobs(Random_Model)/2 * (log(2 * pi) + log(sum(residuals(Random_Model)^2) / nobs(Random_Model)) + 1)
logLik_Fixed <- -nobs(fixed_model)/2 * (log(2 * pi) + log(sum(residuals(fixed_model)^2) / nobs(fixed_model)) + 1)
logLik_FINAL_FIXED <- -nobs(FINAL_FIXED)/2 * (log(2 * pi) + log(sum(residuals(FINAL_FIXED)^2) / nobs(FINAL_FIXED)) + 1)

#logLik_Robust <- -nobs(FINAL_FIXED)/2 * (log(2 * pi) + log(sum(residuals(FINAL_FIXED)^2) / nobs(FINAL_FIXED)) + 1)


# Count the number of parameters
k_Final <- length(coef(FINAL_FIXED))
k_Random <- length(coef(Random_Model))
k_Fixed <- length(coef(fixed_model))
k_Robust <- length(coef(FINAL_ROBUST_STD.ERR_MODEL))



# Compute AIC
AIC_Random <- -2 * logLik_Random + 2 * k_Random
AIC_Fixed <- -2 * logLik_Fixed + 2 * k_Fixed
AIC_Final <- -2 * logLik_FINAL_FIXED + 2 * k_Final
AIC_Robust <- -2 * logLik_FINAL_FIXED + 2 * k_Robust
# Print AIC values
print(AIC_Random)
print(AIC_Fixed)
print(AIC_Final)
print(AIC_Robust)

## As we see The AIC value of the models, The final Model with the the significant variable has smaller value 
## which indicates this model is better than the General one.. Even if the Robust and Normal Model is the same
## we know that Due to HETEROCEDACTICITY and Autocorrelation we prefer the Robust Model
