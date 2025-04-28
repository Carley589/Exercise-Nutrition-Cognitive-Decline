## Read in new_brfss2 ##

install.packages("survey")

library(tidyverse)

brfss2 <- haven::read_sas("C://Users//carle//Downloads//brfss2.sas7bdat")

names(brfss2)[c(1,9,10,11,12,13,14)] <- c("STATE","AGEG5YR","RACE","PAREC2","PSU",
                                          "LLCPWT","STSTR")

new_brfss2 <- brfss2 %>%
  dplyr::select(CIMEMLOS,EDUCA,SEXVAR,AGEG5YR,RACE,PAREC2,PSU,LLCPWT,STSTR,
                NutritionRec,Region)

## Re-categorize Variables ##

new_brfss2$CIMEMLOS <- as.factor(new_brfss2$CIMEMLOS)

new_brfss2$EDUCA <- as.factor(new_brfss2$EDUCA)

new_brfss2$SEXVAR <- as.factor(new_brfss2$SEXVAR)

new_brfss2$AGEG5YR <- as.factor(new_brfss2$AGEG5YR)

new_brfss2$RACE <- as.factor(new_brfss2$RACE)

new_brfss2$PAREC2 <- as.factor(new_brfss2$PAREC2)

new_brfss2$NutritionRec <- as.factor(new_brfss2$NutritionRec)

new_brfss2$Region <- as.factor(new_brfss2$Region)

## Recode CIMEMLOS ##

new_brfss2$CIMEMLOS1 <- ifelse(new_brfss2$CIMEMLOS==1,1,0)


## Run MI ##

cc <- mice::mice(new_brfss2,printFlag=F)

cc1 <- mice::complete(cc)

## Write to file ##

openxlsx::write.xlsx(cc1,"C://Users//carle//Desktop//Imputed BRFSS 2.xlsx")

## Read in CC1 ##

cc1<-readxl::read_xlsx("C://Users//carle//Desktop//Imputed BRFSS 2.xlsx")


## Recode CIMEMLOS for Imputed Data ##

cc1$CIMEMLOS1 <- ifelse(cc1$CIMEMLOS==1,1,0)

## Releveling Age ##

cc1$AGEG5YR<-relevel(factor(cc1$AGEG5YR), ref = "6")


## Set up imputed survey ##

brfss_survey <- survey::svydesign(id=~PSU,weights=~LLCPWT,strata=~STSTR,data=cc1,
                                  nest=T,survey.lonely.psu="adjust")

brfss_mod1 <- (survey::svyglm(CIMEMLOS1~EDUCA+SEXVAR+AGEG5YR+RACE+Region,
                             family=quasibinomial,design=brfss_survey,na.action=na.omit))

brfss_mod2 <- (survey::svyglm(CIMEMLOS1~EDUCA+SEXVAR+AGEG5YR+RACE+Region+PAREC2+NutritionRec,
                              family=quasibinomial,design=brfss_survey,na.action=na.omit))

## Performing Likelihood Ratio Test ##

install.packages("lmtest")

## First Test- Comparing intercept only model to model containing the covariates ##

survey::regTermTest(brfss_mod1, ~EDUCA+SEXVAR+AGEG5YR+RACE+Region, method = "LRT")

## Second Test- Assessing the contribution of PAREC2 and NutritionRec in addition to the covariates ##

survey::regTermTest(brfss_mod2, ~PAREC2+NutritionRec, method = "LRT")




## Obtain 95% CI's ##

betas <- coef(brfss_mod2)

se <- sqrt(diag(vcov(brfss_mod2)))

ci <- data.frame(
  
  Betas = betas,
  
  SEs = se
  
)

ci <- ci %>% 
  dplyr::mutate(var_name = names(betas),
                b_lower = Betas - 1.96*se,
                b_upper = Betas + 1.96*se,
                odds_ratio = exp(Betas),
                o_lower = exp(b_lower),
                o_upper = exp(b_upper))

## write to file Imputed Confidence Intervals ##

openxlsx::write.xlsx(ci,"C://Users//carle//Desktop//New2 Imputed BRFSS Results.xlsx")


## Complete Case Analyses ##

Completecase<-na.omit(new_brfss2)

## Set up Complete Case survey ##

brfss_survey2 <- survey::svydesign(id=~PSU,weights=~LLCPWT,strata=~STSTR,data=Completecase,
                                  nest=T,survey.lonely.psu="adjust")

brfss_mod3 <- (survey::svyglm(CIMEMLOS1~EDUCA+SEXVAR+AGEG5YR+RACE+Region,
                              family=quasibinomial,design=brfss_survey2,na.action=na.omit))

brfss_mod4 <- (survey::svyglm(CIMEMLOS1~EDUCA+SEXVAR+AGEG5YR+RACE+Region+PAREC2+NutritionRec,
                              family=quasibinomial,design=brfss_survey2,na.action=na.omit))

## Performing Likelihood Ratio Test ##


## First Test- Comparing intercept only model to model containing the covariates ##

survey::regTermTest(brfss_mod3, ~EDUCA+SEXVAR+AGEG5YR+RACE+Region, method = "LRT")

## Second Test- Assessing the contribution of PAREC2 and NutritionRec in addition to the covariates ##

survey::regTermTest(brfss_mod4, ~PAREC2+NutritionRec, method = "LRT")


citation("mice")



## Obtain 95% CI's ##

betas <- coef(brfss_mod4)

se <- sqrt(diag(vcov(brfss_mod4)))

ci <- data.frame(
  
  Betas = betas,
  
  SEs = se
  
)

ci <- ci %>% 
  dplyr::mutate(var_name = names(betas),
                b_lower = Betas - 1.96*se,
                b_upper = Betas + 1.96*se,
                odds_ratio = exp(Betas),
                o_lower = exp(b_lower),
                o_upper = exp(b_upper))
                
## write to file Imputed Confidence Intervals ##
                
openxlsx::write.xlsx(ci,"C://Users//carle//Desktop//Completecase_BRFSS Results.xlsx")


## Hit Rate Test for Imputed Data ##

cc1$PRED_PROBS<-predict(brfss_mod2, newdata = cc1, type = "response")

## However, the cut off point of 0.50, while commonly used and logical,
## is arbitrarily chosen. How do we choose a cut off point that well
## categorizes our observations?? ##

cp <- seq(0,.402,by=0.001)

sn <- vector("double",length(cp))

sp <- vector("double",length(cp))

for(i in 1:length(cp)){
  
  tab <- table(cc1$CIMEMLOS1,cc1$PRED_PROBS > cp[i]) %>%
    prop.table()
  
  sp[i] <- ifelse(dim(tab)[2] == 1,0,tab[1,1]/sum(tab[1,]))
  
  sn[i] <- ifelse(dim(tab)[2] == 1,0,tab[2,2]/sum(tab[2,]))
  
}

## Blue is sensitivity, Red is specificity ##

ggplot() +
  geom_line(aes(x = cp, y = sn),color='blue') +
  geom_line(aes(x = cp, y = sp),color='red') +
  theme_classic() + labs(x = "Cut Points",
                         y = "Probabilities") +
  scale_x_continuous(breaks = seq(0,1,by=0.01))

df <- abs(sn-sp)
cp[which.min(df[-c(1:17)])]


tab1<-table(cc1$CIMEMLOS1, cc1$PRED_PROBS>0.102)

tab1

tab1%>%prop.table(margin = 1)
summary(cc1$PRED_PROBS)


## Hit Rate Test for Complete Case Data ##

Completecase$PRED_PROBS<-predict(brfss_mod4, newdata = Completecase, type = "response")

## However, the cut off point of 0.50, while commonly used and logical,
## is arbitrarily chosen. How do we choose a cut off point that well
## categorizes our observations?? ##

cp <- seq(0,.402,by=0.001)

sn <- vector("double",length(cp))

sp <- vector("double",length(cp))

for(i in 1:length(cp)){
  
  tab <- table(Completecase$CIMEMLOS1,Completecase$PRED_PROBS > cp[i]) %>%
    prop.table()
  
  sp[i] <- ifelse(dim(tab)[2] == 1,0,tab[1,1]/sum(tab[1,]))
  
  sn[i] <- ifelse(dim(tab)[2] == 1,0,tab[2,2]/sum(tab[2,]))
  
}

## Blue is sensitivity, Red is specificity ##

ggplot() +
  geom_line(aes(x = cp, y = sn),color='blue') +
  geom_line(aes(x = cp, y = sp),color='red') +
  theme_classic() + labs(x = "Cut Points",
                         y = "Probabilities") +
  scale_x_continuous(breaks = seq(0,1,by=0.01))

df <- abs(sn-sp)
cp[which.min(df[-c(1:17)])]


tab2<-table(Completecase$CIMEMLOS1, Completecase$PRED_PROBS>0.107)

tab2

tab2%>%prop.table(margin = 1)


## Frequency of response tables and chi-square test for Imputed Data##

ed_tab<-
  
  table(cc1$EDUCA,cc1$CIMEMLOS)

chisq.test(ed_tab)

ed_tab


exerc_tab<-
  
  table(cc1$PAREC2,cc1$CIMEMLOS)

chisq.test(exerc_tab)

exerc_tab


nutri_tab<-
  
  table(cc1$NutritionRec,cc1$CIMEMLOS)

chisq.test(nutri_tab)

nutri_tab


sex_tab<-
  
  table(cc1$SEXVAR,cc1$CIMEMLOS)

chisq.test(sex_tab)

sex_tab


age_tab<-
  
  table(cc1$AGEG5YR,cc1$CIMEMLOS)

chisq.test(age_tab)

age_tab  


ethnic_tab<-
  
  table(cc1$RACE,cc1$CIMEMLOS)

chisq.test(ethnic_tab)

ethnic_tab


reg_tab<-
  
  table(cc1$Region,cc1$CIMEMLOS)

chisq.test(reg_tab)


## Frequency of response tables and chi-square test for Complete Case Data##

ed_tab<-
  
  table(Completecase$EDUCA,Completecase$CIMEMLOS)

chisq.test(ed_tab)

ed_tab


exerc_tab<-
  
  table(Completecase$PAREC2,Completecase$CIMEMLOS)

chisq.test(exerc_tab)

exerc_tab


nutri_tab<-
  
  table(Completecase$NutritionRec,Completecase$CIMEMLOS)

chisq.test(nutri_tab)

nutri_tab


sex_tab<-
  
  table(Completecase$SEXVAR,Completecase$CIMEMLOS)

chisq.test(sex_tab)

sex_tab


age_tab<-
  
  table(Completecase$AGEG5YR,Completecase$CIMEMLOS)

chisq.test(age_tab)

age_tab  


ethnic_tab<-
  
  table(Completecase$RACE,Completecase$CIMEMLOS)

chisq.test(ethnic_tab)

ethnic_tab


reg_tab<-
  
  table(Completecase$Region,Completecase$CIMEMLOS)

chisq.test(reg_tab)

reg_tab

## Scatterplot of residuals versus predicted values ##

## Compute the residual with the resid function ##

CIMEMLOS.lm = lm(CIMEMLOS~NutritionRec, data=Completecase)

CIMEMLOS.res = resid(CIMEMLOS.lm)

## Plot the residual against observed values of variable ##

plot(CIMEMLOS.res, Completecase$NutritionRec,
     ylab="Nutrition Rec", xlab="Residuals",
     main="Subjective Cognitive Decline")
abline(0, 0)  # the horizon

