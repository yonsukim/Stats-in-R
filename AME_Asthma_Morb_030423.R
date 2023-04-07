### Average Marginal Effect Model - ASTHMATIC DIABETES  04/07/23 ###


##################### Packages to be installed #####################

install.packages("readxl")
library(readxl)

## install.packages("broom")
## library(broom)

## install.packages("ggeffects")
## library(ggeffects)
## update.packages("ggeffects")

## install.packages("margins")
## library(margins)
## data(mtcars)

#####################################################################




##################### Social Mobility Data ##########################################

example <- read_excel("C:\\Users\\..........\\....xlsx") ### Needs to edit this path based on data location

obs <- read_excel("C:\\Users\\Dongwoo\\Dropbox\\Research\\Asthma_Cormobdities\\Out\\out.xlsx", sheet="COMM_IP")


# View(obs)
# summary(mob)



## Convering DV to a numeric variable
obs$bsev_n <- as.numeric(obs$bsev)
obs$int_n <- as.numeric(obs$int)
obs$pa_n <- as.numeric(obs$pa)



obss = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5"))
obso = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5") & AgeInYears > 18)
obsy = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5") & AgeInYears < 19)
# View(obss)



## Average Marginal Effect (AME) test with the unadjusted model

ip01 <- glm(bsev_n ~ obs, family = binomial(), data = obss)
ip02 <- glm(int_n ~ obs, family = binomial(), data = obss)
ip03 <- glm(pa_n ~ obs, family = binomial(), data = obss)

ip11 <- glm(bsev_n ~ obs, family = binomial(), data = obso)
ip12 <- glm(int_n ~ obs, family = binomial(), data = obso)
ip13 <- glm(pa_n ~ obs, family = binomial(), data = obso)

ip21 <- glm(bsev_n ~ obs, family = binomial(), data = obsy)
ip22 <- glm(int_n ~ obs, family = binomial(), data = obsy)
ip23 <- glm(pa_n ~ obs, family = binomial(), data = obsy)


summary(ip1)
exp(cbind(OR=coef(ip1),confint(ip1)))  ## Gives the same results as SAS
plot(ggpredict(ip01, "moc [all]"), ci = FALSE, add.data = TRUE)

coef(ip01)["1"]

margins(ip01) ## .1477
margins(ip02) ## .1649
margins(ip03) ## .1284

margins(ip11) ## .04029
margins(ip12) ## .05528
margins(ip13) ## .06106

margins(ip21) ## .1596
margins(ip22) ## .1986
margins(ip23) ## .1929


###### Adjusted models 

ip31 <- glm(bsev_n ~ obs+age+race+Gender_val+medi, family = binomial(), data = obss)
ip32 <- glm(int_n ~ obs+age+race+Gender_val+medi, family = binomial(), data = obss)
ip33 <- glm(pa_n ~ obs+age+race+Gender_val+medi, family = binomial(), data = obss)

ip41 <- glm(bsev_n ~ obs+age+race+Gender_val+medi, family = binomial(), data = obso)
ip42 <- glm(int_n ~ obs+age+race+Gender_val+medi, family = binomial(), data = obso)
ip43 <- glm(pa_n ~ obs+age+race+Gender_val+medi, family = binomial(), data = obso)

ip51 <- glm(bsev_n ~ obs+race+Gender_val+medi, family = binomial(), data = obsy)
ip52 <- glm(int_n ~ obs+race+Gender_val+medi, family = binomial(), data = obsy)
ip53 <- glm(pa_n ~ obs+race+Gender_val+medi, family = binomial(), data = obsy)


margins(ip31) #.04892
margins(ip32) #.08302
margins(ip33) #.09353

margins(ip41) # .04915
margins(ip42) # .06662
margins(ip43) # .07563

margins(ip51) # .1551
margins(ip52) # .1837
margins(ip53) # .1819



summary(model_05)  ## Gives the same results as SAS
exp(cbind(OR=coef(model_05),confint(model_05)))  ## Gives the same results as SAS

plot(ggpredict(model_00, "moc [all]"), ci = FALSE, add.data = TRUE)

margins(model_05)

## Summary function provides AME, SE, Z, P, CI - Divided into two groups
summary(margins(model_05, variables = c("moc", "sq1", "aggrp")))
summary(margins(model_05, variables = c("edui","hinc","rlgi","mari")))





#### END OF THE TEST - 04/07/2023 ####

