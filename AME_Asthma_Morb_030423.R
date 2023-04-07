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

obs <- read_excel("C:\\Users\\.....\\Dropbox\\Research\\Asthma_Cormobdities\\Out\\out.xlsx", sheet="COMM_IP")


# View(obs)
# summary(mob)


## Convering DV to a numeric variable
obs$bsev_n <- as.numeric(obs$bsev)
obs$int_n <- as.numeric(obs$int)
obs$pa_n <- as.numeric(obs$pa)


## Creating subgroups by ages - subset function
obss = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5"))
obso = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5") & AgeInYears > 18)
obsy = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5") & AgeInYears < 19)
# View(obss)


## Average Marginal Effect (AME) test with the unadjusted models

ip01 <- glm(bsev_n ~ obs, family = binomial(), data = obss)
ip02 <- glm(int_n  ~ obs, family = binomial(), data = obss)
ip03 <- glm(pa_n   ~ obs, family = binomial(), data = obss)

ip11 <- glm(bsev_n ~ obs, family = binomial(), data = obso)
ip12 <- glm(int_n  ~ obs, family = binomial(), data = obso)
ip13 <- glm(pa_n   ~ obs, family = binomial(), data = obso)

ip21 <- glm(bsev_n ~ obs, family = binomial(), data = obsy)
ip22 <- glm(int_n  ~ obs, family = binomial(), data = obsy)
ip23 <- glm(pa_n   ~ obs, family = binomial(), data = obsy)

# summary(ip1)
# exp(cbind(OR=coef(ip1),confint(ip1)))
# plot(ggpredict(ip01, "moc [all]"), ci = FALSE, add.data = TRUE)
# coef(ip01)["1"]

margins(ip01) ## .1477
margins(ip02) ## .1649
margins(ip03) ## .1284

margins(ip11) ## .04029
margins(ip12) ## .05528
margins(ip13) ## .06106

margins(ip21) ## .1596
margins(ip22) ## .1986
margins(ip23) ## .1929

# AME with SE, Z, P, CI
summary(margins(ip01, variables = c("obs")))
summary(margins(ip02, variables = c("obs")))
summary(margins(ip03, variables = c("obs")))

summary(margins(ip11, variables = c("obs")))
summary(margins(ip12, variables = c("obs")))
summary(margins(ip13, variables = c("obs")))

summary(margins(ip21, variables = c("obs")))
summary(margins(ip22, variables = c("obs")))
summary(margins(ip23, variables = c("obs")))


###### Adjusted models 
ip31 <- glm(bsev_n ~ obs+race+Gender_val+medi, family = binomial(), data = obss)
ip32 <- glm(int_n  ~ obs+race+Gender_val+medi, family = binomial(), data = obss)
ip33 <- glm(pa_n   ~ obs+race+Gender_val+medi, family = binomial(), data = obss)

ip41 <- glm(bsev_n ~ obs+age+race+Gender_val+medi, family = binomial(), data = obso)
ip42 <- glm(int_n  ~ obs+age+race+Gender_val+medi, family = binomial(), data = obso)
ip43 <- glm(pa_n   ~ obs+age+race+Gender_val+medi, family = binomial(), data = obso)

ip51 <- glm(bsev_n ~ obs+race+Gender_val+medi, family = binomial(), data = obsy)
ip52 <- glm(int_n  ~ obs+race+Gender_val+medi, family = binomial(), data = obsy)
ip53 <- glm(pa_n   ~ obs+race+Gender_val+medi, family = binomial(), data = obsy)


margins(ip31) #.1255  .04892
margins(ip32) #.146   .08302
margins(ip33) #.1196  .09353

margins(ip41) #.04915
margins(ip42) #.06662
margins(ip43) #.07563

margins(ip51) #.1551
margins(ip52) #.1837
margins(ip53) #.1819


# Summary tables, OR, and CI
summary(ip31)
exp(cbind(OR=coef(ip31),confint(ip31)))

# AME with SE, Z, P, CI
summary(margins(ip31, variables = c("obs")))
summary(margins(ip32, variables = c("obs")))
summary(margins(ip33, variables = c("obs")))
 
summary(margins(ip41, variables = c("obs")))
summary(margins(ip42, variables = c("obs")))
summary(margins(ip43, variables = c("obs")))

summary(margins(ip51, variables = c("obs")))
summary(margins(ip52, variables = c("obs")))
summary(margins(ip53, variables = c("obs")))

#### END OF THE TEST - 04/07/2023 ####

