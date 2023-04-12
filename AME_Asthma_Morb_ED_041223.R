### Average Marginal Effect Model - ASTHMATIC DIABETES  - ED 04/12/23 ###

 
##################### Packages to be installed #####################

install.packages("readxl")
library(readxl)

install.packages("broom")
library(broom)

install.packages("ggeffects")
library(ggeffects)
update.packages("ggeffects")

install.packages("margins")
library(margins)
data(mtcars)

#####################################################################



obs <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Cormobdities\\Out\\out_ED03.xlsx", sheet="COMM_ED")


# View(obs)
# summary(mob)


## Convering DV to a numeric variable
obs$bsev_n <- as.numeric(obs$bsev)
obs$int_n <- as.numeric(obs$int)
obs$pa_n <- as.numeric(obs$pa)
obs$sev_n <- as.numeric(obs$sev)


## Creating subgroups by ages - subset function
obss = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5"))
obso = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5") & AgeInYears > 18)
obsy = subset(obs, bsev_n !="9" & Race_val %in% c("2","3","4","5") & AgeInYears < 19)

### Tables including the levels of BMI
obsy0 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & sev_n %in% c("0","9"))
obsy1 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & sev_n %in% c("1","9"))
obsy2 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & sev_n %in% c("2","9"))
obsy3 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & sev_n %in% c("3","9"))
# View(obss)


### Values converted
obsy0$sev_n <- ifelse(obsy0$sev_n == 0, 1, obsy0$sev_n)
obsy0$sev_n <- ifelse(obsy0$sev_n == 9, 0, obsy0$sev_n)

obsy1$sev_n <- ifelse(obsy1$sev_n == 1, 1, obsy1$sev_n)
obsy1$sev_n <- ifelse(obsy1$sev_n == 9, 0, obsy1$sev_n)

obsy2$sev_n <- ifelse(obsy2$sev_n == 2, 1, obsy2$sev_n)
obsy2$sev_n <- ifelse(obsy2$sev_n == 9, 0, obsy2$sev_n)

obsy3$sev_n <- ifelse(obsy3$sev_n == 3, 1, obsy3$sev_n)
obsy3$sev_n <- ifelse(obsy3$sev_n == 9, 0, obsy3$sev_n)

# View(obsy1)


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

## Logistic regression with BMI
ip60 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy0)
ip61 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy1)
ip62 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy2)
ip63 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy3)

summary(ip60)
summary(ip61)
summary(ip62)
summary(ip63)

margins(ip31) #.1255  .04892
margins(ip32) #.146   .08302
margins(ip33) #.1196  .09353

margins(ip41) #.04915
margins(ip42) #.06662
margins(ip43) #.07563

margins(ip51) #.1551
margins(ip52) #.1837
margins(ip53) #.1819

margins(ip60)
margins(ip61)
margins(ip62)
margins(ip63)


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

summary(margins(ip60, variables = c("bmi")))
summary(margins(ip61, variables = c("bmi")))
summary(margins(ip62, variables = c("bmi")))
summary(margins(ip63, variables = c("bmi")))


#### END OF THE TEST - 04/12/2023 ####

