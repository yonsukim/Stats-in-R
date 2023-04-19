### Average Marginal Effect Model - ASTHMATIC DIABETES  04/07/23 ###
 

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

    
 
obs <- read_excel("C:\\Users\\...\\Dropbox\\Research\\Asthma_Cormobdities\\Out\\out.xlsx", sheet="COMM_IP")


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

### Tables including the levels of BMI
obsy10 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","1") & sev_n %in% c("0","9"))
obsy11 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","1") & sev_n %in% c("1","9"))
obsy12 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","1") & sev_n %in% c("2","9"))
obsy13 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","1") & sev_n %in% c("3","9"))

obsy20 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","2") & sev_n %in% c("0","9"))
obsy21 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","2") & sev_n %in% c("1","9"))
obsy22 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","2") & sev_n %in% c("2","9"))
obsy23 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","2") & sev_n %in% c("3","9"))

obsy30 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","3") & sev_n %in% c("0","9"))
obsy31 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","3") & sev_n %in% c("1","9"))
obsy32 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","3") & sev_n %in% c("2","9"))
obsy33 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","3") & sev_n %in% c("3","9"))


### Tables using INTERMIT as reference
obsy110 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","1") & sev_n %in% c("0","1"))
obsy111 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","1") & sev_n %in% c("0","2"))
obsy112 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","1") & sev_n %in% c("0","3"))

obsy120 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","2") & sev_n %in% c("0","1"))
obsy121 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","2") & sev_n %in% c("0","2"))
obsy122 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","2") & sev_n %in% c("0","3"))

obsy130 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","3") & sev_n %in% c("0","1"))
obsy131 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","3") & sev_n %in% c("0","2"))
obsy132 = subset(obs, Race_val %in% c("2","3","4","5") & AgeInYears > 18 & bmi %in% c("0","3") & sev_n %in% c("0","3"))

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

obsy10$sev_n <- ifelse(obsy10$sev_n == 0, 1, obsy10$sev_n)
obsy10$sev_n <- ifelse(obsy10$sev_n == 9, 0, obsy10$sev_n)
obsy11$sev_n <- ifelse(obsy11$sev_n == 1, 1, obsy11$sev_n)
obsy11$sev_n <- ifelse(obsy11$sev_n == 9, 0, obsy11$sev_n)
obsy12$sev_n <- ifelse(obsy12$sev_n == 2, 1, obsy12$sev_n)
obsy12$sev_n <- ifelse(obsy12$sev_n == 9, 0, obsy12$sev_n)
obsy13$sev_n <- ifelse(obsy13$sev_n == 3, 1, obsy13$sev_n)
obsy13$sev_n <- ifelse(obsy13$sev_n == 9, 0, obsy13$sev_n)

obsy20$sev_n <- ifelse(obsy20$sev_n == 0, 1, obsy20$sev_n)
obsy20$sev_n <- ifelse(obsy20$sev_n == 9, 0, obsy20$sev_n)
obsy21$sev_n <- ifelse(obsy21$sev_n == 1, 1, obsy21$sev_n)
obsy21$sev_n <- ifelse(obsy21$sev_n == 9, 0, obsy21$sev_n)
obsy22$sev_n <- ifelse(obsy22$sev_n == 2, 1, obsy22$sev_n)
obsy22$sev_n <- ifelse(obsy22$sev_n == 9, 0, obsy22$sev_n)
obsy23$sev_n <- ifelse(obsy23$sev_n == 3, 1, obsy23$sev_n)
obsy23$sev_n <- ifelse(obsy23$sev_n == 9, 0, obsy23$sev_n)

obsy30$sev_n <- ifelse(obsy30$sev_n == 0, 1, obsy30$sev_n)
obsy30$sev_n <- ifelse(obsy30$sev_n == 9, 0, obsy30$sev_n)
obsy31$sev_n <- ifelse(obsy31$sev_n == 1, 1, obsy31$sev_n)
obsy31$sev_n <- ifelse(obsy31$sev_n == 9, 0, obsy31$sev_n)
obsy32$sev_n <- ifelse(obsy32$sev_n == 2, 1, obsy32$sev_n)
obsy32$sev_n <- ifelse(obsy32$sev_n == 9, 0, obsy32$sev_n)
obsy33$sev_n <- ifelse(obsy33$sev_n == 3, 1, obsy33$sev_n)
obsy33$sev_n <- ifelse(obsy33$sev_n == 9, 0, obsy33$sev_n)

# 3X3 models with INTERMIT as ref.
obsy111$sev_n <- ifelse(obsy111$sev_n == 2, 1, obsy111$sev_n)
obsy112$sev_n <- ifelse(obsy112$sev_n == 3, 1, obsy112$sev_n)

obsy121$sev_n <- ifelse(obsy121$sev_n == 2, 1, obsy121$sev_n)
obsy122$sev_n <- ifelse(obsy122$sev_n == 3, 1, obsy122$sev_n)

obsy131$sev_n <- ifelse(obsy131$sev_n == 2, 1, obsy131$sev_n)
obsy132$sev_n <- ifelse(obsy132$sev_n == 3, 1, obsy132$sev_n)


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

### 3X4 models
ip010 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy10)
ip011 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy11)
ip012 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy12)
ip013 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy13)

ip020 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy20)
ip021 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy21)
ip022 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy22)
ip023 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy23)

ip030 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy30)
ip031 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy31)
ip032 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy32)
ip033 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy33)

### 3X3 models with INTERMIT as ref.
ip110 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy110)
ip111 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy111)
ip112 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy112)

ip120 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy120)
ip121 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy121)
ip122 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy122)

ip130 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy130)
ip131 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy131)
ip132 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy132)


summary(ip60)
summary(ip61)
summary(ip62)
summary(ip63)

summary(ip010)
summary(ip011)
summary(ip012)
summary(ip013)

summary(ip020)
summary(ip021)
summary(ip022)
summary(ip023)

summary(ip030)
summary(ip031)
summary(ip032)
summary(ip033)

summary(ip110)
summary(ip111)
summary(ip112)

summary(ip120)
summary(ip121)
summary(ip122)

summary(ip130)
summary(ip131)
summary(ip132)




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

summary(margins(ip010, variables = c("bmi")))
summary(margins(ip011, variables = c("bmi")))
summary(margins(ip012, variables = c("bmi")))
summary(margins(ip013, variables = c("bmi")))

summary(margins(ip020, variables = c("bmi")))
summary(margins(ip021, variables = c("bmi")))
summary(margins(ip022, variables = c("bmi")))
summary(margins(ip023, variables = c("bmi")))

summary(margins(ip030, variables = c("bmi")))
summary(margins(ip031, variables = c("bmi")))
summary(margins(ip032, variables = c("bmi")))
summary(margins(ip033, variables = c("bmi")))

# 3x3 models with INTMIT as ref.
summary(margins(ip110, variables = c("bmi")))
summary(margins(ip111, variables = c("bmi")))
summary(margins(ip112, variables = c("bmi")))

summary(margins(ip120, variables = c("bmi")))
summary(margins(ip121, variables = c("bmi")))
summary(margins(ip122, variables = c("bmi")))

summary(margins(ip130, variables = c("bmi")))
summary(margins(ip131, variables = c("bmi")))
summary(margins(ip132, variables = c("bmi")))


#### END OF THE TEST - 04/19/2023 ####

