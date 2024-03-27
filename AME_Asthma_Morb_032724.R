### Average Marginal Effect Model - ASTHMATIC DIABETES  04/07/23 ###
### FINAL MOELS Updated 03/27/24 ###

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

    
 
obs <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Cormobdities\\Out\\out_032724.xlsx", sheet="COMM_IP")


# View(obs)
# summary(obs$los_n)
# summary(obs$los)
# summary(mob)

is.numeric(obs$LOS)
is.numeric(obs$bsev)
is.numeric(obs$bsev_n)

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





###### Adjusted models 

### 3X3 models with INTERMIT as ref. -  Final models
ip110 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy110)
ip111 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy111)
ip112 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy112)

ip120 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy120)
ip121 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy121)
ip122 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy122)

ip130 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy130)
ip131 <- glm(sev_n ~ bmi+age+race+Gender_val+medi, family = binomial(), data = obsy131)
ip132 <- glm(sev_n ~ bmi+age+race+Gender_val+medi+LOS, family = binomial(), data = obsy132)


# summary - Final models
summary(ip110)
summary(ip111)
summary(ip112)

summary(ip120)
summary(ip121)
summary(ip122)

summary(ip130)
summary(ip131)
summary(ip132)


# 3x3 models with INTMIT as ref.
summary(margins(ip110, variables = c("bmi"))) # 1.7%, p=0.988
summary(margins(ip111, variables = c("bmi"))) # 15.6%, p=0.047
summary(margins(ip112, variables = c("bmi"))) # 15.3%, p=0.062

summary(margins(ip120, variables = c("bmi"))) # -10.0%, p=0.230
summary(margins(ip121, variables = c("bmi"))) #  12.0%, p=0.894
summary(margins(ip122, variables = c("bmi"))) # -1.9%,  p=0.837

summary(margins(ip130, variables = c("bmi"))) # 15.3%, p=0.023
summary(margins(ip131, variables = c("bmi"))) # 7.8%,  p=0.121
summary(margins(ip132, variables = c("bmi"))) # 12.6%, p=0.009


summary(margins(ip130, variables = c("Gender_val")))
summary(margins(ip130, variables = c("age")))
summary(margins(ip130, variables = c("race")))
summary(margins(ip130, variables = c("medi")))

summary(margins(ip132, variables = c("Gender_val")))
summary(margins(ip132, variables = c("age")))
summary(margins(ip132, variables = c("race")))
summary(margins(ip132, variables = c("medi")))

#### END OF THE TEST - 03/27/2023 ####

