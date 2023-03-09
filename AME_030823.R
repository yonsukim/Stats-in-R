### Average Marginal Effect Model ###


##################### Packages to be installed #####################

install.packages("readxl")
library(readxl)

## install.packages("broom")
## library(broom)

## install.packages("ggeffects")
## library(ggeffects)
## update.packages("ggeffects")


## library(margins)
## data(mtcars)

#####################################################################


### Example ###

set.seed(123)
y <- rbinom(300, 1, c(.3, .7))
x <- rnorm(300, 2)
y_1 <- y == 1
x[y_1] <- x[y_1] + rnorm(sum(y_1), 3)

d <- data.frame(x, y)
model2 <- glm(y ~ x, family = binomial(), data = d)

summary(model2)
coef(model2)["x"]

plot(ggpredict(model2, "x [all]"), ci = FALSE, add.data = TRUE)

View(y_1)



##################### Social Mobility Data ##########################################

mob <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Mobility\\Mobility_data\\mob_02.xlsx") ### Needs to edit this path based on data location


# View(mob)
# summary(mob)

## Convering DV to a numeric variable
mob$health_n <- as.numeric(mob$health_b)
mob$hap_n <- as.numeric(mob$hap)
mob$mob2 <- as.numeric(mob$moc)

## Ignore these statement
# mob$mob2[mob$moc=="up"]<-1
# mob$mob2[mob$moc=="st"]<-2
# mob$mob2[mob$moc=="do"]<-3


## Assign reference group
mob$moc <- factor(mob$moc, levels=c("st", "do", "up"))
mob$mari <- factor(mob$mari, levels=c("M", "N", "D"))
# mob$moc <- factor(mob$moc, levels=c("st", "do", "u"), ordered = TRUE)
# mob$moc <- relevel(mob$moc, ref='st')


## Average Marginal Effect (AME) test with the unadjusted model
model_01 <- glm(health_n ~ mob2, family = binomial(), data = mob)
model_00 <- glm(health_n ~ moc, family = binomial(), data = mob)

summary(model_00)  ## Gives the same results as SAS
# confint(model_00)
exp(cbind(OR=coef(model_00),confint(model_00)))  ## Gives the same results as SAS

plot(ggpredict(model_00, "moc [all]"), ci = FALSE, add.data = TRUE)

coef(model_00)["moc"]

margins(model_00)




###### Adjusted models 

# SELF-RATED HEALTH
model_05 <- glm(health_n ~ moc+sq1+aggrp+edui+hinc+rlgi+mari, family = binomial(), data = mob)
model_05 <- glm(health_n ~ moc+sq1+aggrp+edui+hinc+mari, family = binomial(), data = mob)
model_05 <- glm(health_n ~ moc+sq1+aggrp+edui+mari, family = binomial(), data = mob)
model_05 <- glm(health_n ~ moc+sq1+aggrp+mari, family = binomial(), data = mob)
model_05 <- glm(health_n ~ moc+sq1+aggrp, family = binomial(), data = mob)
model_05 <- glm(health_n ~ moc+sq1, family = binomial(), data = mob)
model_05 <- glm(health_n ~ moc+sq1+edui+hinc+rlgi+mari, family = binomial(), data = mob)

summary(model_05)  ## Gives the same results as SAS
exp(cbind(OR=coef(model_05),confint(model_05)))  ## Gives the same results as SAS

plot(ggpredict(model_00, "moc [all]"), ci = FALSE, add.data = TRUE)

margins(model_05)

## Summary function provides AME, SE, Z, P, CI - Divided into two groups
summary(margins(model_05, variables = c("moc", "sq1", "aggrp")))
summary(margins(model_05, variables = c("edui","hinc","rlgi","mari")))


# Happiness
model_15 <- glm(hap_n ~ moc+sq1+aggrp+edui+hinc+rlgi+mari, family = binomial(), data = mob)
model_15 <- glm(hap_n ~ moc+sq1+aggrp+edui+hinc+mari, family = binomial(), data = mob)
model_15 <- glm(hap_n ~ moc+sq1+aggrp+edui+mari, family = binomial(), data = mob)
model_15 <- glm(hap_n ~ moc+sq1+aggrp+mari, family = binomial(), data = mob)
model_15 <- glm(hap_n ~ moc+sq1+aggrp, family = binomial(), data = mob)
model_15 <- glm(hap_n ~ moc+sq1, family = binomial(), data = mob)
model_15 <- glm(hap_n ~ moc+sq1+edui+hinc+rlgi+mari, family = binomial(), data = mob)


summary(model_15)  ## Gives the same results as SAS
exp(cbind(OR=coef(model_15),confint(model_15)))  ## Gives the same results as SAS

plot(ggpredict(model_15, "moc [all]"), ci = FALSE, add.data = TRUE)

margins(model_15)

## Summary function provides AME, SE, Z, P, CI - Divided into two groups
summary(margins(model_15, variables = c("moc", "sq1", "aggrp")))
summary(margins(model_15, variables = c("edui","hinc","rlgi","mari")))


#### END OF THE TEST - 03/09/2023 ####



################ APPENDIX: Example for logistic regression with categorical variables
## https://strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html

smoking <- data.frame(
  sex = factor(c("male", "female", "female", "male", "female", "female",
                 "male", "female", "female", "male", "male", "female",
                 "female"), 
               levels = c("male", "female")),
  smoking = factor(c("no", "yes", "yes", "yes", "yes", "no", "no", "yes",
                     "yes", "no", "no", "no", "yes"), 
                   levels = c("no", "yes")),
  age = c(10, 45, 50, 40, 45, 12, 14, 55, 60, 10, 14, 50, 40)
)


100 * prop.table(table(smoking$sex, smoking$smoking), margin = 1)


model5 <- glm(smoking ~ sex, family = binomial(), data = smoking)

model_parameters(model5, exponentiate = TRUE)

ggpredict(model5, "sex")

margins(model5)


model6 <- glm(smoking ~ sex + age, family = binomial(), data = smoking)

