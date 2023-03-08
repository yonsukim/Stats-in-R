### Average Marginal Effect Model ###

 
## install.packages("readxl")
## library(readxl)

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


### Social mobility
mob <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Mobility\\Mobility_data\\mob_02.xlsx")

# View(mob)

# summary(mob)
mob$health_n <- as.numeric(mob$health_b)
mob$mob2 <- as.numeric(mob$moc)

mob$mob2[mob$moc=="up"]<-1
mob$mob2[mob$moc=="st"]<-2
mob$mob2[mob$moc=="do"]<-3



# Convert the "color" variable to an ordered factor
# mydata$color <- factor(mydata$color, levels = c("red", "green", "blue"), ordered = TRUE)

mob$moc <- factor(mob$moc, levels=c("st", "do", "up"))
# mob$moc <- factor(mob$moc, levels=c("st", "do", "u"), ordered = TRUE)
# mob$moc <- relevel(mob$moc, ref='st')

model_01 <- glm(health_n ~ mob2, family = binomial(), data = mob)
model_00 <- glm(health_n ~ moc, family = binomial(), data = mob)

summary(model_00)  ## Gives the same results as SAS
# confint(model_00)
exp(cbind(OR=coef(model_00),confint(model_00)))  ## Gives the same results as SAS

plot(ggpredict(model_00, "moc [all]"), ci = FALSE, add.data = TRUE)

coef(model_00)["moc"]

margins(model_00)


### Adjusted models

model_05 <- glm(health_n ~ moc+sq1+aggrp+edui+hinc+rlgi+mari, family = binomial(), data = mob)

summary(model_05)  ## Gives the same results as SAS
exp(cbind(OR=coef(model_05),confint(model_05)))  ## Gives the same results as SAS

plot(ggpredict(model_00, "moc [all]"), ci = FALSE, add.data = TRUE)

margins(model_05)






