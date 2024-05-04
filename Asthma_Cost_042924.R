### Asthma Cost 04/29/24 ###



##################### Packages to be installed #####################

install.packages("readxl")
library(readxl)

install.packages("broom")
library(broom)

install.packages("ggeffects")
library(ggeffects)

install.packages("tidyverse")
library(tidyverse)
update.packages("tidyverse")


install.packages("margins")
library(margins)
data(mtcars)

#####################################################################



library(ggplot2) 
library(dplyr) 
tips <- read.csv("C:\\Users\\kimy89\\Dropbox\\Research\\Github\\Stats-in-R\\tips.csv")

##################### Cost Data ##########################################


#####################    IP     ##################################
IP <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Costs\\Results\\R_Cost.xlsx", sheet="IP") 


## Example 
qplot(x = total_bill, y = tip, facets = ~sex, data = tips) +   geom_smooth(method = "lm")
qplot(x = total_bill, y = tip, data = tips, color = sex) +  geom_smooth(method = "lm") 


## Cost
IP2 <- IP[IP$Race == "3-Black" | IP$Race == "4-White" , ]
IP3 <- IP2[IP2$Sev == 1 | IP2$Sev == "4" ,]
#IP3 <- IP2[IP2$Sev != 0 ,]
#IP4 <- IP3[IP3$Tcost <= 50000,]
View(IP4)

qplot(x = Sev, y = Tcost, facets = ~Race, data = IP3) +   geom_smooth(method = "lm")
qplot(x = Sev, y = Tcost, data = IP3, color = Race) +  geom_smooth(method = "lm") 


#ggplot(tips) +  
#aes(x = total_bill, y = tip, color = sex) +   geom_point(color = "grey") +   geom_smooth(method = "lm", se=F )

ggplot(IP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +   geom_smooth(method = "lm", se=F )
 #aes(x = total_bill, y = tip, color = sex) +   geom_point(color = "grey") +   geom_smooth(method = "lm")


# BEST 
## Delete se=FALSE to include CI band

ggplot(IP3) +  
    aes(x = Sev, y = Tcost, color = Race) +   geom_smooth(aes(linetype = Race), method = "lm",  se = FALSE) + xlab("Asthma Severity") + ylab("Direct Cost") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c(" Intermittent", "", "", "Severe")) +
  scale_y_continuous(breaks = c(40000, 50000, 60000, 70000), labels = c("$40,000", "$50,000", "$60,000", "$70,000")) +
  scale_color_manual(values = c("red4", "blue4"))  +
  scale_linetype_manual(values = c("solid", "dotted")) +
  ggtitle("IP") +theme(plot.title = element_text(hjust=0.5)) +
  #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) 



#####################    ED     ##################################
ED <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Costs\\Results\\R_Cost.xlsx", sheet="ED") 

## Cost
ED2 <- ED[ED$Race == "3-Black" | ED$Race == "4-White" , ]
ED3 <- ED2[ED2$Sev == 1 | ED2$Sev == "4" ,]

qplot(x = Sev, y = Tcost, facets = ~Race, data = ED3) +   geom_smooth(method = "lm")
qplot(x = Sev, y = Tcost, data = ED3, color = Race) +  geom_smooth(method = "lm") 

ggplot(ED3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +   geom_smooth(method = "lm")

# BEST 
ggplot(ED3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_smooth(method = "lm", se = FALSE) + xlab("Asthma Severity") + ylab("Direct Cost") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("Intermittent", "", "", "Severe")) +
  scale_y_continuous(breaks = c(5000, 7500, 10000, 125000, 15000), labels = c("$5,000", "$75,000", "$10,000", "$12,500", "$15,000")) +
  scale_color_manual(values = c("red", "blue"))  +
  ggtitle("ED") +theme(plot.title = element_text(hjust=0.5)) +
  #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())


#####################    OP     ##################################
OP <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Costs\\Results\\R_Cost.xlsx", sheet="OP") 

## Cost
OP2 <- OP[OP$Race == "5-Hispanic" | OP$Race == "4-White" , ]
OP3 <- OP2[OP2$Sev == 1 | OP2$Sev == "4" ,]

qplot(x = Sev, y = Tcost, facets = ~Race, data = OP3) +   geom_smooth(method = "lm")
qplot(x = Sev, y = Tcost, data = OP3, color = Race) +  geom_smooth(method = "lm") 

ggplot(OP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +   geom_smooth(method = "lm")

# BEST 
ggplot(OP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_smooth(aes(linetype = Race), method = "lm", se = FALSE ) + xlab("Severity") + ylab("Cost") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("Intermittent", "", "", "Severe")) +
  scale_y_continuous(breaks = c(5000, 7500, 10000, 125000, 15000), labels = c("$5,000", "$75,000", "$10,000", "$12,500", "$15,000")) +
  scale_color_manual(values = c("blue4", "green4"))  +
  scale_linetype_manual(values = c("dotted", "longdash")) +
  ggtitle("OP") +theme(plot.title = element_text(hjust=0.5)) +
    #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())



#### END OF THE TEST - 04/29/2024 ####



#####################    IP with 3 races    ##################################
## Hispanic not sig.

IP <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Costs\\Results\\R_Cost.xlsx", sheet="IP") 

## Cost
IP2 <- IP[IP$Race == "3-Black" | IP$Race == "5-Hispanic" |IP$Race == "4-White" , ]
IP3 <- IP2[IP2$Sev == 1 | IP2$Sev == "4" ,]

qplot(x = Sev, y = Tcost, facets = ~Race, data = IP3) +   geom_smooth(method = "lm")
qplot(x = Sev, y = Tcost, data = IP3, color = Race) +  geom_smooth(method = "lm") 

ggplot(tips) +  
  aes(x = total_bill, y = tip, color = sex) +   geom_point(color = "grey") +   geom_smooth(method = "lm")

ggplot(IP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +   geom_smooth(method = "lm")


ggplot(IP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_smooth(method = "lm") + xlab("Severity") + ylab("Cost") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("Intermittent", "Mild", "Moderate", "Severe")) +
  #scale_y_continuous(breaks = c(40000, 50000, 60000, 70000), labels = c("$40,000", "$50,000", "$60,000", "$70,000")) +
  scale_color_manual(values = c("red", "blue"))  +
  #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())


#####################    ED  with 3 races      ##################################
ED <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Costs\\Results\\R_Cost.xlsx", sheet="ED") 

## Cost
ED2 <- ED[ED$Race == "3-Black" | ED$Race == "4-White" | ED$Race == "5-Hispanic" , ]
ED3 <- ED2[ED2$Sev == 1 | ED2$Sev == "4" ,]

qplot(x = Sev, y = Tcost, facets = ~Race, data = ED3) +   geom_smooth(method = "lm")
qplot(x = Sev, y = Tcost, data = ED3, color = Race) +  geom_smooth(method = "lm") 

ggplot(ED3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +   geom_smooth(method = "lm")

ggplot(ED3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_smooth(aes(linetype = Race), method = "lm", se= FALSE ) + xlab("Asthma Severity") + ylab("Direct Cost") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("Intermittent", "", "", "Severe")) +
  scale_y_continuous(breaks = c(6000, 8000, 10000, 12000, 14000), labels = c("$6,000", "$8,000", "$10,000", "$12,000", "$14,000")) +
  scale_color_manual(values = c("red4","blue4", "green4"))  +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  ggtitle("ED") +theme(plot.title = element_text(hjust=0.5)) +
    #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())




#####################    OP  with 3 races    ##################################
### Black not sig.
OP <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Costs\\Results\\R_Cost.xlsx", sheet="OP") 

## Cost
OP2 <- OP[OP$Race == "3-Black" | OP$Race == "5-Hispanic" | OP$Race == "4-White" , ]
OP3 <- OP2[OP2$Sev == 1 | OP2$Sev == "4" ,]


qplot(x = Sev, y = Tcost, facets = ~Race, data = OP3) +   geom_smooth(method = "lm")
qplot(x = Sev, y = Tcost, data = OP3, color = Race) +  geom_smooth(method = "lm") 

ggplot(OP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +   geom_smooth(method = "lm")

# BEST 
ggplot(OP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_smooth(method = "lm", se = FALSE) + xlab("Severity") + ylab("Cost") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("Intermittent", "Mild", "Moderate", "Severe")) +
  scale_y_continuous(breaks = c(5000, 7500, 10000, 125000, 15000), labels = c("$5,000", "$75,000", "$10,000", "$12,500", "$15,000")) +
  scale_color_manual(values = c("blue", "red"))  +
  #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())




#####################    ALL     ##################################
IP <- read_excel("C:\\Users\\kimy89\\Dropbox\\Research\\Asthma_Costs\\Results\\R_Cost.xlsx", sheet="IP") 

## Cost
IP2 <- IP[IP$Race == "3-Black" | IP$Race == "4-White" , ]
IP3 <- IP2[OP2$Sev == 1 | IP2$Sev == "4" ,]
View(IP3)

qplot(x = Sev, y = Tcost, facets = ~Race, data = OP3) +   geom_smooth(method = "lm")
qplot(x = Sev, y = Tcost, data = OP3, color = Race) +  geom_smooth(method = "lm") 

ggplot(OP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +   geom_smooth(method = "lm")

# BEST 
ggplot(OP3) +  
  aes(x = Sev, y = Tcost, color = Race) +   geom_smooth(method = "lm") + xlab("Severity") + ylab("Cost") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("Intermittent", "Mild", "Moderate", "Severe")) +
  scale_y_continuous(breaks = c(5000, 7500, 10000, 125000, 15000), labels = c("$5,000", "$75,000", "$10,000", "$12,500", "$15,000")) +
  scale_color_manual(values = c("red", "blue"))  +
  #Removing grid  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())











tips_f <- filter(tips, sex == "Female")
tips_m <- filter(tips, sex == "Male")


IP_w <- filter(IP4, Race == "4-White")
IP_b <- filter(IP4, Race == "3-Black")


ggplot(tips) +
  aes(x = total_bill, y = tip, color = sex) +   geom_point(color = "grey") +
  geom_smooth(method = "lm", data = tips_f) +   geom_smooth(method = "lm", data = tips_m)


ggplot(IP4) +
  aes(x = Sev, y = Tcost, color = Race) +
  geom_smooth(method = "lm", data = IP4) +   geom_smooth(method = "lm", data = IP_b) 


ggplot(IP4) +
  aes(x = Sev, y = Tcost, color = Race) +   geom_point(color = "grey") +
  geom_smooth(method = "lm", data = IP4) +   geom_smooth(method = "lm", data = IP_b) +
  geom_point() +  ylim(1000, 50000) 






ggplot(tips) +
  aes(x = sex, y = tip) +
  geom_boxplot() +
  facet_wrap(~smoker)

ggplot(IP4) +
  aes(x = Race, y = Tcost) +
  geom_boxplot() +
  facet_wrap(~smoker)



tips %>% 
  group_by(sex, smoker) %>% 
  summarise(tip_groups = mean(tip)) -> tips2


tips2 %>% 
  ggplot() +
  aes(x = sex, y = tip_groups, color = smoker) +
  geom_line(aes(group = smoker)) +
  geom_point()













  
