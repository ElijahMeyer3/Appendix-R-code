library(ltm)
library(psych)
library(Rcsdp)
library(tidyr)
library(dplyr)
library(Cronbach)
#Cronbach's Alpha Calculations to quantify 
#evidence of reliability for each of our four constructs

set.seed(07032022)

index.IM <- grepl('IM', colnames(AData2))
IM.Q <- AData2[index.IM] %>%  
  na.omit(index.IM)

#Function to calculate Cronbach's Alpha for Intrinsic Motivation Items
cronbach.alpha(IM.Q)
cron.ci(IM.Q %>% as.matrix %>% apply(., 2, as.numeric), type = "boot")


index.IR <- grepl('IR', colnames(AData2))
IR.Q <- AData2[index.IR] %>%  
  na.omit(index.IR) 

cron.ci(IR.Q %>% as.matrix %>% apply(., 2, as.numeric), type = "boot")

#Function to calculate Cronbach's Alpha for Integrated Regulation Items
cronbach.alpha(IR.Q)


index.EM <- grepl('ER', colnames(AData2))
EM.Q <- AData2[index.EM] %>%  
  na.omit(index.EM) 

#Function to calculate Cronbach's Alpha for External Regulation Items
cronbach.alpha(EM.Q)
cron.ci(EM.Q %>% as.matrix %>% apply(., 2, as.numeric), type = "boot")

#### Exploring different groupings

#External Pressure 
### Questions 1, 3, 4, 6 

EM.P <- EM.Q[,c(1,3,4,6)]

#Function to calculate Cronbach's Alpha for Pressure Items
cronbach.alpha(EM.P)
cron.ci(EM.P %>% as.matrix %>% apply(., 2, as.numeric), type = "boot")

EM.R <- EM.Q[,c(2,5,7)]

#Function to calculate Cronbach's Alpha for Rewards Items
cronbach.alpha(EM.R)
cron.ci(EM.R %>% as.matrix %>% apply(., 2, as.numeric), type = "boot")


index.AM <- grepl('AM', colnames(AData2))
AM.Q <- AData2[index.AM] %>%  
  na.omit(index.AM)

#Function to calculate Cronbach's Alpha for Amotivation
cronbach.alpha(AM.Q)
cron.ci(AM.Q %>% as.matrix %>% apply(., 2, as.numeric), type = "boot")

