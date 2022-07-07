library(likert) 
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(lubridate)

#Remove unnecessary rows
Data2 <- Data2 %>%
  filter(!row_number() %in% c(1, 2))

#Filter responses by date administered
Clean.Data2 <- Data2 %>% filter(StartDate >= as.Date("2021-12-05"))

#Replace missing responses with NAs
Clean.Data2[Clean.Data2 ==""] <- NA


#Clean and check data to ensure they are part of population of interest
Clean.Data2 <- Clean.Data2 %>% drop_na(Q6)
Clean.Data2 <- Clean.Data2[Clean.Data2$Q6 == 1,]

#Remove all cols that do not pertain to item questions
#Remove all participants that did not answer in full
AData2 <- Clean.Data2[, -c(1:23,58:66)]  
AData2 <- AData2 %>% 
  na.omit()


# Questions by Construct 

#Create index of IM Questions
index.IM <- grepl('IM', colnames(AData2))
#Subset data to only include IM Questions 
IM.Q <- AData2[index.IM]
#Create index of IR Questions
index.IR <- grepl('IR', colnames(AData2))
#Subset data to only include IR Questions
IR.Q <- AData2[index.IR]
#Create index of External Regulation Questions
index.EM <- grepl('ER', colnames(AData2))
#Subset data into External Regulation Questions
EM.Q <- AData2[index.EM]
#Classify into seperate constructs of Reward and Pressure
EM.P <- EM.Q[,c(1,3,4,6)]
EM.R <- EM.Q[,c(2,5,7)]
#Create index of Amotivation Questions
index.AM <- grepl('AM', colnames(AData2))
#Subset data into Amotivation Questions
AM.Q <- AData2[index.AM]


#### Summary Measure #### 


IM.Q <- na.omit(IM.Q) #Remove missing participants
Intrinsic <- (t(IM.Q)) #Transpose data
Intrinsic <- data.frame(Intrinsic) #Create data frame

Intrinsic.Table <- sapply(Intrinsic, as.numeric) #Make all values numeric
Intrinsic.Table <- t(Intrinsic.Table) #transpose table
apply(Intrinsic.Table , 2 , fivenum) -7 #Create 5 number summary. - 7 to 
#correctly flip scale. 
abs(colMeans(Intrinsic.Table) - 7) #Calculated col means on correct scale


### Line Plot for IM 

IM.Q$Subject <- c(1:length(IM.Q$IM1)) # Create col ID
IM.Q$Subject <- factor(IM.Q$Subject) # Make ID a factor


#Assign correct values to Likert-Scale Response 
#Correctly order levels to appear on line plot
TDL <- melt(IM.Q, id.vars=c("Subject"))
TDL <- TDL %>%  
  mutate(value2 = case_when(
    value == 6 ~ "Strongly disagree",
    value == 5 ~ "Disagree",
    value == 4 ~ "Slightly disagree",
    value == 3 ~ "Slightly agree",
    value == 2 ~ "Agree",
    TRUE ~ "Strongly agree"
  )) %>%
  mutate(
    value3 = factor(
      value2,
      levels = c(
        "Strongly disagree", "Disagree", "Slightly disagree", "Slightly agree", "Agree", "Strongly agree"
      )
    )
  )

#ggplot that tracks participant responses by question faceted by participant
#labels enlarged and bold. Legend removed, as information is clear from facet + title

p <- ggplot(TDL, aes(x=factor(variable), y=value3, colour=Subject, group=Subject)) +   geom_line(size = 2) +
  facet_wrap( ~ Subject) +
  #scale_size_manual( values = c(5:1) ) +
  labs(title = "Intrinsic Motivation Items By Participant" , xlab = " " , ylab = " ") + 
  scale_y_discrete(drop=FALSE)
p + theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          text = element_text(size = 20),
          legend.position = "none",
          axis.text = element_text(face="bold"))  


#### Refer to documentation above for subsequent code
#### Same documentation and procedures for different sets of Items

####Integrated Regulation 
## Summary Statistics 

IR.Q$Subject <- c(1:length(IR.Q$IR1))
IR.Q$Subject <- factor(IR.Q$Subject)

IReg <- (t(IR.Q))
IReg <- data.frame(IReg) 

IR.Table <- sapply(IReg, as.numeric)
IR.Table <- t(IR.Table)
apply(IR.Table , 2 , fivenum) - 7
abs(colMeans(IR.Table) - 7)
sd(IR.Table[,1:11])

## Line Plot for Integrated Regulation Items

TDL <- melt(IR.Q, id.vars=c("Subject"))
TDL <- TDL %>%  
  mutate(value2 = case_when(
    value == 6 ~ "Strongly disagree",
    value == 5 ~ "Disagree",
    value == 4 ~ "Slightly disagree",
    value == 3 ~ "Slightly agree",
    value == 2 ~ "Agree",
    TRUE ~ "Strongly agree"
  )) %>%
  mutate(
    value3 = factor(
      value2,
      levels = c(
        "Strongly disagree", "Disagree", "Slightly disagree", "Slightly agree", "Agree", "Strongly agree"
      )
    )
  )

p <- ggplot(TDL, aes(x=factor(variable), y=value3, colour=Subject, group=Subject)) +   geom_line(size = 2) +
  facet_wrap( ~ Subject) +
  labs(title = "Integrated Regulation Items by Participant" , xlab = "")+ 
  scale_y_discrete(drop=FALSE)
p + theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          text = element_text(size = 20),
          legend.position = "none",
          axis.text = element_text(face="bold")) 



####Extrinsic Motivation - External Regulation
## Summary Statistics 

EM.Q$Subject <- c(1:length(EM.Q$ER1))
EM.Q$Subject <- factor(EM.Q$Subject)

EMReg <- (t(EM.Q))
EMReg <- data.frame(EMReg) 

EM.Table <- sapply(EMReg, as.numeric)
EM.Table <- t(EM.Table)
apply(EM.Table , 2 , fivenum) 
abs(colMeans(EM.Table) - 7) 

sd(EM.Table[,1:7])

####Extrinsic Motivation - External Regulation Line Plot

TDL <- melt(EM.Q, id.vars=c("Subject"))
TDL <- TDL %>%  
  mutate(value2 = case_when(
    value == 6 ~ "Strongly disagree",
    value == 5 ~ "Disagree",
    value == 4 ~ "Slightly disagree",
    value == 3 ~ "Slightly agree",
    value == 2 ~ "Agree",
    TRUE ~ "Strongly agree"
  )) %>%
  mutate(
    value3 = factor(
      value2,
      levels = c(
        "Strongly disagree", "Disagree", "Slightly disagree", "Slightly agree", "Agree", "Strongly agree"
      )
    )
  )

p <- ggplot(TDL, aes(x=factor(variable), y=value3, colour=Subject, group=Subject)) +   geom_line(size = 2) +
  facet_wrap( ~ Subject) +
  #scale_size_manual( values = c(5:1) ) +
  labs(title = "External Regulation Items by Participant" , xlab = "") + 
  scale_y_discrete(drop=FALSE)
p + theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          text = element_text(size = 20),
          legend.position = "none",
          axis.text = element_text(face="bold"))



### Make plots & summary statistics 
#by sub-groups of EM (Not Included In Dissertation Report)
#Pressure

EM.P$Subject <- c(1:length(EM.P$ER1))
EM.P$Subject <- factor(EM.P$Subject)

EMP <- (t(EM.P))
EMP <- data.frame(EMP) 

EMP.Table <- sapply(EMP, as.numeric)
EMP.Table <- t(EMP.Table)
apply(EMP.Table , 2 , fivenum) - 7
abs(colMeans(EMP.Table) - 7) 

TDL <- melt(EM.P, id.vars=c("Subject"))
TDL <- TDL %>%  
  mutate(value2 = case_when(
    value == 6 ~ "Strongly disagree",
    value == 5 ~ "Disagree",
    value == 4 ~ "Slightly disagree",
    value == 3 ~ "Slightly agree",
    value == 2 ~ "Agree",
    TRUE ~ "Strongly agree"
  )) %>%
  mutate(
    value3 = factor(
      value2,
      levels = c(
        "Strongly disagree", "Disagree", "Slightly disagree", "Slightly agree", "Agree", "Strongly agree"
      )
    )
  )

p <- ggplot(TDL, aes(x=factor(variable), y=value3, colour=Subject, group=Subject)) +   geom_line(size = 2) +
  facet_wrap( ~ Subject) +
  #scale_size_manual( values = c(5:1) ) +
  labs(title = "External Regulation Motivation Items [Pressure]" , xlab = "") + 
  scale_y_discrete(drop=FALSE)
p + theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Rewards 

EM.R$Subject <- c(1:length(EM.R$ER2))
EM.R$Subject <- factor(EM.R$Subject)

EMR <- (t(EM.R))
#Intrinsic$QN <- rep(1:length(Intrinsic$X1))
EMR <- data.frame(EMR) 

EMR.Table <- sapply(EMR, as.numeric)
EMR.Table <- t(EMR.Table)
apply(EMR.Table , 2 , fivenum) - 7
abs(colMeans(EMR.Table) - 7) 

TDL <- melt(EM.R, id.vars=c("Subject"))
TDL <- TDL %>%  
  mutate(value2 = case_when(
    value == 6 ~ "Strongly disagree",
    value == 5 ~ "Disagree",
    value == 4 ~ "Slightly disagree",
    value == 3 ~ "Slightly agree",
    value == 2 ~ "Agree",
    TRUE ~ "Strongly agree"
  )) %>%
  mutate(
    value3 = factor(
      value2,
      levels = c(
        "Strongly disagree", "Disagree", "Slightly disagree", "Slightly agree", "Agree", "Strongly agree"
      )
    )
  )

p <- ggplot(TDL, aes(x=factor(variable), y=value3, colour=Subject, group=Subject)) +   geom_line(size = 2) +
  facet_wrap( ~ Subject) +
  #scale_size_manual( values = c(5:1) ) +
  labs(title = "External Regulation Motivation Items [Rewards]" , xlab = "") + 
  scale_y_discrete(drop=FALSE)
p + theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


####Amotivation
## Summary Statistics 


AM.Q$Subject <- c(1:length(AM.Q$AM1))
AM.Q$Subject <- factor(AM.Q$Subject)

A <- (t(AM.Q))
A <- data.frame(A) 

A.Table <- sapply(A, as.numeric)
A.Table <- t(A.Table)
apply(A.Table , 2 , fivenum) - 7
abs(colMeans(A.Table) - 7)
sd(A.Table[,1:8])

##Amotivation Line Graphs

TDL <- melt(AM.Q, id.vars=c("Subject"))
TDL <- TDL %>%  
  mutate(value2 = case_when(
    value == 6 ~ "Strongly disagree",
    value == 5 ~ "Disagree",
    value == 4 ~ "Slightly disagree",
    value == 3 ~ "Slightly agree",
    value == 2 ~ "Agree",
    TRUE ~ "Strongly agree"
  )) %>%
  mutate(
    value3 = factor(
      value2,
      levels = c(
        "Strongly disagree", "Disagree", "Slightly disagree", "Slightly agree", "Agree", "Strongly agree"
      )
    )
  )

p <- ggplot(TDL, aes(x=factor(variable), y=value3, colour=Subject, group=Subject)) +   geom_line(size = 2) +
  facet_wrap( ~ Subject) +
  #scale_size_manual( values = c(5:1) ) +
  labs(title = "Amotivation Items by Participant" , xlab = "") + 
  scale_y_discrete(drop=FALSE)
p + theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          text = element_text(size = 20),
          legend.position = "none",
          axis.text = element_text(face="bold")) 


