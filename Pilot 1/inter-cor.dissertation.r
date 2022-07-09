#Remove participants with missing observations
I <- I %>% na.omit()

#Remove identifer row
#Create correlation matrix across all sets of items
tmp <- cor(I[,-1])

#Grab Integrated Regulation and Intrinsic Motivation items
IMIR <- tmp[grepl("IR" , rownames(tmp)) , grepl("IM" , colnames(tmp))]
#Calculate mean and sd of corr between Integrated Regulation and Intrinsic Items
mean(IMIR)
sd(IMIR)

#Grab Intrinsic Motivation and Pressure Items
IMERE <- tmp[grepl("IM" , rownames(tmp)) , grepl("ERE" , colnames(tmp))]
#Calculate mean and sd of corr between Intrinsic Motivation and Pressure
mean(IMERE)
sd(IMERE)

#Grab Intrinsic Motivation and Rewards Items
IMEPR <- IMIR <- tmp[grepl("IM" , rownames(tmp)) , grepl("EPR" , colnames(tmp))]
#Calculate mean and sd of corr between Intrinsic Motivation and Rewards Items
mean(IMEPR)
sd(IMEPR)

#Grab Intrinsic Motivation and Amotivation Items
IMAM <- IMIR <- tmp[grepl("IM" , rownames(tmp)) , grepl("AM" , colnames(tmp))]
#Calculate mean and sd of corr between Intrinsic Motivation and 
#Amotivation Items
mean(IMAM)
sd(IMAM)

#Grab Integrated Regulation and Rewards Items
IRERE <- IMIR <- tmp[grepl("IR" , rownames(tmp)) , grepl("ERE" , colnames(tmp))]
#Calculate mean and sd of corr between Integrated Regulation and Rewards Items
mean(IRERE)
sd(IRERE)

#Grab Integrated Regulation and Pressure Items
IREPR <- tmp[grepl("IR" , rownames(tmp)) , grepl("EPR" , colnames(tmp))]
#Calculate mean and sd of corr between Integrated Regulation and Pressure Items
mean(IREPR)
sd(IREPR)

#Grab Integrated Regulation and Amotivation Items
IRAM <- tmp[grepl("IR" , rownames(tmp)) , grepl("AM" , colnames(tmp))]
#Calculate mean and sd of corr between Integrated Regulation and Amotivation 
#Items 
mean(IRAM)
sd(IRAM)

#Grab Rewards and Pressure Items
EREEPR <- tmp[grepl("ERE" , rownames(tmp)) , grepl("EPR" , colnames(tmp))]
#Calculate mean and sd of corr between Rewards and Pressure Items
mean(EREEPR)
sd(EREEPR)

#Grab Rewards and Amotivation Items
EREAM <- tmp[grepl("ERE" , rownames(tmp)) , grepl("AM" , colnames(tmp))]
#Calculate mean and sd of corr between Rewards and Amotivation Items
mean(EREAM)
sd(EREAM)

#Grab Pressure and Amotivation Items
EPRAM <- tmp[grepl("EPR" , rownames(tmp)) , grepl("AM" , colnames(tmp))]
#Calculate mean and sd of corr between Pressure and Amotivation Items
mean(EPRAM)
sd(EPRAM)
