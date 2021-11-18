# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)
library("tidyverse")


# Data Importation & Preperation ####

Total <- read_delim("Intervention_Data.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)




# Splitting Data into Postvention and Follow-up Subgroups 

Post <- subset(Total, Follow_up == 0)

Follow_up <- subset(Total, Follow_up == 1)

#Descriptive statistics over emerged data.####

#__Number of Outcomes 

N.outcome.P <- max(Post$Outcome_ID)
N.outcome.F <- max(Follow_up$Outcome_ID)

n.outcome.Total <- (N.outcome.P+N.outcome.F)

#__Amount of Thinking & Acting 

n.Thinking<- sum(Intervention_Data$Thinking)

n.Acting <-  (n.outcome.Total- n.Thinking)

#__Amount of Individual vs. Autonomous 

n.Autonomous<- sum(Intervention_Data$Intervention_Type)

n.Individual<- sum(n.outcome.Total -n.Autonomous )

#__Duration of Intervention

hist(Post$`Duration (weeks)`)
#Skewered -> use of median instead of mean.

Duration.weeksmedian<- median(Post$`Duration (weeks)`)

Low.Duration <- min(Post$`Duration (weeks)`)

Max.Duration <- max(Post$`Duration (weeks)`)

Duration.weeksmedian<- median(Post$`Duration (weeks)`)

#__ Duration Follow up

Follow_Up_Time <- (Post$`Duration (weeks)` -Follow_up$`Duration (weeks)`)

Low.Duration <- min(Post$`Duration (weeks)`)

Max.Duration <- max(Post$`Duration (weeks)`)

Duration.weeksmedian<- median(Post$`Duration (weeks)`)

##__ Follow_upTime 


Follow_upTimemedian <-  median(Follow_up$`Follow_up Time`)

highestF <- max(Follow_up$`Follow_up Time`)
lowestF <- min(Follow_up$`Follow_up Time`)

#Post-Intervention####

#Post-Intervention: Overall effectiveness 


Overall.Post <- rma.mv(g,v, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(Overall.Post)


#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.Post)

# RVE Correction

mfor_CR2 <- vcovCR(Overall.Post, type = "CR2")
Overall.PostRVE<- coef_test(Overall.Post, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.PostRVECI<- conf_int(Overall.Post, vcov = mfor_CR2)

# Estimating I^2 

n <- length(Post$v)
list.inverse.variances <- 1 / (Post$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Post$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall.Post$sigma2[1]
                                         + Overall.Post$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall.Post$sigma2[1]) / (Overall.Post$sigma2[1]
                                   + Overall.Post$sigma2[2] + estimated.sampling.variance)
I2_3 <- (Overall.Post$sigma2[2]) / (Overall.Post$sigma2[1]
                                   + Overall.Post$sigma2[2] + estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Study_VariancePost <- I2_2 * 100
Outcome_VariancePost <- I2_3 * 100


#Thoughts vs. Acts 

Overall.PostT <- rma.mv(g,v, mods= ~Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(Overall.PostT)

#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.PostT)

#RVE
mfor_CR2 <- vcovCR(Overall.PostT, type = "CR2")
Overall.PostTRVE <- coef_test(Overall.PostT, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.PostTRVECI<- conf_int(Overall.PostT, vcov = mfor_CR2)

# Effect size in Thinking outcome
Thinking <- (Overall.PostTRVE$beta[1] +Overall.PostTRVE$beta[2]) 

# Estimating I^2 

n <- length(Post$v)
list.inverse.variances <- 1 / (Post$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Post$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall.PostT$sigma2[1]
                                         + Overall.PostT$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall.PostT$sigma2[1]) / (Overall.PostT$sigma2[1]
                                    + Overall.PostT$sigma2[2] + estimated.sampling.variance)
I2_3 <- (Overall.PostT$sigma2[2]) / (Overall.PostT$sigma2[1]
                                    + Overall.PostT$sigma2[2] + estimated.sampling.variance)

Overall.PostT_Samplingvariance <- I2_1 * 100
Overall.PostT_Study_VariancePost <- I2_2 * 100
Overall.PostT_Outcome_VariancePost <- I2_3 * 100




# Control-group comparison

Overall.PostC <- rma.mv(g,v, mods= ~TAU, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(Overall.PostC)


#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.PostC)

# Robust Variance estimation 

mfor_CR2 <- vcovCR(Overall.PostC, type = "CR2")
Overall.PostCRVE<- coef_test(Overall.PostC, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.PostCRVECI <- conf_int(Overall.PostC, vcov = mfor_CR2)

# TAU group
TAU <- (Overall.PostCRVE$beta[1] +Overall.PostCRVE$beta[2]) 


#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.PostC)


# Estimating I^2 

n <- length(Post$v)
list.inverse.variances <- 1 / (Post$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Post$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall.PostC$sigma2[1]
                                         + Overall.PostC$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall.PostC$sigma2[1]) / (Overall.PostC$sigma2[1]
                                     + Overall.PostC$sigma2[2] + estimated.sampling.variance)
I2_3 <- (Overall.PostC$sigma2[2]) / (Overall.PostC$sigma2[1]
                                     + Overall.PostC$sigma2[2] + estimated.sampling.variance)

Overall.PostC_Samplingvariance <- I2_1 * 100
Overall.PostC_Study_VariancePost <- I2_2 * 100
Overall.PostC_Outcome_VariancePost <- I2_3 * 100




# Intervention Type

Overall.PostI <- rma.mv(g,v, mods= ~Intervention_Type, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(Overall.PostI)


#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.PostI)

#RVE Correction
mfor_CR2 <- vcovCR(Overall.PostI, type = "CR2")
Overall.PostIRVE<- coef_test(Overall.PostI, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.PostIRVECI <- conf_int(Overall.PostI, vcov = mfor_CR2)



#Follow-up####

#Follow_UP. Overall

Overall.Flowup <- rma.mv(g,v,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Follow_up, method="REML")
summary(Overall.Flowup)


#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.Flowup)

#RVE

mfor_CR2 <- vcovCR(Overall.Flowup, type = "CR2")
Overall.FlowupRVE <- coef_test(Overall.Flowup, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.FlowupRVECI <- conf_int(Overall.Flowup, vcov = mfor_CR2)


# I^2 Estimation

n <- length(Follow_up$v)
list.inverse.variances <- 1 / (Follow_up$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Follow_up$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall.Flowup$sigma2[1]
                                         + Overall.Flowup$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall.Flowup$sigma2[1]) / (Overall.Flowup$sigma2[1]
                                     + Overall.Flowup$sigma2[2] + estimated.sampling.variance)
I2_3 <- (Overall.Flowup$sigma2[2]) / (Overall.Flowup$sigma2[1]
                                     + Overall.Flowup$sigma2[2] + estimated.sampling.variance)

Overall.Flowup_Samplingvariance <- I2_1 * 100
Overall.Flowup_Study_VariancePost <- I2_2 * 100
Overall.Flowup_Outcome_VariancePost <- I2_3 * 100


#Decrease of Postvention to FOllow up

Decrease <- Overall.PostRVE$beta -Overall.FlowupRVE$beta 


# Thinking vs. Acting 

Overall.Flowup <- rma.mv(g,v, mods = ~Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Follow_up, method="REML")
summary(Overall.Flowup)

#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.FlowupTI)


#RVE

n <- length(Follow_up$v)
list.inverse.variances <- 1 / (Follow_up$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Follow_up$v^2)
sum.inverse.variances.square <-
  sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances -
  sum.inverse.variances.square
estimated.sampling.variance <- numerator / denominator
I2_1 <- (estimated.sampling.variance) / (Overall.Flowup$sigma2[1]
                                         + Overall.Flowup$sigma2[2] + estimated.sampling.variance)
I2_2 <- (Overall.Flowup$sigma2[1]) / (Overall.Flowup$sigma2[1]
                               + Overall.Flowup$sigma2[2] + estimated.sampling.variance)
I2_3 <- (Overall.Flowup$sigma2[2]) / (Overall.Flowup$sigma2[1]
                               + Overall.Flowup$sigma2[2] + estimated.sampling.variance)
amountvariancelevel1 <- I2_1 * 100
amountvariancelevel2 <- I2_2 * 100
amountvariancelevel3 <- I2_3 * 100
# Control-group comparison


Overall.FlowupT <- rma.mv(g,v, mod=~TAU, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Follow_up, method="REML")
summary(Overall.FlowupT)


#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.FlowupT)


#RVE

mfor_CR2 <- vcovCR(Overall.FlowupT, type = "CR2")
coef_test(Overall.FlowupT, vcov = mfor_CR2, test = ("Satterthwaite"))
conf_int(Overall.FlowupT, vcov = mfor_CR2)




# Intervention comparison 

Overall.FlowupI <- rma.mv(g,v, mod=~ Intervention_Type, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Follow_up, method="REML")
summary(Overall.FlowupI)


#RVE


wait.ES<- anova(Overall.FlowupW, L =c(1))
AttTAU.ES <- anova(Overall.FlowupW, L =c(0))


