# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)
library("tidyverse")
# Data Importation & Preperation ####

Total <- read_delim("Intervention_DataD2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)



Total <- subset(Total, NSSI ==0 )

#Descriptive statistics over emerged data.####


Post <- subset(Total, Follow_up == 0 )

Post2 <- escalc(yi=d, vi=v, data=Post )


AggPost<- aggregate(Post2, cluster= Study_ID, struct = "CS", rho= 0.6)

sum(AggPost$n)


Follow <- subset(Total, Follow_up == 1 )

Follow2 <- escalc(yi=d, vi=v, data=Follow )


AggFollow<- aggregate(Follow2, cluster= Study_ID, struct = "CS", rho= 0.6)

sum(AggFollow$n)

#__Number of studies 

k.Studies <- max(Total$Study_ID)


#__Number of Outcomes 

n.outcome.Total <- max(Total$Outcome_ID)

#__Amount of Thinking & Acting 

n.Thinking<- sum(Total$Thinking)

n.Acting <-  (n.outcome.Total- n.Thinking)

#__Amount of Individual vs. Autonomous 

n.Autonomous<- sum(Total$Autonomous)

n.Individual<- sum(n.outcome.Total -n.Autonomous )

#__Duration of Intervention

hist(Total$`Duration (weeks)`)
#Skewered -> use of median instead of mean.

Duration.weeksmedian<- median(Total$`Duration (weeks)`)

Low.Duration <- min(Total$`Duration (weeks)`)

Max.Duration <- max(Total$`Duration (weeks)`)

Duration.weeksmedian<- median(Total$`Duration (weeks)`)

#__ Duration Follow up

Follow_Up_Time <- (mean(Total$`Duration (weeks)`) -mean(Total$`Duration (weeks)`))

Low.Duration <- min(Total$`Duration (weeks)`)

Max.Duration <- max(Total$`Duration (weeks)`)

Duration.weeksmedian<- median(Total$`Duration (weeks)`)

##__ Follow_upTime 

MedianFollowup<- median(Total$`Follow_up Time`, na.rm = TRUE)


highestF <- max(Total$`Follow_up Time`,  na.rm = TRUE)
lowestF <- min(Total$`Follow_up Time`,  na.rm = TRUE)


# total n 

n_mean_perStudy<- aggregate(x = Total$n,               
                            by = list(Total$Study_ID),              
                            FUN = mean)

n.total <- sum(n_mean_perStudy$x)


# Attrition Rates

Attrition.total <- median(Total$`Total Attrition (%)`, na.rm= TRUE) 

higestAttr. <- max(Total$`Total Attrition (%)`, na.rm=TRUE)

# Sample Characteristics 

Mean.Sex<- mean(Total$`Sex_(Female)`)

Mean.Age <- mean(Total$`Age_(mean)`, na.rm = TRUE)

min.Age <-  min(Total$`Age_(mean)`, na.rm = TRUE)

max.Age <-  max(Total$`Age_(mean)`, na.rm = TRUE)


#Main Analysis####

Overall <- rma.mv(d, v, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall)



#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall)

# RVE Correction

mfor_CR2 <- vcovCR(Overall, type = "CR2")
OverallRVE<- coef_test(Overall, vcov = mfor_CR2, test = ("Satterthwaite"))
OverallRVECI<- conf_int(Overall, vcov = mfor_CR2)

# Estimating I^2 

n <- length(Total$v)
list.inverse.variances <- 1 / (Total$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Total$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall$sigma2[1]
                                         + Overall$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall$sigma2[1]) / (Overall$sigma2[1]
                                   + Overall$sigma2[2] + estimated.sampling.variance)
I2_3 <- (Overall$sigma2[2]) / (Overall$sigma2[1]
                                   + Overall$sigma2[2] + estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Study_VariancePost <- I2_2 * 100
Outcome_VariancePost <- I2_3 * 100


#Thoughts vs. Acts 

Overall.T <- rma.mv(d ~ Thinking, V =v, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.T)

#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.T)

#RVE
mfor_CR2 <- vcovCR(Overall.T, type = "CR2")
Overall.TRVE <- coef_test(Overall.T, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TRVECI<- conf_int(Overall.T, vcov = mfor_CR2)



Eff.Thinking <- Overall.TRVECI$beta[1] + Overall.TRVECI$beta[2]

Eff.Acting <- Overall.TRVECI$beta[1] - Overall.TRVECI$beta[2]

# Estimating I^2 

n <- length(Total$v)
list.inverse.variances <- 1 / (Total$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Total$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall.T$sigma2[1]
                                         + Overall.T$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall.T$sigma2[1]) / (Overall.T$sigma2[1]
                               + Overall.T$sigma2[2] +Overall.T$sigma2[3]+ estimated.sampling.variance)
I2_3 <- (Overall.T$sigma2[2]) / (Overall.T$sigma2[1]
                               + Overall.T$sigma2[2]+Overall.T$sigma2[3] + estimated.sampling.variance)
I2_4 <- (Overall.T$sigma2[3]) / (Overall.T$sigma2[1]
                                 + Overall.T$sigma2[2]+Overall.T$sigma2[3] + estimated.sampling.variance)


SamplingvarianceT <- I2_1 * 100
Study_VariancePostT <- I2_2 * 100
Outcome_VariancePostT <- I2_3 * 100



# Control-group comparison


OverallC <- rma.mv(d,v, mods =~TAU, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(OverallC)



#__ Optimization Control
par(mfrow=c(2,4))
profile(OverallC)

# Robust Variance estimation 

mfor_CR2 <- vcovCR(OverallC, type = "CR2")
Overall.CRVE<- coef_test(OverallC, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.CRVECI <- conf_int(OverallC, vcov = mfor_CR2)

Eff.TAU <- Overall.CRVECI$beta[1] + Overall.CRVECI$beta[2]

Eff.WaitAtt <- Overall.CRVECI$beta[1] - Overall.CRVECI$beta[2]


# TAU group
TAU <- (Overall.CRVE$beta[1] +Overall.CRVE$beta[2]) 



# Estimating I^2 

n <- length(Total$v)
list.inverse.variances <- 1 / (Total$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Total$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (OverallC$sigma2[1]
                                         + OverallC$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (OverallC$sigma2[1]) / (OverallC$sigma2[1]
                                     + OverallC$sigma2[2] + estimated.sampling.variance)
I2_3 <- (OverallC$sigma2[2]) / (OverallC$sigma2[1]
                                     + OverallC$sigma2[2] + estimated.sampling.variance)

Overall.PostC_Samplingvariance <- I2_1 * 100
Overall.PostC_Study_VariancePost <- I2_2 * 100
Overall.PostC_Outcome_VariancePost <- I2_3 * 100




# Intervention Type

Overall.I <- rma.mv(d,v, mods= ~Autonomous, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.I)




#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.I)

#RVE Correction
mfor_CR2 <- vcovCR(Overall.I, type = "CR2")
Overall.IRVE<- coef_test(Overall.I, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.IRVECI <- conf_int(Overall.I, vcov = mfor_CR2)


Eff.Auto <- Overall.IRVECI$beta[1] + Overall.IRVECI$beta[2]

Eff.Human <- Overall.IRVECI$beta[1] - Overall.IRVECI$beta[2]



n <- length(Total$v)
list.inverse.variances <- 1 / (Total$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Total$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall.I$sigma2[1]
                                         + Overall.I$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall.I$sigma2[1]) / (Overall.I$sigma2[1]
                                + Overall.I$sigma2[2] + estimated.sampling.variance)
I2_3 <- (Overall.I$sigma2[2]) / (Overall.I$sigma2[1]
                                + Overall.I$sigma2[2] + estimated.sampling.variance)

Overall.PostC_Samplingvariance <- I2_1 * 100
Overall.PostC_Study_VariancePost <- I2_2 * 100
Overall.PostC_Outcome_VariancePost <- I2_3 * 100

# Time 

OverallF <- rma.mv(d,v, mods= ~Follow_up, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(OverallF)



#__ Optimization Control
#par(mfrow=c(2,4))
#profile(OverallF)

# Robust Variance estimation 

mfor_CR2 <- vcovCR(OverallF, type = "CR2")
Overall.FCRVE<- coef_test(OverallF, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.FCRVECI <- conf_int(OverallF, vcov = mfor_CR2)

Eff.Follow <- Overall.FCRVECI$beta[1] + Overall.FCRVECI$beta[2]

Eff.Post <- Overall.FCRVECI$beta[1] - Overall.FCRVECI$beta[2]


n <- length(Total$v)
list.inverse.variances <- 1 / (Total$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Total$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (OverallF$sigma2[1]
                                         + OverallF$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (OverallF$sigma2[1]) / (OverallF$sigma2[1]
                                 + OverallF$sigma2[2] + estimated.sampling.variance)
I2_3 <- (OverallF$sigma2[2]) / (OverallF$sigma2[1]
                                 + OverallF$sigma2[2] + estimated.sampling.variance)

OverallF_Samplingvariance <- I2_1 * 100
OverallF_Study_VariancePost <- I2_2 * 100
OverallF_Outcome_VariancePost <- I2_3 * 100



# Sensitivity Analysis####

# NSSI Included 
Total2 <- read_delim("Intervention_DataD2.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)

Overall.T2 <- rma.mv(d ~ Thinking, V =v, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total2, method="REML")
summary(Overall.T2)


mfor_CR2 <- vcovCR(Overall.T2, type = "CR2")
Overall.TRVE2<- coef_test(Overall.T2, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TRVECI2<- conf_int(Overall.T2, vcov = mfor_CR2)

# Excluding Suicide

Excl.Suicide <- subset(Total, Suicide == 0)

Overall.TS <- rma.mv(d,v, mods= ~Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Excl.Suicide, method="REML")
summary(Overall.TS)

#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.TS)

#RVE
mfor_CR2 <- vcovCR(Overall.TS, type = "CR2")
Overall.TSRVE <- coef_test(Overall.TS, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TSRVECI<- conf_int(Overall.TS, vcov = mfor_CR2)


# Estimating I^2 

n <- length(Total$v)
list.inverse.variances <- 1 / (Total$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Total$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall.T$sigma2[1]
                                         + Overall.T$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall.T$sigma2[1]) / (Overall.T$sigma2[1]
                                 + Overall.T$sigma2[2] + estimated.sampling.variance)
I2_3 <- (Overall.T$sigma2[2]) / (Overall.T$sigma2[1]
                                 + Overall.T$sigma2[2] + estimated.sampling.variance)

SamplingvarianceT <- I2_1 * 100
Study_VariancePostT <- I2_2 * 100
Outcome_VariancePostT <- I2_3 * 100

# No NSSI

Excl.NSSI <- subset(Total, NSSI == 0)

Overall.TN <- rma.mv(d,v, mods= ~Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Excl.NSSI, method="REML")
summary(Overall.TN)

#Thinking + TAU#####


Overall.TT <- rma.mv(d ~ Thinking+TAU, V =v, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.TT)


#__ Optimization Control
par(mfrow=c(2,4))
profile(Overall.TT)

#RVE
mfor_CR2 <- vcovCR(Overall.TT, type = "CR2")
Overall.TRVE <- coef_test(Overall.TT, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TRVECITT<- conf_int(Overall.TT, vcov = mfor_CR2)

Eff.Thinking <- Overall.TRVECI$beta[1] + Overall.TRVECI$beta[2]

Eff.Acting <- Overall.TRVECI$beta[1] - Overall.TRVECI$beta[2]

# Estimating I^2 

n <- length(Total$v)
list.inverse.variances <- 1 / (Total$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (Total$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (Overall.T$sigma2[1]
                                         + Overall.T$sigma2[2]+ estimated.sampling.variance)
I2_2 <- (Overall.T$sigma2[1]) / (Overall.T$sigma2[1]
                                 + Overall.T$sigma2[2] +Overall.T$sigma2[3]+ estimated.sampling.variance)
I2_3 <- (Overall.T$sigma2[2]) / (Overall.T$sigma2[1]
                                 + Overall.T$sigma2[2]+Overall.T$sigma2[3] + estimated.sampling.variance)
I2_4 <- (Overall.T$sigma2[3]) / (Overall.T$sigma2[1]
                                 + Overall.T$sigma2[2]+Overall.T$sigma2[3] + estimated.sampling.variance)


SamplingvarianceT <- I2_1 * 100
Study_VariancePostT <- I2_2 * 100
Outcome_VariancePostT <- I2_3 * 100




#plotting 

#pch

nrow(Total$TAU)

for (i in 1:nrow(Total)) {
  TAU       <- Total$TAU
  Thinking  <- Total$Thinking
  
  if(TAU[i] ==1 && Thinking[i] == 1) {
    Total$pch[i] <- 1}
  else if (TAU[i] ==0 && Thinking[i] == 1) {
    Total$pch[i] <- 5}
  else if (TAU[i] == 0 && Thinking[i] == 0) {
    Total$pch[i] <- 18}
  else if (TAU[i] ==1 && Thinking[i] == 0) {
    Total$pch[i] <- 20}
  
  }


#legend

funnel(Overall, pch =Total$pch, legend= TRUE, xlab = "Cohens d")

gendA<- legend(x = "topright",
               legend= c("TAU+Thinking","TAU+Acting","Non-TAU+Thinking", "Non-TAU+Acting"),
               pch= c(1,20,5,18))

?funnel
