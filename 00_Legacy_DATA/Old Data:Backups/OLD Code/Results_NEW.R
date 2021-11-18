# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)


# Data Importation

TOSC <- read_delim("App-Paper/Für R/TOSC.cut.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

#Hypothesis I #####

#Five Level

FiveLevel <- rma.mv(g,v,  random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)

# Four Level No Time 
FourLevel.T <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T)

predict(FourLevel.T)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.T)

# RVE Correction
RVE<- coef_test(FourLevel.T,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)

#_Heterogeinty Investigation#####

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (FourLevel.T$sigma2[1]
                                         + FourLevel.T$sigma2[2] +FourLevel.T$sigma2[3]+ estimated.sampling.variance)
I2_2 <- (FourLevel.T$sigma2[1]) / (FourLevel.T$sigma2[1]
                                   + FourLevel.T$sigma2[2] +FourLevel.T$sigma2[3]+ estimated.sampling.variance)
I2_3 <- (FourLevel.T$sigma2[2]) / (FourLevel.T$sigma2[1]
                                   + FourLevel.T$sigma2[2] +FourLevel.T$sigma2[3]+ estimated.sampling.variance)
I2_4 <- (FourLevel.T$sigma2[3]) / (FourLevel.T$sigma2[1]
                                   + FourLevel.T$sigma2[2] +FourLevel.T$sigma2[3]+ estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Study_Variance <- I2_3 * 100
Outcome_Variance <- I2_4 * 100

### Outcome

Samplingvariance 
Control_group_Variance 
Study_Variance 
Outcome_Variance 


#Hypothesis II #####


# Five Level
FiveLevel.T <- rma.mv(g,v, mods = ~Follow_up,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel.T)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FiveLevel.T)

# Four Level - No Time ID

FourLevel.T <- rma.mv(g,v, mods = ~Follow_up,random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T)


# Three Level - No Time & Study ID

ThreeLevel.TS <- rma.mv(g,v, mods = ~Follow_up,random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.TS)

# Parameter evaluation
par(mfrow=c(2,4))
profile(ThreeLevel.TS)

# RVE Correction

RVE<- coef_test(ThreeLevel.TS ,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)


#_Heterogeinty Investigation#####

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (ThreeLevel.TS$sigma2[1]
                                         + ThreeLevel.TS$sigma2[2] + estimated.sampling.variance)
I2_2 <- (ThreeLevel.TS$sigma2[1]) / (ThreeLevel.TS$sigma2[1]
                                   + ThreeLevel.TS$sigma2[2] + estimated.sampling.variance)
I2_3 <- (ThreeLevel.TS$sigma2[2]) / (ThreeLevel.TS$sigma2[1]
                                   + ThreeLevel.TS$sigma2[2] + estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Outcome_Variance <- I2_3 * 100


### Outcome

Samplingvariance 
Control_group_Variance 
Outcome_Variance 


#Hypothesis III######

FourLevel <- rma.mv(g,v, mods= ~wait +Attention ,random= ~1 |Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel)

ThreeLevel <- rma.mv(g,v, mods= ~wait +Attention ,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

# Parameter evaluation
par(mfrow=c(2,4))
profile(ThreeLevel)

# RVE Correction
RVE<- coef_test(ThreeLevel ,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)





#_Heterogeinty Investigation #####

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (ThreeLevel$sigma2[1]
                                         + ThreeLevel$sigma2[2] + estimated.sampling.variance)
I2_2 <- (ThreeLevel$sigma2[1]) / (ThreeLevel$sigma2[1]
                                     + ThreeLevel$sigma2[2] + estimated.sampling.variance)
I2_3 <- (ThreeLevel$sigma2[2]) / (ThreeLevel$sigma2[1]
                                     + ThreeLevel$sigma2[2] + estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Study_Variance <- I2_2 * 100
Outcome_Variance <- I2_3 * 100


### Outcome

Samplingvariance 
Study_Variance 
Outcome_Variance 


#Hypothesis IV######

FiveLevel<- rma.mv(g,v, mods = ~Intervention_Type ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)


# Four Level - Time 

FourLevel <- rma.mv(g,v, mods = ~Intervention_Type ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel)


RVE<- coef_test(FourLevel, vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)


#_Heterogeinty Investigation #####

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (FourLevel$sigma2[1]
                                         + FourLevel$sigma2[2] +FourLevel$sigma2[3]+ estimated.sampling.variance)
I2_2 <- (FourLevel$sigma2[1]) / (FourLevel$sigma2[1]
                                   + FourLevel$sigma2[2] +FourLevel$sigma2[3]+ estimated.sampling.variance)
I2_3 <- (FourLevel$sigma2[2]) / (FourLevel$sigma2[1]
                                   + FourLevel$sigma2[2] +FourLevel$sigma2[3]+ estimated.sampling.variance)
I2_4 <- (FourLevel$sigma2[3]) / (FourLevel$sigma2[1]
                                   + FourLevel$sigma2[2] +FourLevel$sigma2[3]+ estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Study_Variance <- I2_3 * 100
Outcome_Variance <- I2_4 * 100




### Outcome

Samplingvariance 
Control_group_Variance 
Study_Variance
Outcome_Variance 


#Hypothesis V #######

FiveLevel.Thinking <- rma.mv(g,v, mods = ~Thinking ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel.Thinking)


FourLevel.Thinking <- rma.mv(g,v,mods = ~Thinking ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Thinking)

ThreeLevel <- rma.mv(g,v, mods = ~Thinking  ,random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)


par(mfrow=c(2,4))
profile(ThreeLevel)

RVE<- coef_test(ThreeLevel,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)


n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (ThreeLevel$sigma2[1]
                                         + ThreeLevel$sigma2[2] + estimated.sampling.variance)
I2_2 <- (ThreeLevel$sigma2[1]) / (ThreeLevel$sigma2[1]
                                  + ThreeLevel$sigma2[2] + estimated.sampling.variance)
I2_3 <- (ThreeLevel$sigma2[2]) / (ThreeLevel$sigma2[1]
                                  + ThreeLevel$sigma2[2] + estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Outcome_Variance <- I2_3 * 100


### Outcome

Samplingvariance 
Control_group_Variance 
Outcome_Variance 



#SEX Exploratory Analysis####

#_Hypothesis I####
FourLevel <- rma.mv(g,v, mod= ~ `Sex_(Female)`,  random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.T)

# RVE Correction
RVE<- coef_test(FourLevel,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)

#_Heterogeinty Investigation#####

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (FourLevel$sigma2[1]
                                         + FourLevel$sigma2[2] +FourLevel$sigma2[3]+ estimated.sampling.variance)
I2_2 <- (FourLevel$sigma2[1]) / (FourLevel$sigma2[1]
                                   + FourLevel$sigma2[2] +FourLevel$sigma2[3]+ estimated.sampling.variance)
I2_3 <- (FourLevel.T$sigma2[2]) / (FourLevel$sigma2[1]
                                   + FourLevel$sigma2[2] +FourLevel$sigma2[3]+ estimated.sampling.variance)
I2_4 <- (FourLevel.T$sigma2[3]) / (FourLevel$sigma2[1]
                                   + FourLevel$sigma2[2] +FourLevel$sigma2[3]+ estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Study_Variance <- I2_3 * 100
Outcome_Variance <- I2_4 * 100

### Outcome

Samplingvariance 
Control_group_Variance 
Study_Variance 
Outcome_Variance 




#_Hypothesis II####
ThreeLevel.TS <- rma.mv(g,v, mods = ~Follow_up +`Sex_(Female)`,random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.TS)

# Parameter evaluation
par(mfrow=c(2,4))
profile(ThreeLevel.TS)

# RVE Correction
RVE<- coef_test(ThreeLevel.TS,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)



# Hypothesis III
ThreeLevel <- rma.mv(g,v, mods= ~wait +Attention +`Sex_(Female)`,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

# Hypothesis IV

FourLevel <- rma.mv(g,v, mods = ~Intervention_Type+`Sex_(Female)` ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel)

# Hypothesis V

ThreeLevel <- rma.mv(g,v, mods = ~Thinking +`Sex_(Female)`  ,random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

#Subset Analysis#####
# Subsetting all sig. Moderators in subset to inspect Heterogeinty 

# A Thinking vs. Acting 

FiveLevel <- rma.mv(g,v, subset = (Thinking ==1 ), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)



FourLevel <- rma.mv(g,v, subset = (Thinking ==1 )  ,random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel)


ThreeLevel <- rma.mv(g,v, subset = (Thinking ==1 )  ,random= ~1 |ControlGroup_ID/Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)


par(mfrow=c(2,4))
profile(ThreeLevel)

# + Sex

FourLevel <- rma.mv(g,v, mod= ~ `Sex_(Female)`, subset = (Thinking ==1 )  ,random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel)

predict.rma(FourLevel )

?predict.rma

par(mfrow=c(2,4))
profile(FourLevel)

# RVE Correction
RVE<- coef_test(FourLevel,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)



# + Follow up ~ Baseline

ThreeLevel <- rma.mv(g,v, mod= ~Follow_up,random= ~1 |ControlGroup_ID/Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

par(mfrow=c(2,4))
profile(ThreeLevel)
# no problem

# + W/A 

ThreeLevel <- rma.mv(g,v, mod= ~wait + Attention, subset = (Thinking ==1 )  ,random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

par(mfrow=c(2,4))
profile(ThreeLevel)
# Problem


# Exploratory SEX

ThreeLevel <- rma.mv(g,v, mod= ~ `Sex_(Female)`, subset = (Thinking ==1 )  ,random= ~1 |ControlGroup_ID/Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

par(mfrow=c(2,4))
profile(ThreeLevel)
# NO Problem

# Acting

FiveLevel <- rma.mv(g,v, subset = (Thinking ==0)  ,random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)

FourLevel <- rma.mv(g,v, subset = (Thinking ==0)  ,random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel)


TwoLevel <- rma.mv(g,v, mod= ~`Sex_(Female)`, subset = (Thinking ==0 )  ,random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel)

par(mfrow=c(2,4))
profile(TwoLevel)

predict.rma(TwoLevel)

#  Follow up ~ Baseline

TwoLevel <- rma.mv(g,v, mod= ~Follow_up, subset = (Thinking ==0 )  ,random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

par(mfrow=c(2,4))
profile(TwoLevel)

#  wait+ Attention
TwoLevel <- rma.mv(g,v, mod= ~wait + Attention + Follow_up, subset = (Thinking ==0 )  ,random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

par(mfrow=c(2,4))
profile(TwoLevel)

# Sex 

TwoLevel <- rma.mv(g,v, mod= ~`Sex_(Female)`, subset = (Thinking ==0 )  ,random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel)

par(mfrow=c(2,4))
profile(TwoLevel)
# NO Problem

# Attention and TAU vs. Wait 



FiveLevel <- rma.mv(g,v, subset = (Waitlist == 1 )  ,random= ~1 |Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)



ThreeLevel <- rma.mv(g,v, mod= ~Attention + TAU, subset = (Thinking ==1 )  ,random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

par(mfrow=c(2,4))
profile(ThreeLevel)


ThreeLevel <- rma.mv(g,v, mod= ~Attention + TAU, subset = (Thinking ==0 )  ,random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel)

par(mfrow=c(2,4))
profile(ThreeLevel)

FiveLevel <- rma.mv(g,v, mods = ~`Sex_(Female)`, subset = (ControlGroup_ID ==3 ), random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)

par(mfrow=c(2,4))
profile(FiveLevel)

FiveLevel <- rma.mv(g,v, subset = (ControlGroup_ID ==2 ), random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)

par(mfrow=c(2,4))
profile(FiveLevel)

FiveLevel <- rma.mv(g,v, mods = ~`Sex_(Female)`, subset = (ControlGroup_ID ==2), random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)

par(mfrow=c(2,4))
profile(FiveLevel)


FiveLevel <- rma.mv(g,v, mods = ~`Sex_(Female)`, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=OtcomeTime_2, method="REML")
summary(FiveLevel)



par(mfrow=c(2,4))
profile(FiveLevel)

FiveLevel <- rma.mv(g,v, mods = ~`Sex_(Female)`, subset = (ControlGroup_ID == 1), random= ~1 |Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)

par(mfrow=c(2,4))
profile(FiveLevel)


OtcomeTime_1 <- subset(TOSC, Outcome_Time ==1)
OtcomeTime_2 <- subset(TOSC, Outcome_Time ==2)

