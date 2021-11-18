# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)


# Data Importation

TOSC <- read_delim("App-Paper/Für R/TOSC.cut.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)


#Preregistered Analysis#####

#_ Hypothesis I#####

#Five Level

FiveLevel <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)

# Four Level No Time 
FourLevel <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel)

RVE<- coef_test(FourLevel,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)



#_ Hypothesis II #####


# Five Level
FiveLevel.T <- rma.mv(g,v, mods = ~Follow_up,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel.T)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FiveLevel.T)

# Four Level - No Time ID

FourLevel.T <- rma.mv(g,v, mods = ~Follow_up,random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.T)

# RVE Correction

RVE<- coef_test(FourLevel.T ,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)


#_Hypothesis III####

#__Model - Waitlist ####
FourLevel.Wait <- rma.mv(g,v, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Wait)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.Wait)

#Model - Waitlist; No Time 
ThreeLevel.Wait.NT <- rma.mv(g,v, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.Wait.NT)

# Parameter evaluation
par(mfrow=c(2,4))
profile(TwoLevel.Wait.NT)

#Model - Waitlist; No Time, No Outcome 
TwoLevel.Wait.NT <- rma.mv(g,v, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.Wait.NT)


# Parameter evaluation
par(mfrow=c(2,4))
profile(TwoLevel.Wait.NT)

# Model - Waitlist; No Outcome 
ThreeLevel.Wait.NO <- rma.mv(g,v, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID/Time_ID, tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.Wait.NO)

# Parameter evaluation
par(mfrow=c(2,4))
profile(ThreeLevel.Wait.NO)

#Included Model - Waitlist; No Time; No Outcome
TwoLevel.Wait.NTO <- rma.mv(g,v, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID, tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.Wait.NTO)

# Parameter evaluation
par(mfrow=c(2,4))
profile(TwoLevel.Wait.NTO)

RVE<- coef_test(TwoLevel.Wait.NTO,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)




#__Model -Attention####

FourLevel.Att <- rma.mv(g,v, subset=(ControlGroup_ID == 2) ,random= ~1 |Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Att)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.Wait)

#Model - Attention; No Time 
ThreeLevel.Att.NT <- rma.mv(g,v, subset=(ControlGroup_ID == 2) ,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.Att.NT)

# Parameter evaluation
par(mfrow=c(2,4))
profile(ThreeLevel.Att.NT)

#Model - Attention; No Outcome
ThreeLevel.Att.NO <- rma.mv(g,v, subset=(ControlGroup_ID == 2) ,random= ~1 |Study_ID/Time_ID, tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.Att.NO)

# Parameter evaluation
par(mfrow=c(2,4))
profile(ThreeLevel.Att.NO)

#Included Model - Attention; No Time; No Outcome
TwoLevel.Att.NTO <- rma.mv(g,v, subset=(ControlGroup_ID == 2) ,random= ~1 |Study_ID, tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.Att.NTO)

# Parameter evaluation
par(mfrow=c(2,4))
profile(TwoLevel.Att.NTO)

RVE<- coef_test(TwoLevel.Att.NTO,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)


#__Model -TAU####

FourLevel.TAU <- rma.mv(g,v, subset=(ControlGroup_ID == 3) ,random= ~1 |Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.TAU)

# Three Level - No Time 
ThreeLevel.TAU <- rma.mv(g,v, subset=(ControlGroup_ID == 3) ,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.TAU)

# Two - No Time and Study
TwoLevel.TAU <- rma.mv(g,v, subset=(ControlGroup_ID == 3) ,random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.TAU)

# Parameter evaluation
par(mfrow=c(2,4))
profile(TwoLevel.TAU)


RVE<- coef_test(TwoLevel.TAU,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)

# Combining Attention and waitlist to increase power 

TOSC$ControlGroup_ID[TOSC$ControlGroup_ID == 2] <- 1
TOSC$ControlGroup_ID[TOSC$ControlGroup_ID == 3] <- 0



# Attention and waitlist combined 

# Four Level Model
FourLevel.Attwait <- rma.mv(g,v, mods= ~ControlGroup_ID,random= ~1 |Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Attwait)

# Three Level Model
ThreeLevel.Attwait <- rma.mv(g,v, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.Attwait)

# Two Level Model
TwoLevel.Attwait <- rma.mv(g,v, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.Attwait)

# Parameter evaluation
par(mfrow=c(2,4))
profile(TwoLevel.Attwait)


RVE<- coef_test(TwoLevel.Attwait,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)

# Resetting Control Groups 

TOSC <- read_delim("App-Paper/Für R/TOSC.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)


#_Hypothesis IV####

#Model- Non Human 
FiveLevel.NONHumanBased <- rma.mv(g,v, mods = ~Intervention_Type ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel.NONHumanBased)


# Four Level - Time 

FourLevel.NONHumanBased <- rma.mv(g,v, subset=(`Intervention_Type` == 1) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.NONHumanBased)

# Parameter evaluation
par(mfrow=c(2,4))
profile(ThreeLevel.NONHumanBased)

RVE<- coef_test(FourLevel.NONHumanBased, vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)


#Model- Human 

FiveLevel.HumanBased <- rma.mv(g,v, subset=(`Intervention_Type` == 2) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel.HumanBased)

# Parameter evaluation

par(mfrow=c(2,4))
profile(FiveLevel.HumanBased)

# Model Human - excl. T

FourLevel.HumanBased.T <- rma.mv(g,v, subset=(`Intervention_Type` == 2) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.HumanBased.T)

# Parameter evaluation

par(mfrow=c(2,4))
profile(FourLevel.HumanBased.T)

# Model Human - excl. T and C

ThreeLevel.HumanBased.TC <- rma.mv(g,v, subset=(`Intervention_Type` == 2) ,random= ~1 | Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.HumanBased.TC)

# Parameter evaluation

par(mfrow=c(2,4))
profile(ThreeLevel.HumanBased.TC)


RVE<- coef_test(ThreeLevel.HumanBased.TC, vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)



#_Hypothesis V####

#Model-Thinking
FiveLevel.Thinking <- rma.mv(g,v, subset=(Outcome_Type == 1) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel.Thinking)

# Parameter evaluation

par(mfrow=c(2,4))
profile(FiveLevel.Thinking)

#Included Model-Thinking
FourLevel.Thinking <- rma.mv(g,v, subset=(Outcome_Type == 1) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Thinking)

FourLevel.Thinking <- rma.mv(g,v, mod = ~Thinking  ,random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Thinking)


# Parameter evaluation

par(mfrow=c(2,4))
profile(FourLevel.Thinking)

RVE<- coef_test(FourLevel.Thinking,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)



#  Acting


# Parameter evaluation

par(mfrow=c(2,4))
profile(FiveLevel.Acting)

#Included Model- Model- Acting No Time 
FourLevel.Acting <- rma.mv(g,v, mods = ~Acting ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Acting)

# Parameter evaluation

par(mfrow=c(2,4))
profile(FourLevel.Acting)

RVE<- coef_test(FiveLevel.Acting,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)

#_ I^2 Heterogeinty Analysis ####

#__Q-Statistics Approach#####

FourLevel.Acting <- rma.mv(g,v, mods = ~Acting ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID, sigma2=c(0,0,NA), tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.ActingReduced1)

#__Hypothesis I#####

FourLevel.T <- rma.mv(g,v ,random= ~1 | ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T)

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
 

#__Hypothesis II####

FourLevel.T <- rma.mv(g,v, mods = ~Follow_up ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T)


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

#__Hypothesis III####

# Waitlist 

TwoLevel.Wait.NTO <- rma.mv(g,v, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID, tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.Wait.NTO)



n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (TwoLevel.Wait.NTO$sigma2[1]
                                        + estimated.sampling.variance)
I2_2 <- (TwoLevel.Wait.NTO$sigma2[1]) / (TwoLevel.Wait.NTO$sigma2[1]
                                    + estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Study_Variance <- I2_2 * 100


### Outcome

Samplingvariance 
Study_Variance 


# Attention Group
TwoLevel.Att.NTO <- rma.mv(g,v, subset=(ControlGroup_ID == 2) ,random= ~1 |Study_ID, tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.Att.NTO)

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (TwoLevel.Att.NTO$sigma2[1]
                                         + estimated.sampling.variance)
I2_2 <- (TwoLevel.Att.NTO$sigma2[1]) / (TwoLevel.Att.NTO$sigma2[1]
                                         + estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Study_Variance <- I2_2 * 100

Samplingvariance 
Study_Variance

# TAU 

FourLevel.TAU <- rma.mv(g,v, subset=(ControlGroup_ID == 3) ,random= ~1 |Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.TAU)

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (FourLevel.TAU$sigma2[1]
                                         + FourLevel.TAU$sigma2[2] +FourLevel.TAU$sigma2[3]+ estimated.sampling.variance)
I2_2 <- (FourLevel.TAU$sigma2[1]) / (FourLevel.TAU$sigma2[1]
                                   + FourLevel.TAU$sigma2[2] +FourLevel.TAU$sigma2[3]+ estimated.sampling.variance)
I2_3 <- (FourLevel.TAU$sigma2[2]) / (FourLevel.TAU$sigma2[1]
                                   + FourLevel.TAU$sigma2[2] +FourLevel.TAU$sigma2[3]+ estimated.sampling.variance)
I2_4 <- (FourLevel.TAU$sigma2[3]) / (FourLevel.TAU$sigma2[1]
                                   + FourLevel.TAU$sigma2[2] +FourLevel.TAU$sigma2[3]+ estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Study_Variance <- I2_2 * 100
Outcome_Variance <- I2_3 * 100
Time_Variance <- I2_4 * 100

### Outcome

Samplingvariance 
Study_Variance 
Outcome_Variance 
Time_Variance 

#__Hypothesis IV####

# Non HumanBased
FiveLevel.NONHumanBased <- rma.mv(g,v, subset=(`Intervention Type` == 2) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel.NONHumanBased)

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <-
  sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances -
  sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (FiveLevel.NONHumanBased$sigma2[1]
                                         + FiveLevel.NONHumanBased$sigma2[2] +FiveLevel.NONHumanBased$sigma2[3]+FiveLevel.NONHumanBased$sigma2[4]+ estimated.sampling.variance)
I2_2 <- (FiveLevel.NONHumanBased$sigma2[1]) / (FiveLevel.NONHumanBased$sigma2[1]
                                 + FiveLevel.NONHumanBased$sigma2[2] +FiveLevel.NONHumanBased$sigma2[3]+FiveLevel.NONHumanBased$sigma2[4]+ estimated.sampling.variance)
I2_3 <- (FiveLevel.NONHumanBased$sigma2[2]) / (FiveLevel.NONHumanBased$sigma2[1]
                                 + FiveLevel.NONHumanBased$sigma2[2] +FiveLevel.NONHumanBased$sigma2[3]+FiveLevel.NONHumanBased$sigma2[4]+ estimated.sampling.variance)
I2_4 <- (FiveLevel.NONHumanBased$sigma2[3]) / (FiveLevel.NONHumanBased$sigma2[1]
                                 + FiveLevel.NONHumanBased$sigma2[2] +FiveLevel.NONHumanBased$sigma2[3]+FiveLevel.NONHumanBased$sigma2[4]+ estimated.sampling.variance)

I2_5 <- (FiveLevel.NONHumanBased$sigma2[4]) / (FiveLevel.NONHumanBased$sigma2[1]
                                 + FiveLevel.NONHumanBased$sigma2[2] +FiveLevel.NONHumanBased$sigma2[3]+FiveLevel.NONHumanBased$sigma2[4]+ estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Study_Variance <- I2_3 * 100
Outcome_Variance <- I2_4 * 100
Time_Point_Variance <- I2_5 * 100

### Outcome

Samplingvariance 
Control_group_Variance 
Study_Variance 
Outcome_Variance 
Time_Point_Variance 

# Human Based Model

TwoLevel.HumanBased.TCSO <- rma.mv(g,v, subset=(`Intervention Type` == 1) ,random= ~1 | Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.HumanBased.TCSO)

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (TwoLevel.HumanBased.TCSO$sigma2[1]
                                         + estimated.sampling.variance)
I2_2 <- (TwoLevel.HumanBased.TCSO$sigma2[1]) / (TwoLevel.HumanBased.TCSO$sigma2[1]
                                         + estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Study_Variance <- I2_2 * 100


### Outcome

Samplingvariance 
Study_Variance 



#__Hypothesis V####

# Model Thinking
FourLevel.Thinking <- rma.mv(g,v, subset=(Outcome_Type == 1) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Thinking)

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (FourLevel.Thinking$sigma2[1]
                                         + FourLevel.Thinking$sigma2[2] +FourLevel.Thinking$sigma2[3]+ estimated.sampling.variance)
I2_2 <- (FourLevel.Thinking$sigma2[1]) / (FourLevel.Thinking$sigma2[1]
                                   + FourLevel.Thinking$sigma2[2] +FourLevel.Thinking$sigma2[3]+ estimated.sampling.variance)
I2_3 <- (FourLevel.Thinking$sigma2[2]) / (FourLevel.Thinking$sigma2[1]
                                   + FourLevel.Thinking$sigma2[2] +FourLevel.Thinking$sigma2[3]+ estimated.sampling.variance)
I2_4 <- (FourLevel.Thinking$sigma2[3]) / (FourLevel.Thinking$sigma2[1]
                                   + FourLevel.Thinking$sigma2[2] +FourLevel.Thinking$sigma2[3]+ estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Study_Variance <- I2_3 * 100
Outcome_Variance <- I2_4 * 100

Samplingvariance 
Control_group_Variance 
Study_Variance 
Outcome_Variance 


# Acting Model 

FourLevel.Acting <- rma.mv(g,v, subset=(Outcome_Type == 2) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Acting)

n <- length(TOSC$v)
list.inverse.variances <- 1 / (TOSC$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (TOSC$v^2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (FourLevel.Acting$sigma2[1]
                                         + FourLevel.Acting$sigma2[2] +FourLevel.Acting$sigma2[3]+ estimated.sampling.variance)
I2_2 <- (FourLevel.Acting$sigma2[1]) / (FourLevel.Acting$sigma2[1]
                                          + FourLevel.Acting$sigma2[2] +FourLevel.Acting$sigma2[3]+ estimated.sampling.variance)
I2_3 <- (FourLevel.Acting$sigma2[2]) / (FourLevel.Acting$sigma2[1]
                                          + FourLevel.Acting$sigma2[2] +FourLevel.Acting$sigma2[3]+ estimated.sampling.variance)
I2_4 <- (FourLevel.Acting$sigma2[3]) / (FourLevel.Acting$sigma2[1]
                                          + FourLevel.Acting$sigma2[2] +FourLevel.Acting$sigma2[3]+ estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Study_Variance <- I2_3 * 100
Outcome_Variance <- I2_4 * 100

Samplingvariance 
Control_group_Variance 
Study_Variance 
Outcome_Variance 



# Exploratroy Analysis#####

#_Mod.Sex#####

#__ Hypothesis I #####

# Five Level Model
FiveLevel <- rma.mv(g,v, mods = `Sex (Female)` ,random= ~1 | ControlGroup_ID/Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.TS)


FourLevel.OS <- rma.mv(g,v, mods = `Sex (Female)` ,random= ~1 | ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.OS)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.OS)

# Robust Variance Correction
RVE<- coef_test(FourLevel.OS,cluster=TOSC$ControlGroup_ID ,vcov = "CR2")


error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)

#__Hypothesis II#####


FourLevel.TS <- rma.mv(g,v, mods = ~`Sex (Female)`+ Follow_up, random = ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")

summary(FourLevel.TS)

par(mfrow=c(2,4))
profile(FourLevel.TS)

RVE<- coef_test(FourLevel.OS,cluster=TOSC$ControlGroup_ID ,vcov = "CR2")

error<- 1.96*RVE$SE

lower.bound <- RVE$beta - error
upper.bound <- RVE$beta + error

RVE
c(lower.bound,upper.bound)



#__Hypothesis III #####

# Waitlist
FourLevel.Wait <- rma.mv(g,v, mod = ~`Sex (Female)`, subset=(ControlGroup_ID == 1) ,random= ~1 |Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.Wait)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.Wait)

# Attention
TwoLevel.Att.NTO.Sex <- rma.mv(g,v,mod = ~`Sex (Female)`, subset=(ControlGroup_ID == 2) ,random= ~1 |Study_ID, tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.Att.NTO.Sex)

# Parameter evaluation
par(mfrow=c(2,4))
profile(TwoLevel.Att.NTO.Sex)

#TAU
FourLevel.TAU <- rma.mv(g,v,mod = ~`Sex (Female)`, subset=(ControlGroup_ID == 3) ,random= ~1 |Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.TAU)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.TAU)

#Controlgroups as continous moderators 

# Four Level Model
FourLevel.TAUC <- rma.mv(g,v,mod = ~`Sex (Female)`+ControlGroup_ID ,random= ~1 |Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.TAUC)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FourLevel.TAUC)

# Three Level Model
ThreeLevel.TAUC <- rma.mv(g,v,mod = ~`Sex (Female)`+ControlGroup_ID ,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(ThreeLevel.TAUC)

# Parameter evaluation
par(mfrow=c(2,4))
profile(ThreeLevel.TAUC)

#_Hypothesis IV#####

# Human based
TwoLevel.HumanBased.TCSOSex <- rma.mv(g,v, mod=~`Sex (Female)`, subset=(`Intervention Type` == 1) ,random= ~1 | Study_ID,tdist = TRUE, data=TOSC, method="REML")
summary(TwoLevel.HumanBased.TCSOSex)

# Parameter evaluation

par(mfrow=c(2,4))
profile(TwoLevel.HumanBased.TCSOSex)

# Non Human Based

FiveLevel.NONHumanBasedSex <- rma.mv(g,v, mod=~`Sex (Female)`, subset=(`Intervention Type` == 2) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevel.NONHumanBasedSex)

# Parameter evaluation
par(mfrow=c(2,4))
profile(FiveLevel.NONHumanBasedSex)

#_Hypothesis V######

#Model Thinking 

FourLevel.ThinkingSex <- rma.mv(g,v, mod=~`Sex (Female)`, subset=(Outcome_Type == 1) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.ThinkingSex)

# Parameter evaluation

par(mfrow=c(2,4))
profile(FourLevel.ThinkingSex)

# Model Acting 

FourLevel.ActingSex <- rma.mv(g,v, mod=~`Sex (Female)`, subset=(Outcome_Type == 2) ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.ActingSex)

# Parameter evaluation

par(mfrow=c(2,4))
profile(FourLevel.ActingSex)

