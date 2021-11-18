####Set-Up####

library(metafor)
library(readr)
library(clubSandwich)

#Import data
TOSC <- read_delim("App-Paper/Für R/App_TOSC_cut.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

####Model Evaluation####

## Proposed Five Level Model ##


FiveLevel <- rma.mv(g,v, random= list( ~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID, ~1|Time_ID),tdist = TRUE, data=TOSC, method="REML")

summary(FiveLevel)


### profil liklihood plots ###

par(mfrow=c(2,4))
profile(FiveLevel) # Warning for Control Group Level!


#Heterogeitny Analysis

## Using Q-Statistics

### Time (p = 1) -> No Variability due to timepoints ?
FiveLevel.Time <- rma.mv(g,v, random= list(~1 |ControlGroup_ID, ~1| Study_ID, ~1|Outcome_ID, ~1|Time_ID),tdist = TRUE, data=data, method="REML", sigma2 = c(NA,NA,NA,0))

summary(FiveLevel.Time)

anova(FiveLevel,FiveLevel.Time)

### between outcome variance (p ~ 0) 
FiveLevel.outcome <- rma.mv(g,v, random= list(~1 |ControlGroup_ID, ~1| Study_ID, ~1|Outcome_ID, ~1|Time_ID),tdist = TRUE, data=data1, method="REML", sigma2 = c(NA,NA,0,NA))

summary(FiveLevel.outcome)

anova(FiveLevel,FiveLevel.outcome)

### study Level (0,0063) 
FiveLevel.study <- rma.mv(g,v, random= list(~1 |ControlGroup_ID, ~1| Study_ID, ~1|Outcome_ID, ~1|Time_ID),tdist = TRUE, data=data1, method="REML", sigma2 = c(NA,0,NA,NA))

anova(FiveLevel,FiveLevel.study)

### between Controlgroup variance (p.44) 

FiveLevel.Control <- rma.mv(g,v, random= list(~1 |ControlGroup_ID, ~1| Study_ID, ~1|Outcome_ID, ~1|Time_ID),tdist = TRUE, data=data1, method="REML", sigma2 = c(0,NA,NA,NA))
anova(FiveLevel,FiveLevel.Control)


## Proposed Five Level Model; Combining Attention Placebo and waitlist 

TOSC$ControlGroup_ID[TOSC$ControlGroup_ID == 2] <- 1


FiveLevelC2 <- rma.mv(g,v, random= list(~1 |ControlGroup_ID, ~1 | Study_ID,~1 | Outcome_ID, ~1 | Time_ID),tdist = TRUE, data=TOSC, method="REML")

summary(FiveLevelC2)


### profile liklihood plots 

par(mfrow=c(2,4))
profile(FiveLevelC2) # No warning, liklihood plot looks better.


### Interclass correlation coefficent (ICC) of Controllgroup  
round(FiveLevelC2$sigma2[1]/sum(FiveLevelC2$sigma2),3)

### Inclusion of Control group as a Level questionable. 

#Heterogeitny Analysis

## Using Q-Statistics

### Time (p = 1) -> No Variability due to timepoints ?
FiveLevel.Time <- rma.mv(g,v, random= list(~1 |ControlGroup_ID, ~1| Study_ID, ~1|Outcome_ID, ~1|Time_ID),tdist = TRUE, data=data2, method="REML", sigma2 = c(NA,NA,NA,0))

summary(FiveLevel.Time)

anova(FiveLevelC2,FiveLevel.Time)

### between Controlgroup variance (p.44) 

FiveLevel.Control <- rma.mv(g,v, random= list(~1 |ControlGroup_ID, ~1| Study_ID, ~1|Outcome_ID, ~1|Time_ID),tdist = TRUE, data=data2, method="REML", sigma2 = c(0,NA,NA,NA))
anova(FiveLevelC2,FiveLevel.Control)

## Using I^2 Statistics

### Basic Model

n <- length(data$v)
list.inverse.variances <- 1 / (data$v)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (data$v^2)
sum.inverse.variances.square <-
  sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances -
  sum.inverse.variances.square

estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (FiveLevelC2$sigma2[1]
                                         + FiveLevelC2$sigma2[2] +FiveLevelC2$sigma2[3]+FiveLevelC2$sigma2[4]+ estimated.sampling.variance)
I2_2 <- (FiveLevel$sigma2[1]) / (FiveLevelC2$sigma2[1]
                                 + FiveLevelC2$sigma2[2] +FiveLevelC2$sigma2[3]+FiveLevelC2$sigma2[4]+ estimated.sampling.variance)
I2_3 <- (FiveLevel$sigma2[2]) / (FiveLevelC2$sigma2[1]
                                 + FiveLevelC2$sigma2[2] +FiveLevelC2$sigma2[3]+FiveLevelC2$sigma2[4]+ estimated.sampling.variance)
I2_4 <- (FiveLevel$sigma2[3]) / (FiveLevelC2$sigma2[1]
                                 + FiveLevelC2$sigma2[2] +FiveLevelC2$sigma2[3]+FiveLevelC2$sigma2[4]+ estimated.sampling.variance)

I2_5 <- (FiveLevel$sigma2[4]) / (FiveLevelC2$sigma2[1]
                                 + FiveLevelC2$sigma2[2] +FiveLevelC2$sigma2[3]+FiveLevelC2$sigma2[4]+ estimated.sampling.variance)

Samplingvariance <- I2_1 * 100
Control_group_Variance <- I2_2 * 100
Study_Variance <- I2_3 * 100
Outcome_Variance <- I2_4 * 100
Time_Point_Variance <- I2_5 * 100

### Outcome

Samplingvariance # ~ 42%
Control_group_Variance # ~ 0%
Study_Variance # ~ 32%
Outcome_Variance # ~ 25 %
Time_Point_Variance # ~ 0

### Controling the nessesity of TIme and Outcome via SE (Noortgate et al. 2013)

FiveLevel <- rma.mv(g,v, random= list( ~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID, ~1|Time_ID),tdist = TRUE, data=TOSC, method="REML")

summary(FiveLevel) # SE .0303

FourLevel.NoTime <- rma.mv(g,v, random= list( ~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID),tdist = TRUE, data=TOSC, method="REML")

summary(FourLevel.NoTime) # SE .0303

FourLevel.NOControl <- rma.mv(g,v, random= list( ~1| Study_ID, ~1| Outcome_ID, ~1|Time_ID),tdist = TRUE, data=TOSC, method="REML")

summary(FourLevel.NOControl) # SE .0244

ThreeLevel <- rma.mv(g,v, random= list( ~1| Study_ID, ~1| Outcome_ID),tdist = TRUE, data=TOSC, method="REML")

summary(ThreeLevel) # SE .0244

# No Variance due to Time ?


# Using a reduced Data set, only including post vs. furthest follow up.

App_TOSC_cut_Time_Alt <- read_delim("App-Paper/Für R/App_TOSC_cut.Time_Alt.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)

data1 <- App_TOSC_cut_Time_Alt
data1$ControlGroup_ID[data1$ControlGroup_ID == 2] <- 1


FiveLevel.AltTime <- rma.mv(g,v, random= list( ~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID, ~1|Time_ID),tdist = TRUE, data=data1, method="REML")

summary(FiveLevel.AltTime) # SE.0292

# As Subsets
FourLevel.Followup <- rma.mv(g,v, subset = (Follow_up == 1), random= list( ~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID),tdist = TRUE, data=data1, method="REML")

summary(FourLevel.Followup)

FourLevel.Post <- rma.mv(g,v,subset = (Follow_up == 0), random= list( ~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID),tdist = TRUE, data=data1, method="REML")

summary(FourLevel.Post)

FiveLevel.TimeAlt <- rma.mv(g,v, random= list( ~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID, ~1|Outcome_Time),tdist = TRUE, data=data1, method="REML")

summary(FiveLevel.TimeAlt) # 

data1$ControlGroup_ID[data1$ControlGroup_ID == 2] <- 1



## Controlling if Time variance is modelled & just neglectible OR Model fails to include Time

App_TOSC_Post_and_Fl_1 <- read_delim("App-Paper/Für R/App_TOSC_Post and Fl_1.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)

data3<- App_TOSC_Post_and_Fl_1

FiveLevelC2_P1_F1 <- rma.mv(g,v, random= list(~1 |ControlGroup_ID, ~1 | Study_ID,~1 | Outcome_ID, ~1 | Time_ID),tdist = TRUE, data=data3, method="REML")

summary(FiveLevelC2_P1_F1)

# Small advantage in Modelling Control groups
# Within outcome variance (due to time) exits 
# --> Testing if a between Timepoint variance exists, due to timepoints better captures the dependency
# -> New Model Model B: Controlgroup/study/time-point/outcome ?

#### Model B #####

COTS <- read_delim("App-Paper/Für R/App_OTS_cut.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

COTS$ControlGroup_ID[COTS$ControlGroup_ID == 2] <- 1


Model.B <-  rma.mv(g,v, random= list(~1|ControlGroup_ID, ~1| Study_ID, ~1|Outcome_Time, ~1 | Outcome_ID),tdist = TRUE, data=COTS, method="REML")

summary(Model.B)

par(mfrow=c(2,4))

profile(Model.B)

Model.B.FixedTime<- rma.mv(g,v, random= list(~1| Study_ID, ~1|Outcome_Time, ~1 | Outcome_ID),tdist = TRUE, data=OTS, sigma2 = c(NA,0,NA), method="REML")

anova(Model.B,Model.B.FixedTime)

# Time has a negible overall effect on outcome, Level
# But comparing Outcome-TIme-Study, with Time Outcome Study; it emerges that the former models minimally more variance!
# Model is overparamtized (not converging)
# Retaining TOS Design, next evaluating Control groups?



####Hypothesis, with per Pre-Registration Model #### 




## Hypothesis 1 

FiveLevelC2 <- rma.mv(g,v ,random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevelC2)

par(mfrow=c(2,4))
profile(FiveLevelC2) # Not Converging.

### Reducing Control group #####

# Control group has no impact on SE /Heterogeinty if split in TAU, Attention and waitlist.
# Control group do not converge if combined in TAU vs. attention & waitlist.
# -> Not Modelling Control-Group

TOSC <- read_delim("App-Paper/Für R/App_TOSC_cut.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

FourLevel <- rma.mv(g,v ,random= ~1 | Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")

profile(FourLevel) # No Warning of over paramatization or non convergence


##### Answering Hypothesis ####

# Hypothesis 1: Overall Effectivness 


FourLevel <- rma.mv(g,v ,random= ~1 | Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")


# Hypothesis 2: Post vs. Follow up

ThreeLevel.F <- rma.mv(g,v,subset = (Follow_up == "1"),random= ~1 | Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")

par(mfrow=c(2,4))
profile(ThreeLevel.F)

summary(ThreeLevel.F)

ThreeLevel.P <- rma.mv(g,v,subset = (Follow_up == "0"),random= ~1 | Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")

par(mfrow=c(2,4))
profile(ThreeLevel.P)

summary (ThreeLevel.P)

# Follow-up: .1299 CI95%[.076;0.18]
#Post: .169 CI95%[.11;.22]

# Hypothesis 3: Controllgroup Comparison 

FourLevel.TAU <- rma.mv(g,v, subset = (ControlGroup_ID == "3") ,random= ~1 | Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")

par(mfrow=c(2,4))
profile(FourLevel.TAU) # No Warning 

summary(FourLevel.TAU)

FourLevel.W <- rma.mv(g,v, subset = (ControlGroup_ID == "1") ,random= ~1 | Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")

par(mfrow=c(2,4))
profile(FourLevel.W) # No Warning 

summary(FourLevel.W)

FourLevel.Att <- rma.mv(g,v, subset = (ControlGroup_ID == "2") ,random= ~1 | Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")

par(mfrow=c(2,4))
profile(FourLevel.Att) # No Convergence!


TOSC$ControlGroup_ID[TOSC$ControlGroup_ID == 2] <- 1

FourLevel.W.Att <- rma.mv(g,v, subset = (ControlGroup_ID == "1") ,random= ~1 | Study_ID/Outcome_ID/Time_ID, tdist = TRUE, data=TOSC, method="REML")

par(mfrow=c(2,4))
profile(FourLevel.W.Att)

summary(FourLevel.W.Att) # .2 [.11; .29]

par(mfrow=c(2,4))
profile(FourLevel.W.Att)


## Hypothesis 4 

FiveLevelC2.H2 <- rma.mv(g,v, subset = (`Intervention Type` == "1"), random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
FiveLevelC2.NH <- rma.mv(g,v, subset = (`Intervention Type` == "2"), random= ~1 | Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=TOSC, method="REML")

summary(FiveLevelC2.H) #.163
summary(FiveLevelC2.H2)

par(mfrow=c(2,4))
profile(FiveLevelC2.H)


summary(FiveLevelC2.NH) #.148


## Hypothesis 5 
FiveLevelC2.ST <- rma.mv(g,v, subset = (Outcome_Type == "1"), random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=data2, method="REML")
FiveLevelC2.SB <- rma.mv(g,v,subset = (Outcome_Type == "2"), random= ~1 |ControlGroup_ID/ Study_ID/Outcome_ID/Time_ID,tdist = TRUE, data=data2, method="REML")

summary(FiveLevelC2.ST) #.2
summary(FiveLevelC2.SB) #.1

profile(FiveLevelC2.ST)

# Overall to Heterogenous 
# Overall just non significant differences

#### Reduced Models (Four and Three)
# Control group had a very small impact on results (both as a level and a subset).
# To control if results using a Five Level model are robust, Hypthesis are retested using a four Level Model.
#
#


FiveLevelC2 <- rma.mv(g,v ,random= list(~1 |ControlGroup_ID, ~1 | Study_ID , ~1 |Outcome_ID,  ~1 |Time_ID), tdist = TRUE, data=TOSC, method="REML")
summary(FiveLevelC2)

FourLevel <- rma.mv(g,v, random= list( ~1 | Study_ID,~1 | Outcome_ID, ~1 | Time_ID),tdist = TRUE, data=TOSC, method="REML")

summary(FourLevel)


App_OS_cut <- read_delim("App-Paper/Für R/App_OS_cut.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

ThreeLevel <- rma.mv(g,v, random= list( ~1 | Study_ID,~1 | Outcome_ID),tdist = TRUE, data=TOSC, method="REML")

summary(ThreeLevel)

# SE In Five higher then in Three or Four --> Reduction not acceptable.
# Time SE currently not modelled; changing coding to Post - furthest Followup set up.


#### Using a reduced Data set, only including post vs. furthest follow up. #####

App_TOSC_cut_Time_Alt <- read_delim("App-Paper/Für R/App_TOSC_cut.Time_Alt.csv", 
                                    ";", escape_double = FALSE, trim_ws = TRUE)

data1 <- App_TOSC_cut_Time_Alt

data1$ControlGroup_ID[data1$ControlGroup_ID == 2] <- 1

data1$ControlGroup_ID[data1$ControlGroup_ID == 3] <- 2


FiveLevel.TimeAlt <- rma.mv(g,v, random= list(~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID, ~1|Outcome_Time),tdist = TRUE, data=data1, method="REML")
FourLevel.TimeAlt <- rma.mv(g,v, random= list( ~1| Study_ID, ~1| Outcome_ID, ~1|Outcome_Time),tdist = TRUE, data=data1, method="REML")

summary(FiveLevel.TimeAlt)

par(mfrow=c(2,4))
profile(FiveLevel.TimeAlt) # Problem in o^2(4) brocken chain.
profile(FourLevel.TimeAlt) # brocken o^2(4) chain

# Model does not converge!




#### Moderator sex####

FiveLevel.mod <- rma.mv(g,v, mods = ~`Sex (Female)`, random= list( ~1|ControlGroup_ID, ~1| Study_ID, ~1| Outcome_ID, ~1|Time_ID),tdist = TRUE, data=data, method="REML")
