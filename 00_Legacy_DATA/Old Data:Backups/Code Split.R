# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)
library(ggplot2)
library("tidyverse")
library(robumeta)

# Data Importation

TOSC <- read_delim("Für R/TOSC.cut.split.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)



# Splitting into Postvention and Follow-up

Post <- subset(TOSC, Follow_up == 0)

Follow_up <- subset(TOSC, Follow_up == 1)

# Basic Models & robust Variance Models (For publication bias estimation)

# Multi-Level Model

FourLevel.T1 <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(FourLevel.T1)


print(RVE_intercept)

sensitivity(RVE_intercept)

Post$ControlGroup_ID <- group.mean(Post$ControlGroup_ID, Post$Study_ID)
Post$Outcome_ID <- group.center(Post$Outcome_ID, Post$Study_ID)

model_hier <- robu(g ~ ControlGroup_ID + Outcome_ID,
                       data = Post, modelweights = "HIER", studynum =
                         Study_ID, var.eff.size = v, small = TRUE)

#Hypothesis I & Hypothesis II #####


# Four Level -Post 
FourLevel.T1 <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(FourLevel.T1)

RVE.Model <- rma.mv(g,v, random= ~1 |Study_ID,tdist = TRUE, data=Post, method="REML")
summary(RVE.Model)




# Four Level - Follow Up
FourLevel.T2 <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=Follow_up, method="REML")
summary(FourLevel.T2)

# -> no sigma^2 for study Level.
#Study Level not needed ?


# Three Level -Post 
ThreeLevel.T1 <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(ThreeLevel.T1)

# Three Level - Follow Up
ThreeLevel.T2 <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=Follow_up, method="REML")
summary(FourLevel.T2)

# Post
# SE of Model with Study: 0.0307
# SE of Model without Study: 0.0302
# Follow Up
# SE of Model with Study: 0.0664
#SE of Model without Study:  0.0664


anova(ThreeLevel.T1,FourLevel.T1)

anova(ThreeLevel.T2,FourLevel.T2)

# Result: Follow up only profits from removal
# Result: Post, no notable difference except a .000X Denomination, but Model fit profits from removal.
# -> Post: Removal feasible if Paramatization errors are otherwise observed.

# Reintroducing Old PostModel

FourLevel.T1 <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(FourLevel.T1)

# parameter control.
par(mfrow=c(2,4))
profile(FourLevel.T1)
# No Problem 

par(mfrow=c(2,4))
profile(FourLevel.T2)
# No Problem 

------------------------ Corrected -------------------


#_Inkl. Sex as a Moderator ####

# Four Level -Post 
FourLevel.T1 <- rma.mv(g,v, mod = ~ `Sex_(Female)`, subset = (Follow_up == 0), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(FourLevel.T1)

# Four Level - Follow Up
FourLevel.T2 <- rma.mv(g,v,mod = ~ `Sex_(Female)`,subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=Follow_up, method="REML")
summary(FourLevel.T2)

# Study Level not needed.


# Three Level -Post 
FourLevel.T1 <- rma.mv(g,v,mod = ~ `Sex_(Female)`,subset = (Follow_up == 0), random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

# p 0.2554

# Three Level - Follow Up
FourLevel.T2 <- rma.mv(g,v,mod = ~ `Sex_(Female)`,subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)

# p-val = 0.0088
# Only in Follow Up

# parameter control.
par(mfrow=c(2,4))
profile(FourLevel.T1)
# No Problem 

par(mfrow=c(2,4))
profile(FourLevel.T2)
# No Problem 




#Hypothesis III #########

# Overview of all moderator combinations of Control Group

# Three Level -Post wait vs. Attention & TAU ( Any Interaction helps )
FourLevel.T1 <- rma.mv(g,v, mod =~ wait, subset = (Follow_up == 0), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

# Three Level -Follow Up wait vs. Attention & TAU ( Any Interaction helps )
FourLevel.T2 <- rma.mv(g,v, mod =~ wait, subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)

# 0.0029 

# Three Level -Post TAU vs Attention & wait (Psyciatric Moderats, not Interaction alone)
FourLevel.T1 <- rma.mv(g,v, mod =~ TAU, subset = (Follow_up == 0), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

# 0.057

# Three Level -Follow Up TAU vs Attention wait (Psyciatric Moderats, not Interaction alone)
FourLevel.T2 <- rma.mv(g,v, mod =~ TAU, subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)


## Post: TAU as a Moderator.
## Follow Up: Wait as a Moderator.

# Post Modded

FourLevel.T1 <- rma.mv(g,v, mod =~ TAU, subset = (Follow_up == 0), random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

par(mfrow=c(2,4))
profile(FourLevel.T1)

# Follow up Modded
FourLevel.T2 <- rma.mv(g,v, mod =~ wait, subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)


par(mfrow=c(2,4))
profile(FourLevel.T2)
# NO Problem

FourLevel.T2 <- rma.mv(g,v, subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)

par(mfrow=c(2,4))
profile(FourLevel.T2)
# No Problem


#Hypothesis IV########

# Three Level -Post wait 
FourLevel.T1 <- rma.mv(g,v, mod =~ Intervention_Type, subset = (Follow_up == 0), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

# Three Level -Follow Up 
FourLevel.T2 <- rma.mv(g,v, mod =~ Intervention_Type, subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)


# Three Level -Post wait 
FourLevel.T1 <- rma.mv(g,v, mod =~ Intervention_Type, subset = (Follow_up == 0), random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

par(mfrow=c(2,4))
profile(FourLevel.T1)
# No Problem

# Three Level -Follow Up 
FourLevel.T2 <- rma.mv(g,v, mod =~ Intervention_Type, subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)

par(mfrow=c(2,4))
profile(FourLevel.T2)
# No Problem

# No Difference between Intervention Types.

# Hypthesis V######

FourLevel.T1 <- rma.mv(g,v, mod =~ Thinking, subset = (Follow_up == 0), random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

par(mfrow=c(2,4))
profile(FourLevel.T1)


# Three Level -Follow Up 
FourLevel.T2 <- rma.mv(g,v, mod =~ Thinking , subset = (Follow_up == 1), random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)



# Homogenous Versions of Follow up and Postvention


FourLevel.T1 <- rma.mv(g,v, mod =~ Thinking, subset = (Follow_up == 0), random= ~1 |Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

par(mfrow=c(2,4))
profile(FourLevel.T1)

FourLevel.T2 <- rma.mv(g,v, mod =~ wait + `Sex_(Female)`,subset = (Follow_up == 1) ,random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)

par(mfrow=c(2,4))
profile(FourLevel.T2)


FourLevel.T2 <- rma.mv(g,v, mod =~ wait + `Sex_(Female)`,subset = (Follow_up == 1) ,random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T2)

par(mfrow=c(2,4))
profile(FourLevel.T2)

# Value at Sex Mean in waitgroup
anova(FourLevel.T2, L =c(1, mean(TOSC$`Sex_(Female)`)))

# Value at Sex: 50/50 in waitgroup
anova(FourLevel.T2, L =c(1, 50))

# Value at 100% Male in waitgroup

anova(FourLevel.T2, L =c(1, 0))


# Value at 0% Male in waitgroup

anova(FourLevel.T2, L =c(1, 100))


# Value at Sex Mean in Attention& TAU
anova(FourLevel.T2, L =c(0, mean(TOSC$`Sex_(Female)`)))

# Value at Sex: 50/50 in Attention& TAU
anova(FourLevel.T2, L =c(0, 50))

# Value at 100% Male in Attention& TAU

anova(FourLevel.T2, L =c(0, 0))


# Value at 0% Male  in Attention& TAU

test<- anova(FourLevel.T2, L =c(0, 50))

#Graphical representation########

# Estimates for TAU& Attention, varying Sex
data <- data.frame(estimate = rep(NA,102))

for(i in c(1:102)) {
   Out<- anova(FourLevel.T2, L =c(0,i-2))
    Out2<- Out$Lb
    data[,i] <- rep(Out2) }

data <- as.data.frame(t(data[,-1]))
`Attention & TAU` <- data$V1

# Estimates for TAU& Attention, varying Sex

data <- data.frame(estimate = rep(NA,102))

for(i in c(1:102)) {
  Out<- anova(FourLevel.T2, L =c(1,i-2))
  Out2<- Out$Lb
  data[,i] <- rep(Out2) }

data <- as.data.frame(t(data[,-1]))
`Waiting` <- data$V1

# Preperaing Data for grahpical dipiction
`% Female`<- c(0:100)

graph.data<- cbind.data.frame(`Waiting`,`Attention & TAU`, `% Female`)


df <- graph.data %>%
  select(`% Female`,`Waiting` , `Attention & TAU`) %>%
  gather(key = "Control Group", value = "Hedges g", -`% Female`)
head(df)

ggplot(df, aes(x =`% Female` , y = `Hedges g`)) + 
  geom_line(aes(color = `Control Group`, linetype = `Control Group`)) + 
  scale_color_manual(values = c("darkred", "steelblue"))


#weigths for Study Quality estimation##########

# 
FourLevel.T1 <- rma.mv(g,v,subset = (Follow_up == 0), random= ~1 |ControlGroup_ID/Outcome_ID,tdist = TRUE, data=TOSC, method="REML")
summary(FourLevel.T1)

weights.rma.mv(FourLevel.T1)

# Postvention

# Introducing unmoderated Model
FourLevel.T1 <- rma.mv(g,v, random= ~1 |ControlGroup_ID/Study_ID/Outcome_ID,tdist = TRUE, data=Post, method="REML")
summary(FourLevel.T1)

##_Controlvalue 

coef(FourLevel.T1)

wi <- 1 / (sum(FourLevel.T1$sigma2) + Post$v)
sum(wi * Post$g) / sum(wi)

##_Including Dependencies 

round(vcov(FourLevel.T1, type="obs")[1:8,1:8], 3)


round(sum(FourLevel.T1$sigma2) + Post$v[1:8], 3)

round(weights(FourLevel.T1, type="matrix")[1:8,1:8], 3)

W <- weights(FourLevel.T1, type="matrix")
X <- model.matrix(FourLevel.T1)
y <- cbind(Post$g)
solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y

W <- weights(FourLevel.T1, type="matrix")
sum(rowSums(W) * Post$g) / sum(W)

# Output:0.1336904
# Controlvalue: 0.1336904

W[2,1:2]

weights(FourLevel.T1, type="rowsum")[1:4]

wi <- weights(FourLevel.T1, type="rowsum")
sum(wi * Post$g) / sum(wi)

data.frame(k = c(table(Post$Study_ID)),
           weight = sapply(split(wi, Post$Study_ID), sum))

