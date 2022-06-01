# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)
library("tidyverse")
library(devEMF)


# Data Importation & Preparation ####

Total <- read_delim("03_Data.csv", 
                                ",", escape_double = FALSE, trim_ws = TRUE)

Total <- subset(Total, NSSI ==0 )



#Study Aggregate Data for descriptive Statistics####

###Independend of time
Total2 <- escalc(yi=yi, vi=variance, data=Total )
Study_avarage<- aggregate(Total2, cluster= Study_ID, struct = "CS", rho= 0.6, na.rm=TRUE)

###At Post 
Post <- subset(Total, Follow_up == 0)
Post2 <- escalc(yi=yi, vi=variance, data=Post )
Study_avaragePost<- aggregate(Post2, cluster= Study_ID, struct = "CS", rho= 0.6, na.rm=TRUE)

###At Follow up
Follow_up <- subset(Total, Follow_up == 1)
Follow_up2 <- escalc(yi=yi, vi=variance, data=Follow_up )
Study_avarageFollow_up<- aggregate(Follow_up2, cluster= Study_ID, struct = "CS", rho= 0.6, na.rm=TRUE)

#Main Analysis####

#_Unmoderated#####
Overall <- rma.mv(yi = yi, V = variance, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall)

# RVE Correction

mfor_CR2 <- vcovCR(Overall, type = "CR2")
OverallRVE<- coef_test(Overall, vcov = mfor_CR2, test = ("Satterthwaite"))
OverallRVECI<- conf_int(Overall, vcov = mfor_CR2)

#_Mod: Thoughts vs. Acts.#####

Overall.T <- rma.mv(yi =yi, V=variance, mods = ~ Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.T)


#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.T)

#RVE
mfor_CR2 <- vcovCR(Overall.T, type = "CR2")
Overall.TRVE <- coef_test(Overall.T, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TRVECI<- conf_int(Overall.T, vcov = mfor_CR2)

Overall.TRVE$p_Satt

#Changing reference population to derive CI of thoughts
Total$Acting<- Total$Thinking -1
Total$Acting <- Total$Acting*(-1)

Overall.T2 <- rma.mv(yi=yi, V=variance, mods = ~ Acting, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.T2)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.T)

#RVE
mfor_CR2 <- vcovCR(Overall.T2, type = "CR2")
Overall.T2RVE <- coef_test(Overall.T2, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.T2RVECI<- conf_int(Overall.T2, vcov = mfor_CR2)

#_Mod: Controlgroup (TAU vs. waitlist and Attention_Placebo)####

OverallC <- rma.mv(yi=yi, V=variance, mods =~TAU,  random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(OverallC)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(OverallC)

# Robust Variance estimation 

mfor_CR2 <- vcovCR(OverallC, type = "CR2")
Overall.CRVE<- coef_test(OverallC, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.CRVECI <- conf_int(OverallC, vcov = mfor_CR2)

#_Exploratory Analysis: TAU+Thinking#####

Overall.TT <- rma.mv(yi=yi, V=variance, mods= ~ Thinking+TAU, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.TT)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.TT)

#RVE
mfor_CR2 <- vcovCR(Overall.TT, type = "CR2")
Overall.TTRVE <- coef_test(Overall.TT, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TTRVECITT<- conf_int(Overall.TT, vcov = mfor_CR2)

#_Mod:Autonomous vs. Human involved#####

Overall.I <- rma.mv(yi=yi, V=variance, mods= ~Autonomous, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.I)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.I)

#RVE Correction
mfor_CR2 <- vcovCR(Overall.I, type = "CR2")
Overall.IRVE<- coef_test(Overall.I, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.IRVECI <- conf_int(Overall.I, vcov = mfor_CR2)

# Inverse groups and repeat analysis, to extract intercept of autonomous

Total$Human<- Total$Autonomous -1
Total$Human <- Total$Human*(-1)

Overall.I2 <- rma.mv(yi=yi,V=variance, mods= ~Human, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.I2)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.I2)

#RVE Correction
mfor_CR2 <- vcovCR(Overall.I2, type = "CR2")
Overall.I2RVE<- coef_test(Overall.I2, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.I2RVECI <- conf_int(Overall.I2, vcov = mfor_CR2)

#_Mod: Time (Follow_up vs. Post_Intervention)#####

OverallF <- rma.mv(yi=yi, V=variance, mods= ~Follow_up, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(OverallF)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(OverallF)

# Robust Variance estimation 

mfor_CR2 <- vcovCR(OverallF, type = "CR2")
Overall.FCRVE<- coef_test(OverallF, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.FCRVECI <- conf_int(OverallF, vcov = mfor_CR2)

# Sensitivty Analysis####

# NSSI Included 
Total2 <- read_delim("03_Data.csv", 
                    ",", escape_double = FALSE, trim_ws = TRUE)

Overall.T2 <- rma.mv(yi=yi, V=variance, mods= ~ Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total2, method="REML")
summary(Overall.T2)

#RVE Correction
mfor_CR2 <- vcovCR(Overall.T2, type = "CR2")
Overall.TRVE2<- coef_test(Overall.T2, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TRVECI2<- conf_int(Overall.T2, vcov = mfor_CR2)

# Suicide Excluded
Excl.Suicide <- subset(Total, Suicide == 0)

Overall.TS <- rma.mv(yi=yi, V=variance, mods= ~Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Excl.Suicide, method="REML")
summary(Overall.TS)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.TS)

#RVE
mfor_CR2 <- vcovCR(Overall.TS, type = "CR2")
Overall.TSRVE <- coef_test(Overall.TS, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TSRVECI<- conf_int(Overall.TS, vcov = mfor_CR2)


#Plotting####

#__Funnel Plot

#Defining pch

for (i in 1:nrow(Total)) {
  TAU       <- Total$TAU
  Thinking  <- Total$Thinking
  
  if(TAU[i] ==1 && Thinking[i] == 1) {
    Total$pch[i] <- 1}
  else if (TAU[i] ==0 && Thinking[i] == 1) {
    Total$pch[i] <- 0}
  else if (TAU[i] == 0 && Thinking[i] == 0) {
    Total$pch[i] <- 15}
  else if (TAU[i] ==1 && Thinking[i] == 0) {
    Total$pch[i] <- 19}
  
}

# Funnel Plot

funnel(Overall, pch =Total$pch, legend= TRUE, xlab = "Cohen's d")


#Introducing legend
gendA<- legend(x = "topright",
               legend= c("TAU vs. Sucidal Ideation","TAU vs. Sucidal Behaviour","WL/AP vs. Sucidal Ideation", "WL/AP vs. Sucidal Behaviour"),
               pch= c(1,19,0,15))

# Forest Plot with (inch 7x20)

with(Total, forest(yi, vi= variance, 
                   slab= paste(Author,Year, sep =", "),
                   xlab = ("SMD"),
                   xlim= c(-4.5,4.5), 
                   ylim= c(-1,121),
                   order =  Total$Thinking, rows =c(1:45,50:114),
                   cex= .75,                   
                   at= seq(-2,2),
                   efac = c(0.125,0.75),
                   header="Author(s) and Year"))

text((-2.5), c(117, 48), c("Suicidal Ideation","Suicidal Behaviours"), 
     cex = .75, font =2)

addpoly(x =OverallRVECI$beta[1], sei = OverallRVECI$SE[1], 
        cex = .75, ylim = 20, row = -1, annotate = FALSE)
addpoly(x =Overall.TRVECI$beta[1], sei = Overall.TRVECI$SE[1], 
        cex = .75, ylim = 21, row = -3, annotate = FALSE)
addpoly(x =Overall.T2RVECI$beta[1], sei = Overall.T2RVECI$SE[1], 
        cex = .75, ylim = 21, row = -5, annotate = FALSE)


text((3), c(-1, -3, -5), c("-0.12 [-0.16;-0.08]",
                           "-0.06 [-0.09;-0.03]","-0.17 [-0.24;-0.11]"), 
     cex = .75, font =2)

text((-2.5), c(-1,-3, -5), c("Unmoderated Model Average","Moderated Model (Suicidal Behaviour)"
                              ,"Moderated Model (Suicidal Ideation)"), cex = .75, font =2)


abline(h =c(1.5,3.5, 6.5, 7.5, 8.5,12.5, 14.5, 18.5,22.5,28.5,
            29.5,31.5,32.5,33.5, 36.5,42.5, 51.5, 52.5,53.5,
            55.5,57.5,59.5,63.5, 67.5,71.5,72.5,75.5,77.5,79.5,85.5,86.5,87.5,88.5,
            90.5,96.5,98.5,99.5,100.5,103.5, 105.5, 108.5), lty="dotted")
abline(h =0)



# GRADE SUBGROUP Analysis

# Deliberate Self Harm

data1 <- subset(Total, Type_Spesific == 'Deliberate Self Harm')
GRADE_1<- escalc(yi=yi, vi=variance, data=data1)
GRADE_AGGR_1<- aggregate(GRADE_1, cluster=Study_ID, struct= "CS" , rho = 0.6)
rma(GRADE_AGGR_1)

# Non-Suicidal Self Injury

data2 <- subset(Total2, Type_Spesific == 'Non-Suicidal Self Injury')
GRADE_2<- escalc(yi=yi, vi=variance, data=data2)
GRADE_AGGR_2<- aggregate(GRADE_2, cluster=Study_ID, struct= "CS" , rho = 0.6)

rma(GRADE_AGGR_2)




# Self Harm

data3 <- subset(Total, Type_Spesific == 'Self Harm')
GRADE_3<- escalc(yi=yi, vi=variance, data=data3)
GRADE_AGGR_3<- aggregate(GRADE_3, cluster=Study_ID, struct= "CS" , rho = 0.6)

rma(GRADE_AGGR_3)



# Suicidal Ideation

data4 <- subset(Total, Type_Spesific == 'Suicidal Ideation')
GRADE_4<- escalc(yi=yi, vi=variance, data=data4)
GRADE_AGGR_4<- aggregate(GRADE_4, cluster=Study_ID, struct= "CS" , rho = 0.6)

rma(GRADE_AGGR_4)


# Suicide Attempt

data5 <- subset(Total, Type_Spesific == 'Suicide Attempt')
GRADE_5<- escalc(yi=yi, vi=variance, data=data5)
GRADE_AGGR_5<- aggregate(GRADE_5, cluster=Study_ID, struct= "CS" , rho = 0.6)

rma(GRADE_AGGR_5)



# Suicidal Planning

data6 <- subset(Total, Type_Spesific == 'Suicidal Planning')
GRADE_6<- escalc(yi=yi, vi=variance, data=data6)
GRADE_AGGR_6<- aggregate(GRADE_6, cluster=Study_ID, struct= "CS" , rho = 0.6)

rma(GRADE_AGGR_6)


