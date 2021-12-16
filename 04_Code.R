# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)
library("tidyverse")
library(metaviz)

?profile
# Data Importation & Preparation ####

Total <- read_delim("03_Data.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)

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

OverallC <- rma.mv(yi=yi, V=variance, mods =~TAU, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
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
                    ";", escape_double = FALSE, trim_ws = TRUE)

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
    Total$pch[i] <- 5}
  else if (TAU[i] == 0 && Thinking[i] == 0) {
    Total$pch[i] <- 18}
  else if (TAU[i] ==1 && Thinking[i] == 0) {
    Total$pch[i] <- 20}
  
}

# Funnel Plot
funnel(Overall, pch =Total$pch, legend= TRUE, xlab = "Cohens d")

#Introducing legend
gendA<- legend(x = "topright",
               legend= c("TAU+Thinking","TAU+Acting","Non-TAU+Thinking", "Non-TAU+Acting"),
               pch= c(1,20,5,18))

# Forest Plot

with(Total, forest(yi, vi= variance, 
                   slab= paste(Author,Year, sep =";"),
                   xlab = ("SMD"),
                   xlim= c(-2,2), 
                   ylim= c(-3,120),
                   order =  Total$Thinking, rows =c(1:46,50:113),
                   cex= .75,                   at= seq(-1.25,1.25),
                   header="Author(s) and Year"))

text((-1.8), c(116, 48), c("Suicidal Thoughts","Suicidal Behaviours"), cex = .75, font =2)

addpoly(x =OverallRVECI$beta[1], sei = OverallRVECI$SE[1], cex = .75, ylim = 20, row = -1)
addpoly(x =Overall.TRVECI$beta[1], sei = Overall.TRVECI$SE[1], cex = .75, ylim = 21, row = -3)
addpoly(x =Overall.T2RVECI$beta[1], sei = Overall.T2RVECI$SE[1], cex = .75, ylim = 21, row = -5)


text((-1.25), c(-1,-3, -5), c("Unmoderated Model Average","Moderated Model Average (Behaviour)"
                              ,"Moderated Model Average (Thoughts)"), cex = .75, font =2)

abline(h =c(1.5, 3.5,6.5,7.5,8.5,12.5,14.5,18.5,22.5,28.5,29.5,31.5,32.5,33.5,36.5,42.5,45.5,46.5,
            50.5,51.5,52.5,54.5,56.5,58.5,62.5,66.5,70.5,71.5,74.5,76.5,78.5,84.5,85.5,86.5,87.5,89.5,95.5,97.5,98.5,
            99.5,102.5,104.5,107.5,113.5), lty="dotted")
