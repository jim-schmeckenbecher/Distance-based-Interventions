# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)
library("tidyverse")
library(metaviz)
# Data Importation & Preparation ####

Total <- read_delim("03_Data.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)

Total <- subset(Total, NSSI ==0 )



#Study Aggregate Data for descriptive Statistics

# Independend of time
Total2 <- escalc(yi=d, vi=v, data=Total )
Study_avarage<- aggregate(Total2, cluster= Study_ID, struct = "CS", rho= 0.6, na.rm=TRUE)

#At Post 
Post <- subset(Total, Follow_up == 0)
Post2 <- escalc(yi=d, vi=v, data=Post )
Study_avaragePost<- aggregate(Post2, cluster= Study_ID, struct = "CS", rho= 0.6, na.rm=TRUE)

# At Follow up
Follow_up <- subset(Total, Follow_up == 1)
Follow_up2 <- escalc(yi=d, vi=v, data=Follow_up )
Study_avarageFollow_up<- aggregate(Follow_up2, cluster= Study_ID, struct = "CS", rho= 0.6, na.rm=TRUE)

# n at Postintervention
Postn<- sum(Study_avaragePost$n)


#Main Analysis####

#_Unmoderated#####
Overall <- rma.mv(d, v, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall)


#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall)

# RVE Correction

mfor_CR2 <- vcovCR(Overall, type = "CR2")
OverallRVE<- coef_test(Overall, vcov = mfor_CR2, test = ("Satterthwaite"))
OverallRVECI<- conf_int(Overall, vcov = mfor_CR2)

#_Mod: Thoughts vs. Acts.#####

Overall.T <- rma.mv(d, v, mods = ~ Type, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.T)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.T)


#RVE
mfor_CR2 <- vcovCR(Overall.T, type = "CR2")
Overall.TRVE <- coef_test(Overall.T, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TRVECI<- conf_int(Overall.T, vcov = mfor_CR2)

# Changing reference population to derive CI of thoughts
Total$Acting<- Total$Thinking -1

Total$Acting <- Total$Acting*(-1)


Overall.T2 <- rma.mv(d, v, mods = ~ Acting, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.T2)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.T)


#RVE
mfor_CR2 <- vcovCR(Overall.T2, type = "CR2")
Overall.T2RVE <- coef_test(Overall.T2, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.T2RVECI<- conf_int(Overall.T2, vcov = mfor_CR2)


#_Mod: Controlgroup (TAU vs. waitlist and Attention_Placebo)####

OverallC <- rma.mv(d,v, mods =~TAU, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(OverallC)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(OverallC)

# Robust Variance estimation 

mfor_CR2 <- vcovCR(OverallC, type = "CR2")
Overall.CRVE<- coef_test(OverallC, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.CRVECI <- conf_int(OverallC, vcov = mfor_CR2)

#_Mod:Autonomous vs. Human involved#####

Overall.I <- rma.mv(d,v, mods= ~Autonomous, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
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


Overall.I2 <- rma.mv(d,v, mods= ~Human, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.I2)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.I2)

#RVE Correction
mfor_CR2 <- vcovCR(Overall.I2, type = "CR2")
Overall.I2RVE<- coef_test(Overall.I2, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.I2RVECI <- conf_int(Overall.I2, vcov = mfor_CR2)




#_Mod: Time (Follow_up vs. Post_Intervention)#####

OverallF <- rma.mv(d,v, mods= ~Follow_up, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(OverallF)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(OverallF)

# Robust Variance estimation 

mfor_CR2 <- vcovCR(OverallF, type = "CR2")
Overall.FCRVE<- coef_test(OverallF, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.FCRVECI <- conf_int(OverallF, vcov = mfor_CR2)

# Sensitivity Analysis####

# NSSI Included 
Total2 <- read_delim("03_Data.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)

Overall.T2 <- rma.mv(d,v, mods= ~ Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total2, method="REML")
summary(Overall.T2)

#RVE Correction
mfor_CR2 <- vcovCR(Overall.T2, type = "CR2")
Overall.TRVE2<- coef_test(Overall.T2, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TRVECI2<- conf_int(Overall.T2, vcov = mfor_CR2)

# Suicide Excluded
Excl.Suicide <- subset(Total, Suicide == 0)

Overall.TS <- rma.mv(d,v, mods= ~Thinking, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Excl.Suicide, method="REML")
summary(Overall.TS)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.TS)

#RVE
mfor_CR2 <- vcovCR(Overall.TS, type = "CR2")
Overall.TSRVE <- coef_test(Overall.TS, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TSRVECI<- conf_int(Overall.TS, vcov = mfor_CR2)

#_Exploratory Analysis: TAU+Thinking#####

Overall.TT <- rma.mv(d,v, mods= ~ Thinking+TAU, random= ~1 |Study_ID/Outcome_ID,tdist = TRUE, data=Total, method="REML")
summary(Overall.TT)

#Optimization Control (Remove # to run)
#par(mfrow=c(2,4))
#profile(Overall.TT)

#RVE
mfor_CR2 <- vcovCR(Overall.TT, type = "CR2")
Overall.TRVE <- coef_test(Overall.TT, vcov = mfor_CR2, test = ("Satterthwaite"))
Overall.TRVECITT<- conf_int(Overall.TT, vcov = mfor_CR2)

#plotting: Funnel Plot

#Defining pch
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

#basic plot
funnel(Overall, pch =Total$pch, legend= TRUE, xlab = "Cohens d")

#Introducing legend
gendA<- legend(x = "topright",
               legend= c("TAU+Thinking","TAU+Acting","Non-TAU+Thinking", "Non-TAU+Acting"),
               pch= c(1,20,5,18))


# Caterpiller Plot  

forest(Overall.T,
       xlim=c(-2.5,3.5),        ### adjust horizontal plot region limits
       order="obs",             ### order by size of yi
       slab=NA, annotate=FALSE,### remove study labels and annotations
       efac=0,                  ### remove vertical bars at end of CIs
       pch= ifelse(Total$Thinking < '1', 20,1),                  ### changing point symbol to filled circle
       psize=4,                 ### increase point size
       cex.lab=1, cex.axis=1,   ### increase size of x-axis title/labels
       lty=c("solid","blank"))  ### remove horizontal line at top of plot

legendPrePost<- legend(x = "right",
                       legend= c("Thinking","Behaviour"),
                       pch= c(1,20))


viz_forest(x =Overall.T)

# Aggregated by Thougths and Acts 

Thinks<- subset(Total, Total$Thinking ==1)
Thinks <- escalc(yi=d, vi=v, data = Thinks)
Total.Thinking<-  aggregate(Thinks, cluster = Study_ID, struct = "CS", rho =0.6)

Acts<- subset(Total, Total$Thinking ==0)
Acts <- escalc(yi=d, vi=v, data = Acts)
Total.Acts <- aggregate(Acts, cluster = Study_ID, struct = "CS", rho =0.6)

Study_avarage<- rbind(Total.Thinking,Total.Acts)

Study_avarage$Study_ID <- 1:45

Avarage_effect<- rma(Study_avarage, mods=Thinking)

Study_avarage$Year<- format(round(Study_avarage$Year,1), nsmall=1)

Study_avarage$ST_ID <- paste(Study_avarage$Author, "(",Study_avarage$Year,")",sep="")


study_table <- data.frame(
  Study = Study_avarage$ST_ID,
  Intervention = Study_avarage$Intervention,
  Country = Study_avarage$Country)


viz_forest(Avarage_effect,
           xlab= "SMD",
           group = Study_avarage$Thinking,
           summary_label=c("Summary (Acting)", "Summary (Thinking)"),
           summary_table = c("Summary (suicide behaviours)", "Summary(suicidal thoughts)"),
           study_labels =Study_avarage$ST_ID, 
           study_table = study_table,
           annotate_CI = TRUE)



