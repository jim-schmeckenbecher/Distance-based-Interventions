# Loading Libraries

library(metafor)
library(readr)
library(clubSandwich)
library(ggplot2)
library("tidyverse")

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

