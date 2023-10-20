### Required packages
library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad
library(table1) #for descriptives


############################ DESCRIPTIVES######################
Descriptives <- read_excel("ECC_FMD.xlsx",
                           sheet = "Descriptives")
View(Descriptives)
attach(Descriptives)

Descriptives <- Descriptives %>%
  rename(HRmax = HR, Lactatemax=Lactate,RPEmax=RPE)

#ALL
Descriptives <- Descriptives %>%
  rename(HRmax = HR, Lactatemax=Lactate,RPEmax=RPE)

Descriptives <- Descriptives %>%  select("Age","Height","Weight",
                                         "VO2max","HRmax","Lactatemax","RPEmax")

Descriptives %>%
  describe(na.rm=T, skew=FALSE, ranges=F)%>% mutate_if(is.numeric, round, 2) %>%
  kbl(caption = "Descriptives All participants") %>%
  kable_classic(full_width = F, html_font = "Cambria")

################ V3 #########
Df <- read_excel("Eccentric_Thesis.xlsx",
                 sheet = "ESS_BFP")
View(Df)
attach(Df)

Df <- left_join(Df,Descriptives_max, by ="ID")

Df <- Df %>% rename(VO2 = VO )

Df <- Df %>% mutate(VO2_perc = VO2 / VO2max * 100,
                    Lactate_perc = Lactate / Lactatemax * 100,
                    HR_perc = HR / HRmax * 100,
                    RPE_perc = RPE / RPEmax * 100)

## Convert from character to factor data
Df$Sex <- as.factor(Df$Sex)
Df$Condition <- as.factor(Df$Condition)

## Order conditions
Df$Condition <- ordered(Df$Condition,
                        levels = c("Baseline", "Low",
                                   "Moderate","High"))
