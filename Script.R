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
Descriptives <- read_excel("~/ECC_FMD.xlsx",
                           sheet = "Descriptives")
View(Descriptives)
attach(Descriptives)

table1(~ Peak + Basal_Diameter + Basal_Diameter_2 + FMD_Percent + FMD_Percent + FMD_Percent_2 + Absolute_Difference_FMD_mm + Absolute_Difference_FMD_mm_2 + Allometric_FMD_Percent + Allometric_FMD_Percent_2 | Condition,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Df,
       render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)

table1(~ FMD_Basal + FMD_Basal_2 + FMD_AbsoluteChange + FMD_AbsoluteChange_2 + FMD_Difference + FMD_Difference_2 | Condition,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Df,
       render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)

################ V3 #########


Df <- read_excel("~/ECC_FMD.xlsx", sheet = "FMD")
View(Df)
attach(Df)


## Convert from character to factor data
Df$Sex <- as.factor(Df$Sex)
Df$Condition <- as.factor(Df$Condition)

## Order conditions
Df$Condition <- ordered(Df$Condition,
                        levels = c("Pre", "Post"))



## Shapiro Wilk
Df %>% group_by(Condition) %>% shapiro_test(Peak) 
Df %>% group_by(Condition) %>% shapiro_test(Basal_Diameter)
Df %>% group_by(Condition) %>% shapiro_test(Basal_Diameter_2)
Df %>% group_by(Condition) %>% shapiro_test(FMD_Percent)
Df %>% group_by(Condition) %>% shapiro_test(FMD_Percent_2)
Df %>% group_by(Condition) %>% shapiro_test(Absolute_Difference_FMD_mm)
Df %>% group_by(Condition) %>% shapiro_test(Absolute_Difference_FMD_mm_2)
Df %>% group_by(Condition) %>% shapiro_test(Allometric_FMD_Percent) 
Df %>% group_by(Condition) %>% shapiro_test(Allometric_FMD_Percent_2)


##### Wilcoxon for Non-parametric data Peak

wilcox.test(Peak ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(Peak ~ Condition)

## Figure Peak
Peak <- ggboxplot(Df, x = "Condition", y = "Peak",
                 color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Pre", "Post"),
                 ylab = "Peak (mm) ", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 5)
Peak

###Basal Diameter
wilcox.test(Basal_Diameter ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(Basal_Diameter ~ Condition)

## Figure Basal_Diameter
Basal_Diameter <- ggboxplot(Df, x = "Condition", y = "Basal_Diameter",
                 color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Pre", "Post"),
                 ylab = "Basal_Diameter (mm)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 5)
Basal_Diameter

###Basal Diameter_2
wilcox.test(Basal_Diameter_2 ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(Basal_Diameter_2 ~ Condition)

## Figure Basal_Diameter_2
Basal_Diameter_2 <- ggboxplot(Df, x = "Condition", y = "Basal_Diameter_2",
                            color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                            order = c("Pre", "Post"),
                            ylab = "Basal_Diameter_2 (mm)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 5)
Basal_Diameter_2

###FMD_Percent

wilcox.test(FMD_Percent ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(FMD_Percent ~ Condition)

## Figure FMD_Percent
FMD_Percent <- ggboxplot(Df, x = "Condition", y = "FMD_Percent",
                            color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                            order = c("Pre", "Post"),
                            ylab = "FMD (%)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 30)
FMD_Percent


####FMD_Percent_2


wilcox.test(FMD_Percent_2 ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(FMD_Percent_2 ~ Condition)

## Figure FMD_Percent
FMD_Percent_2 <- ggboxplot(Df, x = "Condition", y = "FMD_Percent_2",
                         color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                         order = c("Pre", "Post"),
                         ylab = "FMD (%)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 25)
FMD_Percent_2

###Absolute_Difference_FMD_mm

wilcox.test(Absolute_Difference_FMD_mm ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(Absolute_Difference_FMD_mm ~ Condition)

## Figure Absolute_Difference_FMD_mm
Absolute_Difference_FMD_mm <- ggboxplot(Df, x = "Condition", y = "Absolute_Difference_FMD_mm",
                         color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                         order = c("Pre", "Post"),
                         ylab = "Absolute Difference FMD (mm)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 1)
Absolute_Difference_FMD_mm


####Absolute_Difference_FMD_mm_2
wilcox.test(Absolute_Difference_FMD_mm_2 ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(Absolute_Difference_FMD_mm_2 ~ Condition)

## Figure Absolute_Difference_FMD_mm2
Absolute_Difference_FMD_mm_2 <- ggboxplot(Df, x = "Condition", y = "Absolute_Difference_FMD_mm_2",
                                        color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                                        order = c("Pre", "Post"),
                                        ylab = "Absolute Difference FMD (mm)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 1)
Absolute_Difference_FMD_mm_2

###Allometric_FMd_Percent
wilcox.test(Allometric_FMD_Percent ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(Allometric_FMD_Percent ~ Condition)

## Figure Allometric_FMD_Percent
Allometric_FMD_Percent <- ggboxplot(Df, x = "Condition", y = "Allometric_FMD_Percent",
                                        color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                                        order = c("Pre", "Post"),
                                        ylab = "Allometric_FMD (%)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 135)
Allometric_FMD_Percent

###Allometric_FMd_Percent_2
wilcox.test(Allometric_FMD_Percent_2 ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(Allometric_FMD_Percent_2 ~ Condition)

## Figure Allometric_FMD_Percent_2
Allometric_FMD_Percent_2 <- ggboxplot(Df, x = "Condition", y = "Allometric_FMD_Percent_2",
                                    color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                                    order = c("Pre", "Post"),
                                    ylab = "Allometric_FMD (%)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 130)
Allometric_FMD_Percent_2

