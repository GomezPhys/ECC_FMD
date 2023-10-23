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

table1(~ FMD_Basal + FMD_Basal_2 + FMD_AbsoluteChange + FMD_AbsoluteChange_2 + FMD_Difference + FMD_Difference_2 | Sex*Condition,
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
Df %>% group_by(Condition) %>% shapiro_test(FMD)

Df %>% group_by(Condition) %>% shapiro_test(FMD_Basal)
Df %>% group_by(Condition) %>% shapiro_test(FMD_Peak) 
Df %>% group_by(Condition) %>% shapiro_test(FMD_Difference)

## Wilcoxon for Non-parametric data
wilcox.test(FMD ~ Condition, data = Df2)

## Effect size
Df2  %>% wilcox_effsize(FMD ~ Condition)

## Figure FMD
FMD <- ggboxplot(Df2, x = "Condition", y = "FMD",
                 color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Pre", "Post"),
                 ylab = "FMD", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 28)
FMD
ggsave("FMD.png")





## FMD BASAL t-test

## T-test for parametric data
t_test(FMD_Basal ~ Condition, data = Df2)

## Effect size
Df2  %>% cohens_d(FMD_Basal ~ Condition,
                  paired = TRUE, hedges.correction = TRUE)

## Figure FMD
FMD_Basal <- ggboxplot(Df2, x = "Condition", y = "FMD_Basal",
                       color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                       order = c("Pre", "Post"),
                       ylab = "FMD Basal", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "t.test", paired = F,
                     label.x = 1.4,
                     label.y = 4.7)
FMD_Basal
ggsave("FMD_Basal.png")


## FMD PEAK t-test

## T-test for parametric data
t_test(FMD_Peak ~ Condition, data = Df2)

## Effect size
Df2  %>% cohens_d(FMD_Peak ~ Condition,
                  paired = TRUE, hedges.correction = TRUE)

## Figure FMD
FMD_Peak <- ggboxplot(Df, x = "Condition", y = "FMD_Peak",
                      color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                      order = c("Pre", "Post"),
                      ylab = "FMD Peak", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "t.test", paired = F,
                     label.x = 1.4,
                     label.y = 5)
FMD_Peak
ggsave("FMD_Peak.png")

########## FMD difference

## wilcoxon for parametric data
wilcox.test(FMD_Difference ~ Condition, data = Df)

## Effect size
Df %>% wilcox_effsize(FMD_Difference ~ Condition)

ggsave("FMD_Difference.png")



######FMD_DIFFERENCE

FMD_Difference <- ggboxplot(Df, x = "Condition", y = "FMD_Difference",
                            color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                            order = c("Pre", "Post"),
                            ylab = "FMD Difference", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 27)
FMD_Difference

FMD_Difference_2 <- ggboxplot(Df, x = "Condition", y = "FMD_Difference_2",
                            color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                            order = c("Pre", "Post"),
                            ylab = "FMD Difference_2", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 27)
FMD_Difference_2

####FMD_ABSOLUTE CHNAGE###

FMD_AbsoluteChange <- ggboxplot(Df, x = "Condition", y = "FMD_AbsoluteChange",
                            color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                            order = c("Pre", "Post"),
                            ylab = "FMD_AbsoluteChange", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 4)
FMD_AbsoluteChange


###FMD_Absolute change 2###
FMD_AbsoluteChange_2 <- ggboxplot(Df, x = "Condition", y = "FMD_AbsoluteChange_2",
                                color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                                order = c("Pre", "Post"),
                                ylab = "FMD_AbsoluteChange_2", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 4)
FMD_AbsoluteChange_2


#####FMD_BASALINE
FMD_Basal <- ggboxplot(Df, x = "Condition", y = "FMD_Basal",
                                  color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                                  order = c("Pre", "Post"),
                                  ylab = "FMD_Basal", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 5)
FMD_Basal

####FMD_Baseline_2

FMD_Basal_2 <- ggboxplot(Df, x = "Condition", y = "FMD_Basal_2",
                       color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                       order = c("Pre", "Post"),
                       ylab = "FMD_Basal_2", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 5)
FMD_Basal_2
