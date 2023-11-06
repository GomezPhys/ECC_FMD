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

Descriptives <- read_excel("~/MG_Final_ECC_FMD.xlsx", sheet = "Descriptives")
View(Descriptives)

Descriptives <- Descriptives %>%  select("Age","Height","Weight",
              "VO2max","HR","Lactate","RPE")

Descriptives %>%
  describe(na.rm=T, skew=FALSE, ranges=F)%>% mutate_if(is.numeric, round, 2) %>%
  kbl(caption = "Descriptives All participants") %>%
  kable_classic(full_width = F, html_font = "Cambria")

Descriptives$Sex <- as.factor(Descriptives$Sex)

# By Sex
describeBy(Descriptives ~ Sex, na.rm=T, skew=FALSE, ranges=F)

Df <- read_excel("~/MG_Final_ECC_FMD.xlsx", sheet = "AG")
View(Df)

## Convert from character to factor data
Df$Sex <- as.factor(Df$Sex)
Df$Condition <- as.factor(Df$Condition)

## Order conditions
Df$Condition <- ordered(Df$Condition,
                        levels = c("Pre", "Post"))

Df <-  select("Age","Height","Weight",
                                         "VO2max","HR","Lactate","RPE")

Df %>%
describe(na.rm=T, skew=FALSE, ranges=F)%>% mutate_if(is.numeric, round, 2) %>%
  kbl(caption = "Descriptives All participants") %>%
  kable_classic(full_width = F, html_font = "Cambria")


table1(~ Basal_Diameter + Basal_Diameter_2 + Peak + FMD_Percent + FMD_Percent_2 + Absolute_Difference_FMD_mm + Absolute_Difference_FMD_mm_2 + ESS_Basal + ESS_Hyp | Condition,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Df,
       render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)



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
Df %>% group_by(Condition) %>% shapiro_test(ESS_Basal) 
Df %>% group_by(Condition) %>% shapiro_test(ESS_Hyp)


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


## Effect size
Df  %>% wilcox_effsize(Basal_Diameter_2 ~ Condition)

## Figure Basal_Diameter
Basal_Diameter_2 <- ggboxplot(Df, x = "Condition", y = "Basal_Diameter_2",
                            color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                            order = c("Pre", "Post"),
                            ylab = "Basal (mm)", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 3.75)
Basal_Diameter_2



# Your existing code for Basal plot
Basal <- ggboxplot(Df, x = "Condition", y = "Basal",
                   color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                   order = c("Pre", "Post"),
                   ylab = "Basal (mm)", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = FALSE,
                     label.x = 1.4, label.y = 3.75)

# Your existing code for Peak plot
Peak <- ggboxplot(Df, x = "Condition", y = "Peak",
                  color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                  order = c("Pre", "Post"),
                  ylab = "Peak (mm)", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = FALSE,
                     label.x = 1.4, label.y = 4)

# Combine the plots into a single figure
combined_plot <- ggarrange(Basal, Peak, ncol = 2)

# Display the combined plot
combined_plot
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
                     label.y = 20)
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
                     label.y = 20)
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
                     label.y = .75)
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
                     label.y = .75)
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

###ESS
wilcox.test(ESS_Basal ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(ESS_Basal ~ Condition)

## Figure ESS_Basal
ESS_Basal <- ggboxplot(Df, x = "Condition", y = "ESS_Basal",
                       color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                       order = c("Pre", "Post"),
                       ylab = "ESS_Basal", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 100)
ESS_Basal

###ESShyp
wilcox.test(ESS_Hyp ~ Condition, data = Df)

## Effect size
Df  %>% wilcox_effsize(ESS_Hyp ~ Condition)

## Figure ESS_Hyp
ESS_Hyp <- ggboxplot(Df, x = "Condition", y = "ESS_Hyp",
                     color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                     order = c("Pre", "Post"),
                     ylab = "ESS_Hyp", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 100)
ESS_Hyp



## Effect size
Df  %>% cohens_d(FMD_Percent ~ Condition,
                  paired = TRUE, hedges.correction = TRUE)

Df  %>% cohens_d(FMD_Percent_2 ~ Condition,
                 paired = TRUE, hedges.correction = TRUE)

Df  %>% cohens_d(Basal_Diameter ~ Condition,
                 paired = TRUE, hedges.correction = TRUE)

Df  %>% cohens_d(Basal_Diameter_2 ~ Condition,
                 paired = TRUE, hedges.correction = TRUE)

Df  %>% cohens_d(Peak ~ Condition,
                 paired = TRUE, hedges.correction = TRUE)

Df  %>% cohens_d(Absolute_Difference_FMD_mm ~ Condition,
                 paired = TRUE, hedges.correction = TRUE)

Df  %>% cohens_d(Absolute_Difference_FMD_mm_2 ~ Condition,
                 paired = TRUE, hedges.correction = TRUE)

Df  %>% cohens_d(ESS_Basal ~ Condition,
                 paired = TRUE, hedges.correction = TRUE)

Df  %>% cohens_d(ESS_Hyp ~ Condition,
                 paired = TRUE, hedges.correction = TRUE)