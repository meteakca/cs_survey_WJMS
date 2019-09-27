#initiate the packages
  library(tidyr)#
  library(ggplot2)
  library(tidyverse)
  library(RColorBrewer)
  library(broom)
  library(devtools)
  library(ggrepel)
  library(sjPlot)
  library(interactions)
  library(tools)
  library(hrbrthemes)
  library(ggthemes)
  library(jtools)
  
#Impact of test and its interaction with Gender and Programming Extent: RQ1, 1a, 1b
#RQ1: Does taking the test impact students' perceptions? No
df_ss %>%
    pivot_longer(cols = contains("Mean_1"),
                 names_to = "Mot",
                 values_to = "Means") %>%
    ggplot() +
    geom_boxplot(aes(
      x = as.factor(Test),
      y = Means,
      fill = as.factor(Test)
    )) +
    facet_wrap(Mot ~ ., ncol = 1) +
    theme_ipsum() +
    ggtitle("Impact of taking the test on motivation outcomes")

 lm1 <- lm(Int_Mean_1R ~ Test, data = df_ss)
  summary(lm1)
  glance(lm1)

#RQ1a: Tests interaction with Gender.
  df_ss %>%
    pivot_longer(cols = ends_with("Mean_1"),
                 names_to = "Mot",
                 values_to = "Means") %>%
    drop_na() %>%
    ggplot(aes(y = Means, x = as.factor(Test))) +
    geom_boxplot(aes(fill = Gender)) +
    facet_wrap(Mot ~ ., ncol = 1) +
    facet_wrap(Mot ~ .,
               ncol = 1,
               labeller = labeller(key = labels_outcome)) +
    theme_ipsum_ps() +
    ylab("Outcome Levels") +
    xlab("No Test Condition ------- Test Condition") +
    ggtitle("Interaction of Test and Gender on \nMotivation Outcomes")

#Regression for gender X test interaction 
#Interest not significant
  lm1a <- lm(Int_Mean_1R ~ Test + Gender + Test * Gender, data = df_ss)
  summ(lm1a)
  plot_model(lm1a, type = "int")

#Value - not significant
  lm1a2 <-
    lm(Val_Mean_1 ~ Test + Gender + Test * Gender, data = df_ss)
  summ(lm1a2)
  plot_model(lm1a2, type = "int")

#Expectancy - not signficant
  lm1a3 <- lm(Exp_Mean_1 ~ Test + Gender + Test * Gender, data = df_ss)
  summ(lm1a3)
  plot_model(lm1a3, type = "int")

  plot_summs(lm1a, lm1a2, lm1a3, plot.distributions = TRUE)
  export_summs(lm1a, lm1a2, lm1a3, to.file = "docx", file.name = "test.docx", pvals = TRUE)

#RQ1b: Tests interaction with Programming Experience
  p <- df_ss %>%
    pivot_longer(cols = contains("Mean_1"),
                 names_to = "Mot",
                 values_to = "Means") %>%
    drop_na() %>%
    ggplot(aes(y = Means, x = Test, na.rm = TRUE)) +
    geom_boxplot(aes(fill = as.factor(ProgramingExtent))) +
    facet_wrap(Mot ~ ., ncol = 1) +
    theme_minimal() +
    guides(fill = FALSE) +
    ylab("Outcome Levels") +
    xlab("No Test Condition ------- Test Condition") +
    ggtitle("Interaction of Programming Extent and Test on \nMotivation Outcomes")

  
#Regression for programming extent X test interaction
#Interest not significant
  lm1b <-
    lm(Int_Mean_1R ~ Test + ProgramingExtent + Test * ProgramingExtent, data = df_ss)
  summ(lm1b)
  plot_model(lm1b, type = "int")
  
  #Value - not significant
  lm1b2 <-
    lm(Val_Mean_1 ~ as.numeric(Test) + ProgramingExtent + Test * ProgramingExtent, data = df_ss)
  summ(lm1b2)
  plot_model(lm1b2, type = "int")
  
  #Expectancy - not signficant
  lm1b3 <-
    lm(Exp_Mean_1 ~ Test + ProgramingExtent + Test * ProgramingExtent, data = df_ss)
  summ(lm1b3)
  plot_model(lm1b3, type = "int")
  

#RQ2: Does the programming experience impact outcomes?
  df_ss %>%
    pivot_longer(cols = contains("Mean_1"),
                 names_to = "Mot",
                 values_to = "Means") %>%
    drop_na() %>%
    ggplot(aes(x = Means)) +
    geom_boxplot(aes(
      x = ProgramingExtent,
      y = Means,
      fill = as.factor(ProgramingExtent)
    )) +
    guides(fill = FALSE) +
    facet_wrap(Mot ~ .,
               ncol = 1,
               labeller = labeller(key = labels_outcome)) +
    theme_ipsum_rc() +
    ylab("Motivation Outcome Level") +
    xlab("None - A little - Some - A lot \nProgramming Experience Reported") +
    ggtitle("Impact of programming experience (self-report) \non motivation outcomes")

#Regression for Interest, Significant
lm2 <- lm(Int_Mean_1R ~ ProgramingExtent, data = df_ss)
summ(lm2)

#Regression for Expectancy, Significant
lm2_2 <- lm(Exp_Mean_1 ~ ProgramingExtent, data = df_ss)
summ(lm2_2)  

#Regression for Value, Significant
lm2_3 <- lm(Val_Mean_1 ~ ProgramingExtent, data = df_ss)
summ(lm2_2)  


#RQ2b: Gender interaction with Programming Experience
  df_ss %>%
    pivot_longer(cols = contains("Mean_1"),
                 names_to = "Mot",
                 values_to = "Means") %>%
    drop_na() %>%
    ggplot(aes(y = Means, x = Gender, na.rm = TRUE)) +
    geom_boxplot(aes(fill = as.factor(ProgramingExtent))) +
    facet_wrap(Mot ~ ., ncol = 1) +
    theme_minimal() +
    ylab("Outcome Levels") +
    ggtitle("Interaction of Programming Extent and Gender on \nMotivation Outcomes")

#interactions Gender & Programming Extent
#Interest - not significant
  lm2b_1 <- lm(Int_Mean_1R ~ ProgramingExtent + Gender + Gender * ProgramingExtent, data = df_ss)
  summ(lm2b_1)
  interact_plot(lm2b_1, pred = ProgramingExtent, modx = Gender)

#Value - not significant
  lm2b_2 <- lm(Val_Mean_1 ~ ProgramingExtent + Gender + Gender * ProgramingExtent, data = df_ss)
  summ(lm2b_2)
  interact_plot(lm2b_2, pred = ProgramingExtent, modx = Gender)

#Expectancy - not significant
  lm2b_3 <- lm(Exp_Mean_1 ~ ProgramingExtent + Gender + Gender * ProgramingExtent, data = df_ss)
  summ(lm2b_3)
  interact_plot(lm2b_3, pred = ProgramingExtent, modx = Gender)