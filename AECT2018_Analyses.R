#load the packages
  library(tidyr)
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
  library(huxtable)
  library(psych)
#Descriptives

  ggplot(data = subset(df_ss, !is.na(df_ss$ProgramingExtent)), aes(x = as.factor(ProgramingExtent)))+
    geom_bar(aes(fill=ProgramingExtent), show.legend = FALSE)+
    scale_x_discrete(labels = c('None','A little','Some','A lot'))+
    labs(x="", y="Count",
         title="",
         subtitle="",
         caption="") +
    theme(text = element_text(size=20))

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
    #facet_wrap(Mot ~ ., ncol = 1) +
  labs(x="Experimental group (0 = Control, 1 = Test)", y="Motivation Mean (1 to 7)",
       title="Impact of taking the test on motivation outcomes",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") +
  theme_ipsum()

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
    facet_wrap(Mot ~ .,
               ncol = 1) +
    theme_ipsum_ps() +
    ylab("Outcome Levels") +
    xlab("No Test Condition ------- Test Condition") +
    ggtitle("Interaction of Test and Gender on \nMotivation Outcomes")
  
  #RQ1a: Tests performance interaction with Gender.
  df_ss %>%
    pivot_longer(cols = ends_with("Mean_1"),
                 names_to = "Mot",
                 values_to = "Means") %>%
    drop_na() %>%
    ggplot(aes(y = Means, x = DBN_POMP)) +
    geom_point(aes(color = Gender)) +
    facet_wrap(Mot ~ .,
               ncol = 1) +
    theme_ipsum_ps() +
    ylab("Outcome Levels") +
    xlab("No Test Condition ------- Test Condition") +
    ggtitle("Interaction of Test and Gender on \nMotivation Outcomes")
    
#RQ3 Gender performance interaction - self-efficacy
  df_ss <- df_ss %>%
    filter(!is.na(Gender))
  ggplot(df_ss) +
    aes(x = Exp_Mean_1, y = DBN_POMP, colour = Gender) +
    geom_jitter(size = 4.96, alpha = 0.8) +
    geom_smooth(method = lm, se = FALSE) +
    scale_colour_grey() +
    labs(x = "Self-efficacy", y = "Pretest Performance") +
    theme_minimal() +
    theme(text = element_text(size = 20))

#RQ3 Gender performance interaction - UV
  df_ss <- df_ss %>%
    filter(!is.na(Gender))
  ggplot(df_ss) +
    aes(x = Val_Mean_1, y = DBN_POMP, colour = Gender) +
    geom_jitter(size = 4.96, alpha = 0.7) +
    #geom_boxplot()+
    geom_smooth(method = lm, se = FALSE) +
   # scale_colour_grey() +
    labs(x = "Utility Value", y = "Pretest Performance") +
    theme_minimal() +
    theme(text = element_text(size = 20))  
  

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
  genderTestregs <-
    huxreg(
      'Interest' = lm1a,
      'Value' = lm1a2,
      'Self-efficacy' = lm1a3,
      bold_signif = 0.05
    )
  genderTestregs                                                         %>%
    theme_article                                                  %>%
    set_background_color(1:nrow(genderTestregs), evens, grey(.95)) %>%
    set_font_size(final(), 1, 9)                                   %>%
    set_bold(final(), 1, FALSE)                                    %>%
    set_top_border(final(), 1, 1)                                  %>%
    set_caption('Linear regressions of motivation outcomes')      %>%
    quick_pdf(
      genderTestregs,
      file = "huxtable-output.pdf",
      borders = 0.4,
      open = interactive(),
      width = NULL,
      height = NULL
    )
  
#RQ1b: Tests interaction with Programming Experience
df_ss %>%
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
  

  
#Analysis for the test-takers, to see the impact of taking the test:
#DBN_POMP and motivation outcomes
  df_ss %>%
    pivot_longer(cols = contains("Mean_1"),
                 names_to = "Mot",
                 values_to = "Means") %>%
    ggplot(aes(x = DBN_POMP,
               y = Means)) +
    geom_boxplot(aes(color = DBN_POMP)) +
    facet_wrap(Mot ~ ., ncol = 1) +
    theme_minimal() +
    ggtitle("Impact of taking the test on motivation outcomes")

#interst
lm3 <- lm(Int_Mean_1R ~ DBN_POMP, data = df_ss)
summ(lm3)
#value
lm3a <- lm(Val_Mean_1 ~ DBN_POMP, data = df_ss)
summ(lm3a)
#expecanties
lm3b <- lm(Exp_Mean_1 ~ DBN_POMP, data = df_ss)
summ(lm3b)  


#DBN_POMP +Control and interactions
#first gender (filtering high pomp scores)
df_ss %>%
  filter(DBN_POMP<60) %>% 
  pivot_longer(cols = contains("Mean_1"),
               names_to = "Mot",
               values_to = "Means") %>%
  drop_na() %>% 
  ggplot(aes(x = DBN_POMP,
             y = Means)) +
  geom_boxplot(aes(fill = as.factor(Gender), color = as.factor(Gender))) +
  facet_wrap(Mot ~ ., ncol = 1) +
  theme_minimal() +
  ggtitle("Impact of taking the test on motivation outcomes")

#interst
lm3_1 <-
  lm(Int_Mean_1R ~ DBN_POMP + Gender + DBN_POMP * Gender, data = df_ss)
summ(lm3_1)
plot_model(lm3_1, type = "int")
interact_plot(lm3_1, pred = DBN_POMP, modx = Gender)

#value
lm3_1a <-
  lm(Val_Mean_1 ~ DBN_POMP + Gender + DBN_POMP * Gender, data = df_ss)
summ(lm3_1a)
plot_model(lm3_1a, type = "int")
interact_plot(lm3_1a, pred = DBN_POMP, modx = Gender)

#expecanties
lm3_1b <-
  lm(Exp_Mean_1 ~ DBN_POMP + Gender + DBN_POMP * Gender, data = df_ss)
summ(lm3_1b)
plot_model(lm3_1b, type = "int")
interact_plot(lm3_1b, pred = DBN_POMP, modx = Gender)

#programming experience
df_ss %>%
  pivot_longer(cols = contains("Mean_1"),
               names_to = "Mot",
               values_to = "Means") %>%
  drop_na() %>% 
  ggplot(aes(x = ProgramingExtent,
             y = Means)) +
  geom_boxplot(aes(fill = ProgramingExtent)) +
  facet_wrap(Mot ~ ., ncol = 1) +
  theme_minimal() +
  ggtitle("Impact of taking the test on motivation outcomes")

#interst
lm3_2 <-
  lm(Int_Mean_1R ~ DBN_POMP + ProgramingExtent + DBN_POMP * ProgramingExtent, data = df_ss)
summ(lm3_2)
plot_model(lm3_2, type = "int")
interact_plot(lm3_2, pred = DBN_POMP, modx = ProgramingExtent)

#value
lm3_2a <-
  lm(Val_Mean_1 ~ DBN_POMP  + ProgramingExtent + DBN_POMP * ProgramingExtent, data = df_ss)
summ(lm3_2a)
plot_model(lm3_2a, type = "int")
interact_plot(lm3_2a, pred = DBN_POMP, modx = ProgramingExtent)

#expecanties
lm3_2b <-
  lm(Exp_Mean_1 ~ DBN_POMP  + ProgramingExtent + DBN_POMP * ProgramingExtent, data = df_ss)
summ(lm3_2b)
plot_model(lm3_2b, type = "int")
interact_plot(lm3_2b, pred = DBN_POMP, modx = ProgramingExtent)



#programming experience + Gender x DBNPomp (3-way interaction)
df_ss %>%
  pivot_longer(cols = contains("Mean_1"),
               names_to = "Mot",
               values_to = "Means") %>%
  drop_na() %>% 
  ggplot(aes(x = DBN_POMP,
             y = Means)) +
  geom_boxplot(aes(color = Gender)) +
  facet_wrap(Mot ~ ProgramingExtent, ncol = 4) +
  theme_minimal() +
  ggtitle("Impact of taking the test on motivation outcomes")

#interst -continue here
lm3_3 <-
  lm(Int_Mean_1R ~ DBN_POMP + ProgramingExtent + Gender+ DBN_POMP * ProgramingExtent * Gender, data = df_ss)
summ(lm3_3)
plot_model(lm3_3, type = "pred")
interact_plot(lm3_3, pred = DBN_POMP, modx = ProgramingExtent)

#value
lm3_3a <-
  lm(Val_Mean_1 ~ DBN_POMP  + ProgramingExtent + Gender+ DBN_POMP * ProgramingExtent * Gender, data = df_ss)
summ(lm3_3a)
plot_model(lm3_3a, type = "int")
interact_plot(lm3_3a, pred = DBN_POMP, modx = ProgramingExtent)

#expecanties
lm3_3b <-
  lm(Exp_Mean_1 ~ DBN_POMP  + ProgramingExtent + Gender+ DBN_POMP * ProgramingExtent * Gender, data = df_ss)
summ(lm3_3b)
plot_model(lm3_3b, type = "int")
interact_plot(lm3_3b, pred = DBN_POMP, modx = ProgramingExtent)





#using the DBNandNoTest Variable
#DBNandNoTest ~ Interest - sig
lm4 <-
  lm(Int_Mean_1R ~ PerfAndControl, data = df_ss)
summ(lm4)
glance(lm4)
plot_model(lm4, type = "pred")

df_ss %>%
    pivot_longer(cols = contains("Mean_1"),
                 names_to = "Mot",
                 values_to = "Means") %>%
    ggplot(aes(y = Means, x = PerfAndControl)) +
    geom_boxplot(aes(fill = PerfAndControl)) +
    facet_wrap(Mot ~ ., ncol = 1) +
    theme_minimal() +
    ylab("Outcome Levels") +
    ggtitle("")

#anova for Tukey post hoc
aov1 <- aov(Int_Mean_1R ~ PerfAndControl, data = df_ss)
summary.aov(aov1)

#value - sig
lm4a <-
  lm(Val_Mean_1 ~ PerfAndControl, data = df_ss)
summ(lm4a)
glance(lm4a)
plot_model(lm4a, type = "pred")

#exp - not sig
lm4b <-
  lm(Exp_Mean_1 ~ PerfAndControl, data = df_ss)
summ(lm4b)
glance(lm4b)
plot_model(lm4b, type = "pred")

#interaction with gender - interest - significant
lm4_1 <-
  lm(Int_Mean_1R ~ PerfAndControl + Gender + Gender * PerfAndControl, data = df_ss)
summ(lm4_1)
glance(lm4_1)
plot_model(lm4_1, type = "int")

#value not sign
lm4_1a <-
  lm(Val_Mean_1 ~ PerfAndControl + Gender + Gender * PerfAndControl, data = df_ss)
summ(lm4_1a)
glance(lm4_1a)
plot_model(lm4_1a, type = "int")

#Expectancy not sign
lm4_1b <-
  lm(Exp_Mean_1 ~ PerfAndControl + Gender + Gender * PerfAndControl, data = df_ss)
summ(lm4_1b)
glance(lm4_1b)
plot_model(lm4_1b, type = "int")

huxreg(lm4_1, lm4_1a, lm4_1b)
