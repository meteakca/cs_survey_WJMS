df_ss$Int_Mean_1R <-
  (df_ss$Int1 + df_ss$Int2 + df_ss$Int5 + df_ss$Int6++df_ss$Int7) / 5

df_ss$ProgramingExtent <- as.numeric(df_ss$ProgramingExtent)
labels_outcome <-
  c(Exp_Mean_1 = "Efficacy Mean",
    Int_Mean_1 = "Interest Mean",
    Val_Mean_1 = "Value Mean")


df_ss$Int_Mean_1R[122] <- 4
df_ss$Int_Mean_1R[123] <- 5.6
df_ss$Int_Mean_1R[146] <- 3

hist(df_ss$Int_Mean_1R)
hist(df_ss$Val_Mean_1)
hist(df_ss$Exp_Mean_1)


df_ss$DBNandNoTest[211] <- "highperformance"
df_ss$DBNandNoTest[212] <- "highperformance"
df_ss$DBNandNoTest[213] <- "highperformance"
df_ss$DBNandNoTest[214] <- "highperformance"
df_ss$DBNandNoTest[215] <- "highperformance"
df_ss$DBNandNoTest[216] <- "highperformance"

df_ss[df_ss$DBNandNoTest == "notest", "DBNandNoTest"] <- 0
df_ss[df_ss$DBNandNoTest == "lowperformance", "DBNandNoTest"] <- 1
df_ss[df_ss$DBNandNoTest == "moderateperformance", "DBNandNoTest"] <- 2
df_ss[df_ss$DBNandNoTest == "highperformance", "DBNandNoTest"] <- 3


df_ss$Int_Mean_1R <- as.numeric(df_ss$Int_Mean_1R)
df_ss$Int_Mean_1 <- as.numeric(df_ss$Int_Mean_1)
df_ss$Val_Mean_1 <- as.numeric(df_ss$Val_Mean_1)
df_ss$Exp_Mean_1 <- as.numeric(df_ss$Exp_Mean_1)
df_ss$ProgramingExtent <- as.numeric(df_ss$ProgramingExtent)


df_ss$ProgramingExtent <- factor(
  df_ss$ProgramingExtent,
  levels = c(0, 1, 2, 3),
  labels = c("None", "A little", "Some", "A lot")
)

df_ss$Test <- factor(df_ss$Test,
                     levels = c(0, 1),
                     labels = c("No Test", "Test"))


df_ss$ProgramingExtent <- as.numeric(df_ss$ProgramingExtent)
df_ss$DBN_POMP <- as.numeric(df_ss$DBN_POMP)
df_ss$PerfAndControl <- as.factor(df_ss$PerfAndControl)


df_ss$Test <- as.numeric(df_ss$Test)
df_ss$Test[df_ss$Test == "NA"] <- "0"
df_ss$Test[df_ss$Test == "Test"] <- "1"
df_ss$Test[is.na(df_ss$Test)] <- 0

#cleaning up the environment, removing old models
rm(lm3, lm3a, lm3b, m1, m2, m3, m4, m5, m5a, m5b, m6, m6a, m6b, p, lm3c) 

#create a new variable
df_ss$PerfAndControl <- ifelse(df_ss$DBN_POMP<18, 1, 2)
df_ss$PerfAndControl[is.na(df_ss$PerfAndControl)] <- 0

summary(df_ss)
DataExplorer::plot_bar(df_ss)

write.csv(df_ss, file = "surveyCS.csv")



#reliability of the pretest
df_ss$DBN1 <-  as.numeric(df_ss$DBN1)
df_ss$DBN2 <-  as.numeric(df_ss$DBN2)
df_ss$DBN3 <-  as.numeric(df_ss$DBN3)
df_ss$DBN4 <-  as.numeric(df_ss$DBN4)
df_ss$DBN5 <-  as.numeric(df_ss$DBN5)
df_ss$DBN6 <-  as.numeric(df_ss$DBN6)

pretest <- select(df_ss, 6,7,8,9,10,11)
alpha(pretest) 
