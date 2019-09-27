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


df_ss$Gender <- factor(df_ss$Gender,
                       levels = c(0, 1),
                       labels = c("Male", "Female"))


df_ss$ProgramingExtent <- factor(
  df_ss$ProgramingExtent,
  levels = c(0, 1, 2, 3),
  labels = c("None", "A little", "Some", "A lot")
)

df_ss$Test <- factor(df_ss$Test,
                     levels = c(0, 1),
                     labels = c("No Test", "Test"))


df_ss$ProgramingExtent <- as.numeric(df_ss$ProgramingExtent)

df_ss$Test <- as.numeric(df_ss$Test)
df_ss$Test[df_ss$Test == "NA"] <- "0"
df_ss$Test[df_ss$Test == "Test"] <- "1"
df_ss$Test[is.na(df_ss$Test)] <- 0
