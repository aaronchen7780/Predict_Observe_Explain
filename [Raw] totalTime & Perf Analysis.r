---
title: "[Raw] totalTime & Performance Analysis"
author: "Yu-An Chen"
date: "9/22/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, include = FALSE}
rm(list = ls())
library(devtools)
library(MASS)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyr)
library(stringr)
```
# Performance Formatting and Matching
Reading and Removing unnecessary variables 
```{r include = FALSE}
df <- read.csv("SocialSp21.csv")
groupings <- read.csv("spring2021_groupAssignment.csv")
df_clean <- df[c(1,20,21,22)]
df_clean <- df_clean[-c(1),]

```

Standardizing Scores via Z-scores
```{r include = FALSE}
stdzscore <- function(inputData){
  inputData$z.A <- 0
  inputData$z.B <- 0
  inputData$z.C <- 0
  for(i in 2:4){
    sd <- sd(inputData[,i])
    miu <- mean(inputData[,i])
    for(j in 1:nrow(inputData)){
      inputData[,i+3][j] = (inputData[,i][j] - miu) / sd 
    }
  }
  names(inputData)[5] <- "Condition.A"
  names(inputData)[6] <- "Condition.B"
  names(inputData)[7] <- "Condition.C"
  return(inputData[c(1,5,6,7)])
}

df_clean <- stdzscore(df_clean)

```

Joining the datasets & renaming columns 
```{r include = FALSE}
df_joined <- left_join(df_clean, groupings)

names(df_joined)[2] <- "Baseline"
```

Assigning each observation to their conditions
```{r include = FALSE}
df_joined$Text <- 0
df_joined$POE <- 0


assign_conditions <- function(input){
  for(i in 1:nrow(input)){
    if(input$Group[i] == "Group A"){
      input$Text[i] = input$Condition.B[i]
      input$POE[i] = input$Condition.C[i]
    }
    else{
      input$Text[i] = input$Condition.C[i]
      input$POE[i] = input$Condition.B[i]      
    }
  }
  return(input)
}

df_final <- assign_conditions(df_joined)
df_final <- df_final[c(1,2,6,7)]
```


## Reading data & Preliminary Cleaning
```{r, include = FALSE}
df <- read.csv('barPlot Data - Separated by semesters copy.csv')
df <- df[c(1,2,3,8)]

timeS21 <- read.csv('TimeData copy.csv')
timeS21 <- timeS21[c(1,2,3,8)]
names(timeS21)[2] <- "Semester"

df <- rbind(df, timeS21)

PerfF18 <- read.csv('F18Performance copy.csv')
PerfF19 <- read.csv('F19Performance copy.csv')
PerfS19 <- read.csv('S19Performance copy.csv')
PerfS20 <- read.csv('S20Performance copy.csv')
PerfS21 <- read.csv('S21Performance copy.csv')

PerfF18 <- na.omit(PerfF18)
PerfF19 <- na.omit(PerfF19)
PerfS19 <- na.omit(PerfS19)
PerfS20 <- na.omit(PerfS20)
PerfS21 <- na.omit(PerfS21)

df$diff_from_baseline <- 0
df$condition <- as.factor(df$condition)

names(PerfS21)[names(PerfS21) == "AnonID"] <- "Student"
```


```{r}
##Dealing with cases where participants didn't participate in all conditions
checkCount <- function(input){
  input$counter <- 0
  for (i in 1:nrow(input)){
    for (j in 1:nrow(input)){
      if (input$AnonID[i] == input$AnonID[j]){
        input$counter[i] = input$counter[i] + 1
      }
    }
  }
  return(subset(input, counter == 3))
}

df <- checkCount(df)


TimeF18 <- subset(df, Semester == "Fall2018")
TimeF19 <- subset(df, Semester == "Fall2019")
TimeS19 <- subset(df, Semester == "Spring2019")
TimeS20 <- subset(df, Semester == "Spring2020")
TimeS21 <- subset(df, Semester == "Spring2021")
```

```{r}
## Making sure both time and performance data have the same observations
dropMissingPerf <- function(timedata, perfdata){
  perfdata$keep <- 0 
  for(i in 1:nrow(perfdata) ){
    if(any(timedata$AnonID == perfdata$Student[i])){
      perfdata$keep[i] = 1
    }
  }
  return(subset(perfdata, keep == 1))
}

dropMissingTime <- function(timedata, perfdata){
  timedata$keep <- 0
    for(i in 1:nrow(timedata) ){
    if(any(perfdata$Student == timedata$AnonID[i])){
      timedata$keep[i] = 1
      }
    }
  return(subset(timedata, keep == 1))
}


PerfF18 <- dropMissingPerf(TimeF18, PerfF18)
PerfF19 <- dropMissingPerf(TimeF19, PerfF19)
PerfS19 <- dropMissingPerf(TimeS19, PerfS19)
PerfS20 <- dropMissingPerf(TimeS20, PerfS20)
PerfS21 <- dropMissingPerf(TimeS21, PerfS21)

TimeF18 <- dropMissingTime(TimeF18, PerfF18)
TimeF19 <- dropMissingTime(TimeF19, PerfF19)
TimeS19 <- dropMissingTime(TimeS19, PerfS19)
TimeS20 <- dropMissingTime(TimeS20, PerfS20)
TimeS21 <- dropMissingTime(TimeS21, PerfS21)

```

## Time Plots
```{r}

TimePlotF18 <- ggplot(TimeF18, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Fall 2018 Time")

TimePlotF19 <- ggplot(TimeF19, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Fall 2019 Time")

TimePlotS19 <- ggplot(TimeS19, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Spring 2019 Time")

TimePlotS20 <- ggplot(TimeS20, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Spring 2020 Time")

TimePlotS21 <- ggplot(TimeS21, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Spring 2021 Time")

TimePlotF18
TimePlotF19
TimePlotS19
TimePlotS20
TimePlotS21

```

## Time Significance testing: Creating difference from baseline values
```{r}

baseline_diff <- function(input, condA){
  input$diff_from_baseline <- 0
  for(i in 1:nrow(input)){
  if(input$condition[i] != condA){
    for(j in 1:nrow(input)){
      if(input$AnonID[i] == input$AnonID[j] &
         input$condition[j] == condA){
        baseVal = input$totalTime[j]
      }
    }
    
    input$diff_from_baseline[i] = 
      baseVal - input$totalTime[i]
    }
  }
  return(input)
}

TimeF18 <- baseline_diff(TimeF18, "Text")
TimeF19 <- baseline_diff(TimeF19, "Text")
TimeS19 <- baseline_diff(TimeS19, "Baseline")
TimeS20 <- baseline_diff(TimeS20, "Baseline")
TimeS21 <- baseline_diff(TimeS21, "Baseline")

```

# Time: Plots of difference from baseline
```{r}
TimeDiffPlot <- function(input, condA, condB, sem){
  a <- subset(input, condition == condA)
  b <- subset(input, condition == condB)
  final <- data.frame(Type = c(paste("Baseline -", condA, sep = " ") , paste("Baseline -", condB, sep = " ")), diff_from_baseline = c(mean(a$diff_from_baseline), mean(b$diff_from_baseline)))
  
  return(ggplot(data=final, aes(x=Type, y=diff_from_baseline)) +
  geom_col(width = 0.55)+ labs(y="Time difference from Baseline (min)", x = "Group",
              title = paste("Avg. Difference from Baseline Condition", sem, sep = " ")))
  
}

TimeDiffPlot(TimeF18, "PO", "POE", "(Fall 2018)")
TimeDiffPlot(TimeF19, "POE", "POMC", "(Fall 2019)")
TimeDiffPlot(TimeS19, "POE", "Text", "(Spring 2019)")
TimeDiffPlot(TimeS20, "POE", "Text", "(Spring 2020)")
TimeDiffPlot(TimeS21, "POE", "Text", "(Spring 2021)")
```

# Time: Paired T-tests
```{r}
ttest <- function(input, condA, condB){
  first <- subset(input, condition == condA)
  second <- subset(input, condition == condB)
  return(t.test(first$diff_from_baseline, second$diff_from_baseline, paired = TRUE, alternative = "two.sided"))
}

ttest(TimeF18, "PO", "POE")
ttest(TimeF19, "POE", "POMC")
ttest(TimeS19, "POE", "Text")
ttest(TimeS20, "POE", "Text")
ttest(TimeS21, "POE", "Text")


```

# Time: Relevant LMER models
```{r}

lmerModel <- function(input, refcond){
  return(lmer(totalTime ~ condition + (1|AnonID), data = input))
}

TimeF18lmer <- lmer(totalTime ~ relevel(condition, ref = "Text") + (1 |AnonID), data= TimeF18)
summary(TimeF18lmer)

TimeF19lmer <- lmer(totalTime ~ relevel(condition, ref = "Text") + (1 |AnonID), data= TimeF19)
summary(TimeF19lmer)

TimeS19lmer <- lmer(totalTime ~ relevel(condition, ref = "Baseline") + (1 |AnonID), data= TimeS19)
summary(TimeS19lmer)

TimeS20lmer <- lmer(totalTime ~ relevel(condition, ref = "Baseline") + (1 |AnonID), data= TimeS20)
summary(TimeS20lmer)

TimeS21lmer <- lmer(totalTime ~ relevel(condition, ref = "Baseline") + (1 |AnonID), data= TimeS21)
summary(TimeS21lmer)

```

## Time: Extended Tests
```{r}
ttestExtended <- function(input, condA, condB){
  first <- subset(input, condition == condA)
  second <- subset(input, condition == condB)
  return(t.test(first$totalTime, second$totalTime, paried = TRUE, alternative = "two.sided"))
}

ttestExtended(TimeF19, "POE", "Text")
ttestExtended(TimeS19, "POE", "Text")
ttestExtended(TimeS20, "POE", "Text")

```


## Performance 

To prepare the data so that it becomes an acceptable df for the violin plots, we will transform the data from wide into long form. Also note that we are setting factor_key to TRUE in our gather function so that we can re-level our reference conditions in the later LMER tests.
```{r}
gather_to_long <- function(df, cond1, cond2, cond3){
  keycol <- "condition"
  valuecol <- "zscore"
  gathercols <- c(cond1, cond2, cond3)
  return (gather_(df, keycol, valuecol, gathercols, factor_key = TRUE))
}

PerfF18long <- gather_to_long(PerfF18, "PO", "POE", "Text")
PerfF19long <- gather_to_long(PerfF19, "POE", "PO...MC", "Text.Only")
PerfS19long <- gather_to_long(PerfS19, "Baseline", "Control", "Experimental")
PerfS20long <- gather_to_long(PerfS20, "baseline", "control", "experimental")
PerfS21long <- gather_to_long(PerfS21, "Baseline", "Text", "POE")
```


## Performance: Plotting Distributions

We will first plot the distributions of the data using a violin plot to determine if we will need to further deal with outliers or do data transformations

Fall 2018 Densities: 
```{r}
F18Violin <- ggplot(PerfF18long,aes(x=condition, y = zscore, color = condition)) + 
  geom_violin() + labs(title="F18Violin")
F18Violin
```

Fall 2019 Densities:
```{r}
F19Violin <- ggplot(PerfF19long,aes(x=condition, y = zscore, color = condition)) + 
  geom_violin() + labs(title="F19Violin")
F19Violin
```

Spring 2019 Densities:
```{r}
S19Violin <- ggplot(PerfS19long,aes(x=condition, y = zscore, color = condition)) + 
  geom_violin() + labs(title="S19Violin")
S19Violin
```

Spring 2020 Densities:
```{r}
S20Violin <- ggplot(PerfS20long,aes(x=condition, y = zscore, color = condition)) + 
  geom_violin() + labs(title="S20Violin")
S20Violin
```
Spring 2021 Densities:
```{r}
S21Violin <- ggplot(PerfS21long,aes(x=condition, y = zscore, color = condition)) + 
  geom_violin() + labs(title="S21Violin")
S21Violin
```

## Performance: Creating Results

To find out how many z-scores away from the mean each condition results for the students, we will take the average of the z-scores in each condition for each semester

```{r}

ResultsF18 <- data.frame(Type = c("PO", "POE", "Text"), Avg = c(mean(PerfF18$PO), mean(PerfF18$POE), mean(PerfF18$Text)))

ResultsF19 <- data.frame(Type = c("POMC", "POE", "Text"), Avg = c(mean(PerfF19$PO...MC), mean(PerfF19$POE), mean(PerfF19$Text)))

ResultsS19 <- data.frame(Type = c("Baseline", "Control", "Experimental"), Avg = c(mean(PerfS19$Baseline), mean(PerfS19$Control), mean(PerfS19$Experimental)))

ResultsS20 <- data.frame(Type = c("Baseline", "Control", "Experimental"), Avg = c(mean(PerfS20$baseline), mean(PerfS20$control), mean(PerfS20$experimental)))

ResultsS21 <- data.frame(Type = c("Baseline", "Text", "POE"), Avg = c(mean(PerfS21$Baseline), mean(PerfS21$Text), mean(PerfS21$POE)))


F18plot <- ggplot(data=ResultsF18, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Fall 2018)")

F19plot <- ggplot(data=ResultsF19, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Fall 2019)")

S19plot <- ggplot(data=ResultsS19, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Spring 2019)")

S20plot <- ggplot(data=ResultsS20, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Spring 2020)")

S21plot <- ggplot(data=ResultsS21, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Spring 2021)")




F18plot
F19plot
S19plot
S20plot
S21plot
```

## Performance: Difference From Baseline
Here we will conduct both a simple two sample t-test and an lmer regression to determine if the differences from the baseline are significant

**Creating differences columns**
```{r}
make_differences <- function(df, baselineCol){
  df$diffFromBase1 <- 0
  df$diffFromBase2 <- 0
  if(baselineCol == 4){
    for(i in 1:nrow(df)){
      df$diffFromBase1[i] = df[baselineCol][i,] - df[2][i,]
      df$diffFromBase2[i] = df[baselineCol][i,] - df[3][i,]
    }
  }
  else if(baselineCol == 2){
    for(i in 1:nrow(df)){
      df$diffFromBase1[i] = df[baselineCol][i,] - df[3][i,]
      df$diffFromBase2[i] = df[baselineCol][i,] - df[4][i,]
    }
  }
  return(df)
}


PerfF18 <- make_differences (PerfF18, 4)
PerfF19 <- make_differences (PerfF19, 4)
PerfS19 <- make_differences (PerfS19, 2)
PerfS20 <- make_differences (PerfS20, 2)
PerfS21 <- make_differences (PerfS21, 2)
```

**Plots of the differences**

```{r}

diffResultsF18 <- data.frame(Type = c("Baseline - PO", "Baseline - POE"), Avg = c(mean(PerfF18$diffFromBase1), mean(PerfF18$diffFromBase2)))

diffResultsF19 <- data.frame(Type = c("Baseline - POMC", "Baseline - POE"), Avg = c(mean(PerfF19$diffFromBase1), mean(PerfF19$diffFromBase2)))

diffResultsS19 <- data.frame(Type = c("Baseline - Control", "Baseline - Experimental"), Avg = c(mean(PerfS19$diffFromBase1), mean(PerfS19$diffFromBase2)))

diffResultsS20_ <- data.frame(Type = c("Baseline - Control", "Baseline - Experimental"), Avg = c(mean(PerfS20$diffFromBase1), mean(PerfS20$diffFromBase2)))

diffResultsS21_ <- data.frame(Type = c("Baseline - Control", "Baseline - Experimental"), Avg = c(mean(PerfS21$diffFromBase1), mean(PerfS21$diffFromBase2)))


diffPlotF18 <- ggplot(data=diffResultsF18, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="baseline - condition", x = "Group",
              title = "Z-score Difference from Baseline (Fall 2018)")

diffPlotF19 <- ggplot(data=diffResultsF19, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="baseline - condition", x = "Group",
              title = "Z-score Difference from Baseline (Fall 2019)")

diffPlotS19 <- ggplot(data=diffResultsS19, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="baseline - condition", x = "Group",
              title = "Z-score Difference from Baseline (Spring 2019)")

diffPlotS20 <- ggplot(data=diffResultsS20_, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="baseline - condition", x = "Group",
              title = "Z-score Difference from Baseline (Spring 2020)")

diffPlotS21 <- ggplot(data=diffResultsS21_, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="baseline - condition", x = "Group",
              title = "Z-score Difference from Baseline (Spring 2021)")

diffPlotF18
diffPlotF19
diffPlotS19
diffPlotS20
diffPlotS21
```

## Performance: Paired t-tests

Below are the summaries of the paired t-tests. None of the performance differences are significant at the alpha = 0.05 level.
```{r}
t.test(PerfF18$diffFromBase1, PerfF18$diffFromBase2, paired = TRUE, alternative = "two.sided")
t.test(PerfF19$diffFromBase1, PerfF19$diffFromBase2, paired = TRUE, alternative = "two.sided")
t.test(PerfS19$diffFromBase1, PerfS19$diffFromBase2, paired = TRUE, alternative = "two.sided")
t.test(PerfS20$diffFromBase1, PerfS20$diffFromBase2, paired = TRUE, alternative = "two.sided")
t.test(PerfS21$diffFromBase1, PerfS21$diffFromBase2, paired = TRUE, alternative = "two.sided")
```

## Performance: Condition LMER
Now for further analysis, we will run a linear mixed-effect model on the differences between the differences from the baseline conditions. To do this, we will need to apply the new changes to our long data format and remove the baseline conditions so our comparison can be made on the differences from the baseline.
```{r}

PerfF18long <- gather_to_long(PerfF18, "PO", "POE", "Text")
PerfF19long <- gather_to_long(PerfF19, "POE", "PO...MC", "Text.Only")
PerfS19long <- gather_to_long(PerfS19, "Baseline", "Control", "Experimental")
PerfS20long <- gather_to_long(PerfS20, "baseline", "control", "experimental")
PerfS21long <- gather_to_long(PerfS21, "Baseline", "Text", "POE")

remove_base <- function(input, baseCond){
  return(subset(input, condition != baseCond))
}

F18lmer <- lmer(zscore ~  relevel(condition, ref = "Text") + (1 |Student), data= PerfF18long)
summary(F18lmer)


F19lmer <- lmer(zscore ~ relevel(condition, ref = "Text.Only") + (1 |Student), data= PerfF19long)
summary(F19lmer)


S19lmer <- lmer(zscore ~ condition + (1 |Student), data= PerfS19long)
summary(S19lmer)


S20lmer <- lmer(zscore ~ condition + (1 |Student), data= PerfS20long)
summary(S20lmer)

S21lmer <- lmer(zscore ~ condition + (1 |Student), data= PerfS21long)
summary(S21lmer)

```
# Comparison with Analyses.R
```{r}
createTimeZ <- function(df){
  pretest <- subset(df, condition == "Text")
  rest <- subset(df, condition != "Text")
  pretest$ztotalTime <- scale(pretest$totalTime, center = T, scale = T)
  rest$ztotalTime <- scale(rest$totalTime, center = T, scale = T)
  return (rbind(pretest, rest))
}

F18Groups <- read.csv("18F_Groups.csv")
F18Groups$Group <- 0
for(i in 1:nrow(F18Groups)){
  if (F18Groups$Condition[i] == "87862"){
    F18Groups$Group[i] = "A"
  }
  else if (F18Groups$Condition[i] == "37526"){
    F18Groups$Group[i] = "B"
  }
  else if (F18Groups$Condition[i] == "32001"){
    F18Groups$Group[i] = "C"
  }
  else if (F18Groups$Condition[i] == "31549"){
    F18Groups$Group[i] = "D"
  }
  else if (F18Groups$Condition[i] == "28103"){
    F18Groups$Group[i] = "E"
  }
  else{
    F18Groups$Group[i] = "F"
  }
}
names(F18Groups)[1] = "Student"

TimeF18 <- createTimeZ(TimeF18)
names(TimeF18)[1] = "Student"
PerfCompareLong <- merge(PerfF18long, TimeF18,by = c("Student", "condition"))
names(TimeF18)[1] = "AnonID"

modifyForAnalysis <- function(df){
  final <- subset(df, condition != "Text")
  df$Z_preTestPerf <- 0 
  df$Z_preTestTime <- 0
  for (i in 1:nrow(final)){
    row_num = which(df$condition == "Text" & df$Student == final$Student[i])
    final$Z_preTestPerf[i] = df$zscore[row_num]
    final$Z_preTestTime[i] = df$ztotalTime[row_num]
  }
  return(final)
}
PerfCompareLong <- modifyForAnalysis(PerfCompareLong)
PerfCompareLong <- merge(PerfCompareLong, F18Groups, by = "Student")


zm0 = lm(formula = zscore ~ Z_preTestTime + Z_preTestPerf + Group + condition*ztotalTime, data=PerfCompareLong)
summary(zm0)
```

```{r}
zm0b = lmer(formula = zscore ~ Z_preTestTime + Z_preTestPerf + Group + condition*ztotalTime + (1|Student),data=PerfCompareLong)
summary(zm0b)
```

```{r}
zm0b = lmer(formula = zscore ~ Z_preTestPerf + Group + condition + (1|Student),data=PerfCompareLong)
summary(zm0b)
```

```{r}
summary(lm(zscore ~ Z_preTestPerf, data = PerfCompareLong))
plot(zscore ~ Z_preTestPerf, data = PerfCompareLong)
```

# Linear Models predicting zQuizScore:
```{r}
F18Quiz1 <- subset(read.csv("F18QuizQuestionsExam1.csv"), select = c(1,8))
F18Quiz2 <- subset(read.csv("F18QuizQuestionsExam2.csv"), select = c(1,8))
F18Quiz3 <- subset(read.csv("F18QuizQuestionsExam3.csv"), select = c(1,8))

PerfCompareLong <- subset(PerfCompareLong, select= c(1,2,19,20,21,22,23))
names(F18Groups)[1] = "AnonID"
F18Quiz <- merge(na.omit(merge(merge(F18Quiz1, F18Quiz2), F18Quiz3)), F18Groups)
rm(F18Quiz1, F18Quiz2, F18Quiz3)

addQuiz <- function(df, quizInfo, baseCond, controlCond, expCond){
  df$quizScore <- 0
  for(i in 1:nrow(df)){
    row_num <- which(quizInfo$AnonID == df$Student[i])
    if(df$condition[i] == baseCond){
      if(quizInfo$Condition[row_num] == '32001' | quizInfo$Condition[row_num] == '24807' ){
        df$quizScore[i] = quizInfo$SUM1[row_num]
      }
      else if (quizInfo$Condition[row_num] == '37526' | quizInfo$Condition[row_num] == '28103' ){
        df$quizScore[i] = quizInfo$SUM2[row_num]        
      }
      else if (quizInfo$Condition[row_num] == '31549' | quizInfo$Condition[row_num] == '87862' ){
        df$quizScore[i] = quizInfo$SUM3[row_num]   
      }
    }
    else if(df$condition[i] == controlCond){
      if(quizInfo$Condition[row_num] == '87862' | quizInfo$Condition[row_num] == '28103' ){
        df$quizScore[i] = quizInfo$SUM1[row_num]   
      }
      else if (quizInfo$Condition[row_num] == '31549' | quizInfo$Condition[row_num] == '24807' ){
        df$quizScore[i] = quizInfo$SUM2[row_num]       
      }
      else if (quizInfo$Condition[row_num] == '37526' | quizInfo$Condition[row_num] == '32001'){
        df$quizScore[i] = quizInfo$SUM3[row_num]
      }
    }
    else if(df$condition[i] == expCond){
      if(quizInfo$Condition[row_num] == '31549' | quizInfo$Condition[row_num] == '37526' ){
        df$quizScore[i] = quizInfo$SUM1[row_num]   
      }
      else if (quizInfo$Condition[row_num] == '32001' | quizInfo$Condition[row_num] == '87862' ){
        df$quizScore[i] = quizInfo$SUM2[row_num]       
      }
      else if (quizInfo$Condition[row_num] == '24807' | quizInfo$Condition[row_num] == '28103'){
        df$quizScore[i] = quizInfo$SUM3[row_num]
      }
    }
  }
  df$zQuizScore<- scale(df$quizScore, center = T, scale = T)
  return (df)
}

PerfCompareLong <- addQuiz(PerfCompareLong, F18Quiz, "Text", "PO", "POE")
```

```{r}
zm1 = lm(formula = zQuizScore ~ Z_preTestTime + Z_preTestPerf + Group + condition*ztotalTime, data=PerfCompareLong)
summary(zm1)
```

```{r}
zm1b = lmer(formula = zQuizScore ~ Z_preTestTime + Z_preTestPerf + Group + condition*ztotalTime + (1|Student), data=PerfCompareLong)
summary(zm1b)
```

```{r}
zm1c = lmer(formula = zQuizScore ~ Z_preTestPerf + Group + condition + (1|Student), data=PerfCompareLong)
summary(zm1b)
```

```{r}
zm3 = lmer(formula = ztotalTime ~ Z_preTestTime + Group + condition + (1|Student), data=PerfCompareLong)
summary(zm3)
```



# Performance: Z-Score differences from baseline LMER
We will now look at another LMER analysis, where we test for the differences in zscore differences from the baseline conditions. To do this, we will first extract the values (diffFromBase1 and diffFromBase2) from our  performance data sets, then populate the new dataframe with the differences from baseline, conditions, and the Student IDs. 
```{r}

new <- function(input, condA, condB){
  arr <- NULL
  arrlen <- 1
  for(i in 1:nrow(input)){
    arr[arrlen] <- input[i,ncol(input)-1]
    arr[arrlen + 1] <- input[i,ncol(input)]
    arrlen <- arrlen + 2
  }
  final <- data.frame(zscore_deviation = c(arr))
  final$condition <- 0
  final$Student <- 0
  
  for(j in 1:nrow(final)){
    if(j%%2 == 1){
      final$condition[j] = condA
      final$Student[j] = input$Student[(j/2) + 1]
    }
    else{
      final$condition[j] = condB
      final$Student[j] = input$Student[j/2]
    }
  }
  return(final)
}

ZDiff_F18 <- new(PerfF18, "PO", "POE")
ZDiff_F19 <- new(PerfF19, "POMC", "POE")
ZDiff_S19 <- new(PerfS19, "Control", "Experimental")
ZDiff_S20 <- new(PerfS20, "Control", "Experimental")
ZDiff_S21 <- new(PerfS21, "Text", "POE")

ZD_F18Results <- lmer(zscore_deviation ~  condition + (1 |Student), data= ZDiff_F18)
summary(ZD_F18Results)

ZD_F19Results <- lmer(zscore_deviation ~  condition + (1 |Student), data= ZDiff_F19)
summary(ZD_F19Results)

ZD_S19Results <- lmer(zscore_deviation ~  condition + (1 |Student), data= ZDiff_S19)
summary(ZD_S19Results)

ZD_S20Results <- lmer(zscore_deviation ~  condition + (1 |Student), data= ZDiff_S20)
summary(ZD_S20Results)

ZD_S21Results <- lmer(zscore_deviation ~  condition + (1 |Student), data= ZDiff_S21)
summary(ZD_S21Results)

```
## Performance: Extended t-tests
We would like to know a little bit about the differences between POE and text conditions in Fall 2019 and Spring 2020 because the questions given in the PO+MC conditions may have been lacking in overall quality. We will run standard t-tests between the two conditions in these semesters
```{r}
t.test(PerfF19$POE, PerfF19$Text.Only, paired = TRUE, alternative = "two.sided")
t.test(PerfS20$control, PerfS20$experimental, paired = TRUE, alternative = "two.sided")
```


## Data Analysis: Gain per minute

Now we want to normalize performance by time to determine how much "performance" students earn for every minute in their condition. To do this, we will divide each student's z-score performance by the completion time


```{r}
calculate_gains <- function(perfdata, timedata){
  perfdata$gain_per_min <- 0
  column <- 0
  for(i in 1:nrow(perfdata)){
    for(j in 1:nrow(timedata)){
      if(perfdata$Student[i] == timedata$AnonID[j]){
        column = j
      }
    }
    perfdata$gain_per_min[i] = perfdata$zscore[i]/timedata$totalTime[column]
  }
  return(perfdata)
}

PerfF18long <- gather_to_long(PerfF18, "PO", "POE", "Text")
PerfF19long <- gather_to_long(PerfF19, "POE", "PO...MC", "Text.Only")
PerfS19long <- gather_to_long(PerfS19, "Baseline", "Control", "Experimental")
PerfS20long <- gather_to_long(PerfS20, "baseline", "control", "experimental")
PerfS21long <- gather_to_long(PerfS21, "Baseline", "Text", "POE")

PerfF18long <- calculate_gains(PerfF18long, TimeF18)
PerfF19long <- calculate_gains(PerfF19long, TimeF19)
PerfS19long <- calculate_gains(PerfS19long, TimeS19)
PerfS20long <- calculate_gains(PerfS20long, TimeS20)
PerfS21long <- calculate_gains(PerfS21long, TimeS21)

```

To see the average gains across conditions, we will create barplots for visualization.

```{r}

a1<-mean(subset(PerfF18long, condition == "PO")$gain_per_min)
a2<-mean(subset(PerfF18long, condition == "POE")$gain_per_min)
a3<-mean(subset(PerfF18long, condition == "Text")$gain_per_min)

a4<-mean(subset(PerfF19long, condition == "POE")$gain_per_min)
a5<-mean(subset(PerfF19long, condition == "PO...MC")$gain_per_min)
a6<-mean(subset(PerfF19long, condition == "Text.Only")$gain_per_min)

a7<-mean(subset(PerfS19long, condition == "Baseline")$gain_per_min)
a8<-mean(subset(PerfS19long, condition == "Control")$gain_per_min)
a9<-mean(subset(PerfS19long, condition == "Experimental")$gain_per_min)

a10<-mean(subset(PerfS20long, condition == "baseline")$gain_per_min)
a11<-mean(subset(PerfS20long, condition == "control")$gain_per_min)
a12<-mean(subset(PerfS20long, condition == "experimental")$gain_per_min)

a13<-mean(subset(PerfS21long, condition == "Baseline")$gain_per_min)
a14<-mean(subset(PerfS21long, condition == "Text")$gain_per_min)
a15<-mean(subset(PerfS21long, condition == "POE")$gain_per_min)


perfResultsF18 <- data.frame(Type = c("PO", "POE", "Text"), Avg = c(a1, a2, a3))
perfResultsF19 <- data.frame(Type = c("POE", "POMC", "Text"), Avg = c(a4, a5, a6))
perfResultsS19 <- data.frame(Type = c("Baseline", "Control(Text)", "Experimental (POE)"), Avg = c(a7, a8, a9))
perfResultsS20 <- data.frame(Type = c("Baseline", "Control(Text)", "Experimental (POE)"), Avg = c(a10, a11, a12))
perfResultsS21 <- data.frame(Type = c("Baseline", "Control(Text)", "Experimental (POE)"), Avg = c(a13, a14, a15))


perfPlotF18 <- ggplot(data=perfResultsF18, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Fall 2018 )")

perfPlotF19 <- ggplot(data=perfResultsF19, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Fall 2019 )")

perfPlotS19 <- ggplot(data=perfResultsS19, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Spring 2019 )")

perfPlotS20 <- ggplot(data=perfResultsS20, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Spring 2020 )")


perfPlotS21 <- ggplot(data=perfResultsS21, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Spring 2021 )")

perfPlotF18
perfPlotF19
perfPlotS19
perfPlotS20
perfPlotS21
```

## T-tests: Gain per minute
Now we will see if the gains per minute are significantly different between the conditions

```{r}

gain_diff_create<- function(df, baseline){
  df$gain_diff <- 0
  for(i in 1:nrow(df)){
    if(df$condition[i] != baseline){
      checkRow = which(df$condition == baseline & df$Student == df$Student[i])
      df$gain_diff[i] = df$gain_per_min[checkRow] - df$gain_per_min[i]
    }
  }
  return(df)
}



PerfF18long <- gain_diff_create(PerfF18long, "Text")
PerfF19long <- gain_diff_create(PerfF19long, "Text.Only")
PerfS19long <- gain_diff_create(PerfS19long, "Baseline")
PerfS20long <- gain_diff_create(PerfS20long, "baseline")
PerfS21long <- gain_diff_create(PerfS21long, "Baseline")

po <- subset(PerfF18long, condition == "PO")
poe <- subset(PerfF18long, condition == "POE")
t.test(po$gain_diff, poe$gain_diff, paired = TRUE, alternative = "two.sided")

pomc <- subset(PerfF19long, condition == "PO...MC")
poe <- subset(PerfF19long, condition == "POE")
t.test(poe$gain_diff, pomc$gain_diff, paired = TRUE, alternative = "two.sided")

control <- subset(PerfS19long, condition == "Control")
experimental <- subset(PerfS19long, condition == "Experimental")
t.test(control$gain_diff, experimental$gain_diff, paired = TRUE, alternative = "two.sided")

control <- subset(PerfS20long, condition == "control")
experimental <- subset(PerfS20long, condition == "experimental")
t.test(control$gain_diff, experimental$gain_diff, paired = TRUE, alternative = "two.sided")

control <- subset(PerfS21long, condition == "Text")
experimental <- subset(PerfS21long, condition == "POE")
t.test(control$gain_diff, experimental$gain_diff, paired = TRUE, alternative = "two.sided")

```

# Gain Per Min: LMER
```{r}
PerfF18long <- remove_base(PerfF18long, "Text")
PerfF19long <- remove_base(PerfF19long, "Text.Only")
PerfS19long <- remove_base(PerfS19long, "Baseline")
PerfS20long <- remove_base(PerfS20long, "baseline")
PerfS21long <- remove_base(PerfS21long, "Baseline")


F18lmer_gain <- lmer(gain_diff ~ condition + (1 |Student), data= PerfF18long)
summary(F18lmer_gain)


F19lmer_gain <- lmer(gain_diff ~ condition + (1 |Student), data= PerfF19long)
summary(F19lmer_gain)


S19lmer_gain <- lmer(gain_diff ~ condition + (1 |Student), data= PerfS19long)
summary(S19lmer_gain)


S20lmer_gain <- lmer(gain_diff ~ condition + (1 |Student), data= PerfS20long)
summary(S20lmer_gain)

S21lmer_gain <- lmer(gain_diff ~ condition + (1 |Student), data= PerfS21long)
summary(S21lmer_gain)


```
