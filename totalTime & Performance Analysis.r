---
title: "totalTime & Performance Analysis"
author: "Yu-An Chen"
date: "10/21/2021"
output: html_document
---
# {.tabset}

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
library(readr)
```

## Performance Formatting and Matching
Reading and Removing unnecessary variables 
```{r}
df <- read.csv("SocialSp21.csv")
groupings <- read.csv("spring2021_groupAssignment.csv")
df_clean <- df[c(1,20,21,22)]
df_clean <- df_clean[-c(1),]

```

Standardizing Scores via Z-scores
```{r}
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
```{r}
df_joined <- left_join(df_clean, groupings)

names(df_joined)[2] <- "Baseline"
```

Assigning each observation to their conditions
```{r}
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
```{r}
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


df$timeOutlier <- 0
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

## Removing outliers
```{r}
exclude_outliers <- function(condData, destData){
  Q1 <- quantile(condData$totalTime, .25)
  Q3 <- quantile(condData$totalTime, .75)
  IQR <- IQR(condData$totalTime)
  upper <- Q3 + 1.5*IQR
  lower <- Q1 - 1.5*IQR
  
  for (i in 1:nrow(condData)){
    if (!(condData$totalTime[i] > lower & condData$totalTime[i] < upper)){
      index = which(destData$AnonID == condData$AnonID[i])
      for(j in 1:length(index)){
        change = index[j]
        destData$timeOutlier[change] = 1
      }
    }
  }
  return (destData)
}


po <- subset(TimeF18, condition == "PO")
poe <- subset(TimeF18,condition == "POE")
text <- subset(TimeF18, condition == "Text")

TimeF18 <- exclude_outliers(po, TimeF18)
TimeF18 <- exclude_outliers(poe, TimeF18)
TimeF18 <- exclude_outliers(text, TimeF18)


poe <- subset(TimeF19,condition == "POE")
pomc <- subset(TimeF19,condition == "POMC")
text <- subset(TimeF19,condition == "Text")

TimeF19 <- exclude_outliers(poe, TimeF19)
TimeF19 <- exclude_outliers(pomc, TimeF19)
TimeF19 <- exclude_outliers(text, TimeF19)


baseline <- subset(TimeS19,condition == "Baseline")
poe <- subset(TimeS19,condition == "POE")
text <- subset(TimeS19,condition == "Text")

TimeS19 <- exclude_outliers(baseline, TimeS19)
TimeS19 <- exclude_outliers(poe, TimeS19)
TimeS19 <- exclude_outliers(text, TimeS19)


baseline <- subset(TimeS20,condition == "Baseline")
poe <- subset(TimeS20,condition == "POE")
text <- subset(TimeS20,condition == "Text")

TimeS20 <- exclude_outliers(baseline, TimeS20)
TimeS20 <- exclude_outliers(poe, TimeS20)
TimeS20 <- exclude_outliers(text, TimeS20)


baseline <- subset(TimeS21, condition == "Baseline")
poe <- subset(TimeS21,condition == "POE")
text <- subset(TimeS21, condition == "Text")

TimeS21 <- exclude_outliers(baseline, TimeS21)
TimeS21 <- exclude_outliers(poe, TimeS21)
TimeS21 <- exclude_outliers(text, TimeS21)

```

Marking performance outliers 
```{r}
get_upper <- function(dfcondition){
    Q1 <- quantile(dfcondition, .25)
    Q3 <- quantile(dfcondition, .75)
    IQR <- IQR(dfcondition)
    upper <-Q3 + 1.5*IQR
    return(upper)
}

get_lower <- function(dfcondition){
    Q1 <- quantile(dfcondition, .25)
    Q3 <- quantile(dfcondition, .75)
    IQR <- IQR(dfcondition)
    lower <- Q3 - 1.5*IQR
    return(lower)
}

remove_outliers <- function(input, upper1, lower1, upper2, lower2, upper3, lower3){
  input <- na.omit(input)
  input$perfOutlier <- 0
  for(i in 1:nrow(input)){
    if (input[2][i,] > upper1 | input[2][i,] < lower1){
      input$perfOutlier[i] = 1
    }
    if (input[3][i,] > upper2 | input[3][i,] < lower2){
      input$perfOutlier[i] = 1
    }
    if (input[4][i,] > upper3 | input[4][i,] < lower3){
      input$perfOutlier[i] = 1
    }
  }
  return (input)
}


PerfF18 <- remove_outliers(PerfF18, get_upper(PerfF18$PO), get_lower(PerfF18$PO), get_upper(PerfF18$POE), get_lower(PerfF18$POE),  get_upper(PerfF18$Text), get_lower(PerfF18$Text))


PerfF19 <- remove_outliers(PerfF19, get_upper(PerfF19$PO...MC), get_lower(PerfF19$PO...MC), get_upper(PerfF19$POE), get_lower(PerfF19$POE),  get_upper(PerfF19$Text), get_lower(PerfF19$Text))


PerfS19 <- remove_outliers(PerfS19, get_upper(PerfS19$Baseline), get_lower(PerfS19$Baseline), get_upper(PerfS19$Control), get_lower(PerfS19$Control),  get_upper(PerfS19$Experimental), get_lower(PerfS19$Experimental))

PerfS20 <- remove_outliers(PerfS20, get_upper(PerfS20$baseline), get_lower(PerfS20$baseline), get_upper(PerfS20$control), get_lower(PerfS20$control),  get_upper(PerfS20$experimental), get_lower(PerfS20$experimental))

PerfS21 <- remove_outliers(PerfS21, get_upper(PerfS21$Baseline), get_lower(PerfS21$Baseline), get_upper(PerfS21$Text), get_lower(PerfS21$Text),  get_upper(PerfS21$POE), get_lower(PerfS21$POE))
```

Extracting outlier-removed datasets for both time and performance 
```{r}
cleanTime <- function(timedata, perfdata){
  timedata$overallOutlier <- 0
  for(i in 1:nrow(timedata)){
    if(timedata$timeOutlier[i] == 1){
      timedata$overallOutlier[i] = 1
    }
  }
  for(j in 1:nrow(perfdata)){
    if(perfdata$perfOutlier[j] == 1){
      index = which(timedata$AnonID == perfdata$Student[j])
      for(k in 1:length(index)){
        change = index[k]
        timedata$overallOutlier[change] = 1
      }
    }
  }
  return(subset(timedata, overallOutlier == 0))
}

cleanPerf <- function(timedata, perfdata){
  perfdata$overallOutlier <- 0
  for(i in 1:nrow(perfdata)){
    if(perfdata$perfOutlier[i] == 1){
      perfdata$overallOutlier[i] = 1
    }
  }
  for(j in 1:nrow(timedata)){
    if(timedata$timeOutlier[j] == 1){
      index = which(perfdata$AnonID == timedata$Student[j])
      for(k in 1:length(index)){
        change = index[k]
        perfdata$overallOutlier[change] = 1
      }
    }
  }
  return(subset(perfdata, overallOutlier == 0))
}

PerfF18_cleaned <- cleanPerf(TimeF18, PerfF18)
PerfF19_cleaned <- cleanPerf(TimeF19, PerfF19)
PerfS19_cleaned <- cleanPerf(TimeS19, PerfS19)
PerfS20_cleaned <- cleanPerf(TimeS20, PerfS20)
PerfS21_cleaned <- cleanPerf(TimeS21, PerfS21)

TimeF18_cleaned <- cleanTime(TimeF18, PerfF18)
TimeF19_cleaned <- cleanTime(TimeF19, PerfF19)
TimeS19_cleaned <- cleanTime(TimeS19, PerfS19)
TimeS20_cleaned <- cleanTime(TimeS20, PerfS20)
TimeS21_cleaned <- cleanTime(TimeS21, PerfS21)
```

## Time Plots
```{r}

TimePlotF18 <- ggplot(TimeF18_cleaned, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Fall 2018 excluding outliers")

TimePlotF19 <- ggplot(TimeF19_cleaned, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Fall 2019 excluding outliers")

TimePlotS19 <- ggplot(TimeS19_cleaned, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Spring 2019 excluding outliers")

TimePlotS20 <- ggplot(TimeS20_cleaned, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Spring 2020 excluding outliers")

TimePlotS21 <- ggplot(TimeS21_cleaned, aes(x = condition, y=totalTime, color = condition)) + 
  geom_violin() + labs(title="Spring 2021 excluding outliers")

TimePlotF18
TimePlotF19
TimePlotS19
TimePlotS20
TimePlotS21

```

## Time tests: Difference From Baseline
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

TimeF18_cleaned <- baseline_diff(TimeF18_cleaned, "Text")
TimeF19_cleaned <- baseline_diff(TimeF19_cleaned, "Text")
TimeS19_cleaned <- baseline_diff(TimeS19_cleaned, "Baseline")
TimeS20_cleaned <- baseline_diff(TimeS20_cleaned, "Baseline")
TimeS21_cleaned <- baseline_diff(TimeS21_cleaned, "Baseline")

```

Plots of difference from baseline
```{r}
TimeDiffPlot <- function(input, condA, condB, sem){
  a <- subset(input, condition == condA)
  b <- subset(input, condition == condB)
  final <- data.frame(Type = c(paste("Baseline -", condA, sep = " ") , paste("Baseline -", condB, sep = " ")), diff_from_baseline = c(mean(a$diff_from_baseline), mean(b$diff_from_baseline)))
  
  return(ggplot(data=final, aes(x=Type, y=diff_from_baseline)) +
  geom_col(width = 0.55)+ labs(y="Time difference from Baseline (min)", x = "Group",
              title = paste("Avg. Difference from Baseline Condition", sem, sep = " ")))
  
}

TimeDiffPlot(TimeF18_cleaned, "PO", "POE", "(Fall 2018)")
TimeDiffPlot(TimeF19_cleaned, "POE", "POMC", "(Fall 2019)")
TimeDiffPlot(TimeS19_cleaned, "POE", "Text", "(Spring 2019)")
TimeDiffPlot(TimeS20_cleaned, "POE", "Text", "(Spring 2020)")
TimeDiffPlot(TimeS21_cleaned, "POE", "Text", "(Spring 2021)")
```

## Time: Paired T-tests & LMER
```{r}
ttest <- function(input, condA, condB){
  first <- subset(input, condition == condA)
  second <- subset(input, condition == condB)
  return(t.test(first$diff_from_baseline, second$diff_from_baseline, paired = TRUE, alternative = "two.sided"))
}

ttest(TimeF18_cleaned, "PO", "POE")
ttest(TimeF19_cleaned, "POE", "POMC")
ttest(TimeS19_cleaned, "POE", "Text")
ttest(TimeS20_cleaned, "POE", "Text")
ttest(TimeS21_cleaned, "POE", "Text")

lmerModel <- function(input, refcond){
  return(lmer(totalTime ~ condition + (1|AnonID), data = input))
}

TimeF18lmer <- lmer(totalTime ~ relevel(condition, ref = "Text") + (1 |AnonID), data= TimeF18_cleaned)
summary(TimeF18lmer)

TimeF19lmer <- lmer(totalTime ~ relevel(condition, ref = "Text") + (1 |AnonID), data= TimeF19_cleaned)
summary(TimeF19lmer)

TimeS19lmer <- lmer(totalTime ~ relevel(condition, ref = "Baseline") + (1 |AnonID), data= TimeS19_cleaned)
summary(TimeS19lmer)

TimeS20lmer <- lmer(totalTime ~ relevel(condition, ref = "Baseline") + (1 |AnonID), data= TimeS20_cleaned)
summary(TimeS20lmer)

TimeS21lmer <- lmer(totalTime ~ relevel(condition, ref = "Baseline") + (1 |AnonID), data= TimeS21_cleaned)
summary(TimeS21lmer)

```

## Time: Extended Tests
```{r}
ttestExtended <- function(input, condA, condB){
  first <- subset(input, condition == condA)
  second <- subset(input, condition == condB)
  return(t.test(first$totalTime, second$totalTime, paried = TRUE, alternative = "two.sided"))
}

ttestExtended(TimeF19_cleaned, "POE", "Text")
ttestExtended(TimeS19_cleaned, "POE", "Text")
ttestExtended(TimeS20_cleaned, "POE", "Text")

```


## Performance: Preparing Data

To prepare the data so that it becomes an acceptable df for the violin plots, we will transform the data from wide into long form. Also note that we're setting factor_key to TRUE in our gather function so that we can re-level our reference conditions in the later LMER tests.
```{r}
gather_to_long <- function(df, cond1, cond2, cond3){
  keycol <- "condition"
  valuecol <- "zscore"
  gathercols <- c(cond1, cond2, cond3)
  return (gather_(df, keycol, valuecol, gathercols, factor_key = TRUE))
}

PerfF18long <- gather_to_long(PerfF18_cleaned, "PO", "POE", "Text")
PerfF19long <- gather_to_long(PerfF19_cleaned, "POE", "PO...MC", "Text.Only")
PerfS19long <- gather_to_long(PerfS19_cleaned, "Baseline", "Control", "Experimental")
PerfS20long <- gather_to_long(PerfS20_cleaned, "baseline", "control", "experimental")
PerfS21long <- gather_to_long(PerfS21_cleaned, "Baseline", "Text", "POE")
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

**Z-score difference From Mean**
To find out how many z-scores away from the mean each condition results for the students, we will take the average of the z-scores in each condition for each semester

```{r}

ResultsF18_cleaned <- data.frame(Type = c("PO", "POE", "Text"), Avg = c(mean(PerfF18_cleaned$PO), mean(PerfF18_cleaned$POE), mean(PerfF18_cleaned$Text)))

ResultsF19_cleaned <- data.frame(Type = c("POMC", "POE", "Text"), Avg = c(mean(PerfF19_cleaned$PO...MC), mean(PerfF19_cleaned$POE), mean(PerfF19_cleaned$Text)))

ResultsS19_cleaned <- data.frame(Type = c("Baseline", "Control", "Experimental"), Avg = c(mean(PerfS19_cleaned$Baseline), mean(PerfS19_cleaned$Control), mean(PerfS19_cleaned$Experimental)))

ResultsS20_cleaned <- data.frame(Type = c("Baseline", "Control", "Experimental"), Avg = c(mean(PerfS20_cleaned$baseline), mean(PerfS20_cleaned$control), mean(PerfS20_cleaned$experimental)))

ResultsS21_cleaned <- data.frame(Type = c("Baseline", "Text", "POE"), Avg = c(mean(PerfS21_cleaned$Baseline), mean(PerfS21_cleaned$Text), mean(PerfS21_cleaned$POE)))


F18plot <- ggplot(data=ResultsF18_cleaned, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Fall 2018 cleaned)")

F19plot <- ggplot(data=ResultsF19_cleaned, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Fall 2019 cleaned)")

S19plot <- ggplot(data=ResultsS19_cleaned, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Spring 2019 cleaned)")

S20plot <- ggplot(data=ResultsS20_cleaned, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Spring 2020 cleaned)")

S21plot <- ggplot(data=ResultsS21_cleaned, aes(x=Type, y=Avg)) +
  geom_col(width = 0.65)+ labs(y="z-score deviation from Mean", x = "Group",
              title = "Avg. Z-score Difference from Mean (Spring 2021 cleaned)")




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


PerfF18_cleaned <- make_differences (PerfF18_cleaned, 4)
PerfF19_cleaned <- make_differences (PerfF19_cleaned, 4)
PerfS19_cleaned <- make_differences (PerfS19_cleaned, 2)
PerfS20_cleaned <- make_differences (PerfS20_cleaned, 2)
PerfS21_cleaned <- make_differences (PerfS21_cleaned, 2)
```

**Plots of the differences**

```{r}

diffResultsF18 <- data.frame(Type = c("Baseline - PO", "Baseline - POE"), Avg = c(mean(PerfF18_cleaned$diffFromBase1), mean(PerfF18_cleaned$diffFromBase2)))

diffResultsF19 <- data.frame(Type = c("Baseline - POMC", "Baseline - POE"), Avg = c(mean(PerfF19_cleaned$diffFromBase1), mean(PerfF19_cleaned$diffFromBase2)))

diffResultsS19 <- data.frame(Type = c("Baseline - Control", "Baseline - Experimental"), Avg = c(mean(PerfS19_cleaned$diffFromBase1), mean(PerfS19_cleaned$diffFromBase2)))

diffResultsS20_ <- data.frame(Type = c("Baseline - Control", "Baseline - Experimental"), Avg = c(mean(PerfS20_cleaned$diffFromBase1), mean(PerfS20_cleaned$diffFromBase2)))

diffResultsS21_ <- data.frame(Type = c("Baseline - Control", "Baseline - Experimental"), Avg = c(mean(PerfS21_cleaned$diffFromBase1), mean(PerfS21_cleaned$diffFromBase2)))


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

## Performance: Paired t-tests & Condition LMER

Below are the summaries of the paired t-tests. None of the performance differences are significant at the alpha = 0.05 level.
```{r}
t.test(PerfF18_cleaned$diffFromBase1, PerfF18_cleaned$diffFromBase2, paired = TRUE, alternative = "two.sided")
t.test(PerfF19_cleaned$diffFromBase1, PerfF19_cleaned$diffFromBase2, paired = TRUE, alternative = "two.sided")
t.test(PerfS19_cleaned$diffFromBase1, PerfS19_cleaned$diffFromBase2, paired = TRUE, alternative = "two.sided")
t.test(PerfS20_cleaned$diffFromBase1, PerfS20_cleaned$diffFromBase2, paired = TRUE, alternative = "two.sided")
t.test(PerfS21_cleaned$diffFromBase1, PerfS21_cleaned$diffFromBase2, paired = TRUE, alternative = "two.sided")
```
Now for further analysis, we will run a linear mixed-effect model on the differences between the differences from the baseline conditions. To do this, we will need to apply the new changes to our long data format and remove the baseline conditions so our comparison can be made on the differences from the baseline.
```{r}

PerfF18long <- gather_to_long(PerfF18_cleaned, "PO", "POE", "Text")
PerfF19long <- gather_to_long(PerfF19_cleaned, "POE", "PO...MC", "Text.Only")
PerfS19long <- gather_to_long(PerfS19_cleaned, "Baseline", "Control", "Experimental")
PerfS20long <- gather_to_long(PerfS20_cleaned, "baseline", "control", "experimental")
PerfS21long <- gather_to_long(PerfS21_cleaned, "Baseline", "Text", "POE")

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
## Performance: Z-Score differences from baseline LMER
We will now look at another LMER analysis, where we test for the differences in zscore differences from the baseline conditions. To do this, we will first extract the values (diffFromBase1 and diffFromBase2) from our cleaned performance data sets, then populate the new dataframe with the differences from baseline, conditions, and the Student IDs. 
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

ZDiff_F18 <- new(PerfF18_cleaned, "PO", "POE")
ZDiff_F19 <- new(PerfF19_cleaned, "POMC", "POE")
ZDiff_S19 <- new(PerfS19_cleaned, "Control", "Experimental")
ZDiff_S20 <- new(PerfS20_cleaned, "Control", "Experimental")
ZDiff_S21 <- new(PerfS21_cleaned, "Text", "POE")

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
t.test(PerfF19_cleaned$POE, PerfF19_cleaned$Text.Only, paired = TRUE, alternative = "two.sided")
t.test(PerfS20_cleaned$control, PerfS20_cleaned$experimental, paired = TRUE, alternative = "two.sided")
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

PerfF18long <- gather_to_long(PerfF18_cleaned, "PO", "POE", "Text")
PerfF19long <- gather_to_long(PerfF19_cleaned, "POE", "PO...MC", "Text.Only")
PerfS19long <- gather_to_long(PerfS19_cleaned, "Baseline", "Control", "Experimental")
PerfS20long <- gather_to_long(PerfS20_cleaned, "baseline", "control", "experimental")
PerfS21long <- gather_to_long(PerfS21_cleaned, "Baseline", "Text", "POE")

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
              title = "Z-score gain per minute (Fall 2018 cleaned)")

perfPlotF19 <- ggplot(data=perfResultsF19, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Fall 2019 cleaned)")

perfPlotS19 <- ggplot(data=perfResultsS19, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Spring 2019 cleaned)")

perfPlotS20 <- ggplot(data=perfResultsS20, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Spring 2020 cleaned)")


perfPlotS21 <- ggplot(data=perfResultsS21, aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per minute (Spring 2021 cleaned)")

perfPlotF18
perfPlotF19
perfPlotS19
perfPlotS20
perfPlotS21
```

## Gain per minute: T-tests & LMER
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


## Gain per Sentence Analysis:
Reading files and creating dataframes that can take in sentence lengths.
```{r}
nsentences <- function(df){
  return(str_count(df, "\\. ") + str_count(df, "\\? ") + str_count(df, "\\! "))
}

gather_to_long <- function(df, cond1, cond2, cond3){
  keycol <- "condition"
  valuecol <- "zscore"
  gathercols <- c(cond1, cond2, cond3)
  return (gather_(df, keycol, valuecol, gathercols, factor_key = TRUE))
}

PerfF18long <- gather_to_long(PerfF18_cleaned, "PO", "POE", "Text")[c(1,14,15)]
```

Assigning sentence lengths to each student. 
```{r}
add_elements <- function(df, Doc1, Doc2, i){
    df$ReadLen[i] <- nsentences(Doc1) + nsentences(Doc2)
    df$sent_gain_per_len[i] <- df$zscore[i]/df$ReadLen[i] 
    df$totalWords[i] <- lengths(gregexpr("\\W+", Doc1)) + 1 + lengths(gregexpr("\\W+", Doc2)) + 1
    df$word_gain_per_len[i] <- df$zscore[i]/df$totalWords[i]
    return(df)
}


assign_lengths <- function(df, AB1, AB2, A3, B3, A4, B4, A5, B5, A6, B6, baseline, control , experimental){
  for (i in 1:nrow(df)){
    if (df$condition[i] == baseline){
      df<- add_elements(df, AB1, AB2, i)
    }
    else if(df$condition[i] == control & df$Group[i] == "A"){
      df<- add_elements(df, A3, A4, i)
    }
    else if (df$condition[i] == control & df$Group[i] == "B"){
      df<- add_elements(df, B5, B6, i)
    }
    else if(df$condition[i] == experimental & df$Group[i] == "A"){
      df<- add_elements(df, A5, A6, i)
    }
    else if (df$condition[i] == experimental & df$Group[i] == "B"){
      df<- add_elements(df, B3, B4, i)
    }
  }
  df$sent_diff_from_baseline <- 0
  df$word_diff_from_baseline <- 0
  for (j in 1:nrow(df)){
    if(df$condition[j] == control | df$condition[j] == experimental){
      baselineGain <- df$sent_gain_per_len[which(df$Student == df$Student[j] & df$condition == baseline)]
      df$sent_diff_from_baseline[j] = baselineGain - df$sent_gain_per_len[j]
      
      baselineGain <- df$word_gain_per_len[which(df$Student == df$Student[j] & df$condition == baseline)]
      df$word_diff_from_baseline[j] = baselineGain - df$word_gain_per_len[j]
      
    }
  }
  return(df)
}

assign_lengths_many <- function(df, EF1, EF2, AD3, AD4, BC5, BC6, CD1, CD2, BF3, BF4, AE5, AE6, AB1, AB2, CE3, CE4, DF5, DF6, PO_POMC, baseline, experimental){
  for(i in 1:nrow(df)){
    if(df$condition[i] == PO_POMC){
      if(df$Group[i] == "E" | df$Group[i] == "F"){
        df<- add_elements(df, EF1, EF2, i)
      }
      else if (df$Group[i] == "A" | df$Group[i] == "D"){
        df<- add_elements(df, AD3, AD4, i)
      }
      else{
        df<- add_elements(df, BC5, BC6, i)
      }
    }
    else if(df$condition[i] == baseline){
      if(df$Group[i] == "C" | df$Group[i] == "D"){
        df<- add_elements(df, CD1, CD2, i)
      }
      else if (df$Group[i] == "B" | df$Group[i] == "F"){
        df<- add_elements(df, BF3, BF4, i)
      }
      else{
        df<- add_elements(df, AE5, AE6, i)
      }
    }
    else{
      if(df$Group[i] == "A" | df$Group[i] == "B"){
        df<- add_elements(df, AB1, AB2, i)
      }
      else if (df$Group[i] == "C" | df$Group[i] == "E"){
        df<- add_elements(df, CE3, CE4, i)
      }
      else{
        df<- add_elements(df, DF5, DF6, i)
      }
    }
  }
  return(df)
}
```


Fall 2018
```{r}
W_F18POMC1EF <- read_file("W_F19POMC1EF.txt")
W_F18POMC2EF <- read_file("W_F19POMC2EF.txt")
W_F18POMC3AD <- read_file("W_F19POMC3AD.txt")
W_F18POMC4AD <- read_file("W_F19POMC4AD.txt")
W_F18POMC5BC <- read_file("W_F19POMC5BC.txt")
W_F18POMC6BC <- read_file("W_F19POMC6BC.txt")

W_F18Text1CD <- read_file("W_F19Text1CD.txt")
W_F18Text2CD <- read_file("W_F19Text2CD.txt")
W_F18Text3BF <- read_file("W_F19Text3BF.txt")
W_F18Text4BF <- read_file("W_F19Text4BF.txt")
W_F18Text5AE <- read_file("W_F19Text5AE.txt")
W_F18Text6AE <- read_file("W_F19Text6AE.txt")

W_F18Exp1AB <- read_file("W_F19Exp1AB.txt")
W_F18Exp2AB <- read_file("W_F19Exp2AB.txt")
W_F18Exp3CE <- read_file("W_F19Exp3CE.txt")
W_F18Exp4CE <- read_file("W_F19Exp4CE.txt")
W_F18Exp5DF <- read_file("W_F19Exp5DF.txt")
W_F18Exp6DF <- read_file("W_F19Exp6DF.txt")


PerfF18long <- gather_to_long(PerfF18_cleaned, "PO", "POE", "Text")[c(1,14,15)]

F18Groups <- read.csv("18F_Groups.csv")
names(F18Groups)[1] <- "Student"
names(F18Groups)[2] <- "Group"

for (i in 1:nrow(F18Groups)){
  if(F18Groups$Group[i] == "31549"){
    F18Groups$Group[i] <- "A"
  }
  else if(F18Groups$Group[i] == "37526"){
    F18Groups$Group[i] <- "B"
  }
  else if(F18Groups$Group[i] == "32001"){
    F18Groups$Group[i] <- "C"
  }
  else if(F18Groups$Group[i] == "24807"){
    F18Groups$Group[i] <- "D"
  }
  else if(F18Groups$Group[i] == "87862"){
    F18Groups$Group[i] <- "E"
  }
  else{
    F18Groups$Group[i] <- "F"
  }
}

PerfF18long <- merge(PerfF18long, F18Groups)

PerfF18long <- assign_lengths_many(PerfF18long, W_F18POMC1EF, W_F18POMC2EF, W_F18POMC3AD, W_F18POMC4AD, W_F18POMC5BC, W_F18POMC6BC, W_F18Text1CD, W_F18Text2CD, W_F18Text3BF, W_F18Text4BF, W_F18Text5AE, W_F18Text6AE, W_F18Exp1AB, W_F18Exp2AB, W_F18Exp3CE, W_F18Exp4CE, W_F18Exp5DF, W_F18Exp6DF, "PO", "Text", "POE")

ggplot(data= data.frame(Type = c("Text","PO", "POE"), Avg = c(mean(subset(PerfF18long, condition == "Text")$sent_gain_per_len), mean(subset(PerfF18long, condition == "PO")$sent_gain_per_len), mean(subset(PerfF18long, condition == "POE")$sent_gain_per_len))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per sentence", x = "Group",
              title = "Z-score gain per sentence, Fall 2018")

ggplot(data= data.frame(Type = c("Text","PO", "POE"), Avg = c(mean(subset(PerfF18long, condition == "Text")$word_gain_per_len), mean(subset(PerfF18long, condition == "PO")$word_gain_per_len), mean(subset(PerfF18long, condition == "POE")$word_gain_per_len))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per sentence", x = "Group",
              title = "Z-score gain per word, Fall 2018")

summary(aov(sent_gain_per_len ~ condition, data = PerfF18long))
sent_gain_plot <- TukeyHSD(aov(sent_gain_per_len ~ condition, data = PerfF18long))
plot(sent_gain_plot)

summary(aov(word_gain_per_len ~ condition, data = PerfF18long))
word_gain_plot <- TukeyHSD(aov(word_gain_per_len ~ condition, data = PerfF18long))
plot(word_gain_plot)

```


Fall 2019
```{r}
W_F19POMC1EF <- read_file("W_F19POMC1EF.txt")
W_F19POMC2EF <- read_file("W_F19POMC2EF.txt")
W_F19POMC3AD <- read_file("W_F19POMC3AD.txt")
W_F19POMC4AD <- read_file("W_F19POMC4AD.txt")
W_F19POMC5BC <- read_file("W_F19POMC5BC.txt")
W_F19POMC6BC <- read_file("W_F19POMC6BC.txt")

W_F19Text1CD <- read_file("W_F19Text1CD.txt")
W_F19Text2CD <- read_file("W_F19Text2CD.txt")
W_F19Text3BF <- read_file("W_F19Text3BF.txt")
W_F19Text4BF <- read_file("W_F19Text4BF.txt")
W_F19Text5AE <- read_file("W_F19Text5AE.txt")
W_F19Text6AE <- read_file("W_F19Text6AE.txt")

W_F19Exp1AB <- read_file("W_F19Exp1AB.txt")
W_F19Exp2AB <- read_file("W_F19Exp2AB.txt")
W_F19Exp3CE <- read_file("W_F19Exp3CE.txt")
W_F19Exp4CE <- read_file("W_F19Exp4CE.txt")
W_F19Exp5DF <- read_file("W_F19Exp5DF.txt")
W_F19Exp6DF <- read_file("W_F19Exp6DF.txt")


names(PerfF19_cleaned)[2] <- "POMC"
names(PerfF19_cleaned)[4] <- "Text"
PerfF19long <- gather_to_long(PerfF19_cleaned, "POMC", "POE", "Text")[c(1,9,10)]

F19Groups <- read.csv("19F_Groups.csv")
names(F19Groups)[1] <- "Student"
names(F19Groups)[2] <- "Group"
for (i in 1:nrow(F19Groups)){
  F19Groups$Group[i] <- toupper(F19Groups$Group[i])
}

PerfF19long <- merge(PerfF19long, F19Groups)

PerfF19long <- assign_lengths_many(PerfF19long, W_F19POMC1EF, W_F19POMC2EF, W_F19POMC3AD, W_F19POMC4AD, W_F19POMC5BC, W_F19POMC6BC, W_F19Text1CD, W_F19Text2CD, W_F19Text3BF, W_F19Text4BF, W_F19Text5AE, W_F19Text6AE, W_F19Exp1AB, W_F19Exp2AB, W_F19Exp3CE, W_F19Exp4CE, W_F19Exp5DF, W_F19Exp6DF, "POMC", "Text", "POE")

ggplot(data= data.frame(Type = c("Text","POMC", "POE"), Avg = c(mean(subset(PerfF19long, condition == "Text")$sent_gain_per_len), mean(subset(PerfF19long, condition == "POMC")$sent_gain_per_len), mean(subset(PerfF19long, condition == "POE")$sent_gain_per_len))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per sentence", x = "Group",
              title = "Z-score gain per sentence, Fall 2019")

ggplot(data= data.frame(Type = c("Text","POMC", "POE"), Avg = c(mean(subset(PerfF19long, condition == "Text")$word_gain_per_len), mean(subset(PerfF19long, condition == "POMC")$word_gain_per_len), mean(subset(PerfF19long, condition == "POE")$word_gain_per_len))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per sentence", x = "Group",
              title = "Z-score gain per word, Fall 2019")

summary(aov(sent_gain_per_len ~ condition, data = PerfF19long))
sent_gain_plot <- TukeyHSD(aov(sent_gain_per_len ~ condition, data = PerfF19long))
plot(sent_gain_plot)

summary(aov(word_gain_per_len ~ condition, data = PerfF19long))
word_gain_plot <- TukeyHSD(aov(word_gain_per_len ~ condition, data = PerfF19long))
plot(word_gain_plot)

```


Spring 2019
Note: S2019 and S2020 used the same documents for Baseline, POE, and Text. So we will load in the same text files for these two semesters. 
```{r}
W_S19Base1AB <-read_file("W_S20Baseline1AB.txt")
W_S19Base2AB <-read_file("W_S20Baseline2AB.txt")

W_S19Control3A <- read_file("W_S20Control3A.txt")
W_S19Control4A <- read_file("W_S20Control4A.txt")
W_S19Control5B <- read_file("W_S20Control5B.txt")
W_S19Control6B <- read_file("W_S20Control6B.txt")

W_S19Exp3B <- read_file("W_S20Exp3B.txt")
W_S19Exp4B <- read_file("W_S20Exp4B.txt")
W_S19Exp5A <- read_file("W_S20Exp5A.txt")
W_S19Exp6A <- read_file("W_S20Exp6A.txt")

S19Groups <- read.csv("19S_Groups.csv")
names(S19Groups)[1] = "Student"

PerfS19long <- merge(gather_to_long(PerfS19_cleaned, "Baseline", "Control", "Experimental")[c(1,8,9)], S19Groups)

for (i in 1:nrow(PerfS19long)){
  if(PerfS19long$Group[i] == "52618"){
    PerfS19long$Group[i] = "A"
  }
  else{
    PerfS19long$Group[i] = "B"
  }
}

PerfS19long <- assign_lengths(PerfS19long, W_S19Base1AB, W_S19Base2AB, W_S19Control3A, W_S19Exp3B, W_S19Control4A, W_S19Exp4B, W_S19Exp5A, W_S19Control5B, W_S19Exp6A, W_S19Control6B, "Baseline", "Control", "Experimental")



ggplot(data= data.frame(Type = c("Baseline - Control","Baseline - Experimental"), Avg = c(mean(subset(PerfS19long, condition == "Control")$sent_diff_from_baseline), mean(subset(PerfS19long, condition == "Experimental")$sent_diff_from_baseline))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per sentence (Baseline - Condition), Spring 2019")


ggplot(data= data.frame(Type = c("Baseline - Control","Baseline - Experimental"), Avg = c(mean(subset(PerfS19long, condition == "Control")$word_diff_from_baseline), mean(subset(PerfS19long, condition == "Experimental")$word_diff_from_baseline))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per word (Baseline - Condition), Spring 2019")

summary(lm(word_diff_from_baseline ~ condition, data = subset(PerfS19long, condition!="Baseline")))

summary(lm(sent_diff_from_baseline ~ condition, data = subset(PerfS19long, condition!="Baseline")))

t.test(subset(PerfS19long, condition=="Control")$word_diff_from_baseline, subset(PerfS19long, condition=="Experimental")$word_diff_from_baseline, paired = TRUE, var.equal=TRUE)

t.test(subset(PerfS19long, condition=="Control")$sent_diff_from_baseline, subset(PerfS19long, condition=="Experimental")$sent_diff_from_baseline, paired = TRUE, var.equal=TRUE)
```

Spring 2020
```{r}
W_S20Base1AB <-read_file("W_S20Baseline1AB.txt")
W_S20Base2AB <-read_file("W_S20Baseline2AB.txt")

W_S20Control3A <- read_file("W_S20Control3A.txt")
W_S20Control4A <- read_file("W_S20Control4A.txt")
W_S20Control5B <- read_file("W_S20Control5B.txt")
W_S20Control6B <- read_file("W_S20Control6B.txt")

W_S20Exp3B <- read_file("W_S20Exp3B.txt")
W_S20Exp4B <- read_file("W_S20Exp4B.txt")
W_S20Exp5A <- read_file("W_S20Exp5A.txt")
W_S20Exp6A <- read_file("W_S20Exp6A.txt")

S20Groups <- read.csv("20S_Groups.csv")[c(1,4)]
names(S20Groups)[1] = "Student"

PerfS20long <- merge(gather_to_long(PerfS20, "baseline", "control", "experimental"), S20Groups)[c(1,6,7,8)]

PerfS20long <- assign_lengths(PerfS20long, W_S20Base1AB, W_S20Base2AB, W_S20Control3A, W_S20Exp3B, W_S20Control4A, W_S20Exp4B, W_S20Exp5A, W_S20Control5B, W_S20Exp6A, W_S20Control6B, "baseline", "control", "experimental")

ggplot(data= data.frame(Type = c("Baseline - Control","Baseline - Experimental"), Avg = c(mean(subset(PerfS20long, condition == "control")$sent_diff_from_baseline), mean(subset(PerfS20long, condition == "experimental")$sent_diff_from_baseline))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per sentence (Baseline - Condition), Spring 2020")


ggplot(data= data.frame(Type = c("Baseline - Control","Baseline - Experimental"), Avg = c(mean(subset(PerfS20long, condition == "control")$word_diff_from_baseline), mean(subset(PerfS20long, condition == "experimental")$word_diff_from_baseline))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per word (Baseline - Condition), Spring 2020")

summary(lm(word_diff_from_baseline ~ condition, data = subset(PerfS20long, condition!="baseline")))

summary(lm(sent_diff_from_baseline ~ condition, data = subset(PerfS20long, condition!="baseline")))

t.test(subset(PerfS20long, condition=="control")$word_diff_from_baseline, subset(PerfS20long, condition=="experimental")$word_diff_from_baseline, paired = TRUE, var.equal=TRUE)

t.test(subset(PerfS20long, condition=="control")$sent_diff_from_baseline, subset(PerfS20long, condition=="experimental")$sent_diff_from_baseline, paired = TRUE, var.equal=TRUE)
```

Spring 2021
```{r}
W_S21Base1AB <-read_file("W_S21Baseline1AB.txt")
W_S21Base2AB <-read_file("W_S21Baseline2AB.txt")

W_S21Control3A <- read_file("W_S21Control3A.txt")
W_S21Control4A <- read_file("W_S21Control4A.txt")
W_S21Control5B <- read_file("W_S21Control5B.txt")
W_S21Control6B <- read_file("W_S21Control6B.txt")

W_S21Exp3B <- read_file("W_S21Exp3B.txt")
W_S21Exp4B <- read_file("W_S21Exp4B.txt")
W_S21Exp5A <- read_file("W_S21Exp5A.txt")
W_S21Exp6A <- read_file("W_S21Exp6A.txt")

S21Groups <- read.csv("spring2021_groupAssignment.csv")
names(S21Groups)[1] = "Student"

PerfS21long <- merge(gather_to_long(PerfS21_cleaned, "Baseline", "Text", "POE")[c(1,7,8)], S21Groups)

for (i in 1:nrow(PerfS21long)){
  if(PerfS21long$Group[i] == "Group A"){
    PerfS21long$Group[i] = "A"
  }
  else{
    PerfS21long$Group[i] = "B"
  }
}

PerfS21long <- assign_lengths(PerfS21long, W_S21Base1AB, W_S21Base2AB, W_S21Control3A, W_S21Exp3B, W_S21Control4A, W_S21Exp4B, W_S21Exp5A, W_S21Control5B, W_S21Exp6A, W_S21Control6B, "Baseline", "Text", "POE")


ggplot(data= data.frame(Type = c("Baseline - Control","Baseline - Experimental"), Avg = c(mean(subset(PerfS21long, condition == "Text")$sent_diff_from_baseline), mean(subset(PerfS21long, condition == "POE")$sent_diff_from_baseline))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per sentence (Baseline - Condition), Spring 2021")

ggplot(data= data.frame(Type = c("Baseline - Control","Baseline - Experimental"), Avg = c(mean(subset(PerfS21long, condition == "Text")$word_diff_from_baseline), mean(subset(PerfS21long, condition == "POE")$word_diff_from_baseline))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per word (Baseline - Condition), Spring 2021")

ggplot(data= data.frame(Type = c("Baseline", "Control","Experimental"), Avg = c(mean(subset(PerfS21long, condition == "Baseline")$word_gain_per_len), mean(subset(PerfS21long, condition == "Text")$word_gain_per_len), mean(subset(PerfS21long, condition == "POE")$word_gain_per_len))), aes(x=Type, y=Avg)) +
  geom_col(width = 0.6)+ labs(y="gain per minute", x = "Group",
              title = "Z-score gain per word, Spring 2021")

mean(subset(PerfS21long, condition == "Text")$word_gain_per_len)
mean(subset(PerfS21long, condition == "POE")$word_gain_per_len)

mean(subset(PerfS21long, condition == "Text")$word_diff_from_baseline)
mean(subset(PerfS21long, condition == "POE")$word_diff_from_baseline)



summary(lm(word_diff_from_baseline ~ condition, data = subset(PerfS21long, condition!="Baseline")))

summary(lm(sent_diff_from_baseline ~ condition, data = subset(PerfS21long, condition!="Baseline")))

t.test(subset(PerfS21long, condition=="Text")$word_diff_from_baseline, subset(PerfS21long, condition=="POE")$word_diff_from_baseline, paired = TRUE, var.equal=TRUE)

t.test(subset(PerfS21long, condition=="Text")$sent_diff_from_baseline, subset(PerfS21long, condition=="POE")$sent_diff_from_baseline, paired = TRUE, var.equal=TRUE)
```
