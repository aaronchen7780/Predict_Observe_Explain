---
title: "POE contamination check"
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
```{r}
df <- read.csv("SocialSp21.csv")
GroupS21 <- read.csv("spring2021_groupAssignment.csv")
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
df_joined <- left_join(df_clean, GroupS21)

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

# Label Groups
```{r}
GroupF18 <- read.csv("18F_Groups.csv")
GroupF19 <- read.csv("19F_Groups.csv")
GroupS19 <- read.csv("19S_Groups.csv")
GroupS20 <- read.csv("20S_Groups.csv")
GroupS21 <- read.csv("spring2021_groupAssignment.csv")

group_label1 <- function(df, condA, condB, condC, groupA, groupB, groupC, groupD, groupE, groupF){
  df$include <- 0 
  df$Group <- "A"
    for (i in 1:nrow(df)){
      if (df$Condition[i] == groupA | df$Condition[i] == groupB) {
        df$include[i] <- 1 
        df$Group[i] = condA
      }
      else if (df$Condition[i] == groupC | df$Condition[i] == groupD){
        df$include[i] <- 1 
        df$Group[i] <- condB
      }
      else if (df$Condition[i] == groupE | df$Condition[i] == groupF){
        df$include[i] <- 1 
        df$Group[i] <- condC
      }
    }
  return (subset(df, include == 1))
}


group_label2 <- function(df, condA, condB, groupA, groupB){
  df$include <- 0 
  df$Condition <- "A"
    for(i in 1:nrow(df)){
      if(df$Group[i] == groupA) {
        df$include[i] <- 1 
        df$Condition[i] = condA
      }
      else if (df$Group[i] == groupB) {
        df$include[i] <- 1 
        df$Condition[i] = condB
      }
    }
  return(subset(df, include == 1))
}





GroupF18 <- group_label1(GroupF18, "Baseline", "POE", "PO", "32001", "24807", "31549", "37526", "87862", "28103")

names(GroupF18)[2] <- "Group"
names(GroupF18)[4] <- "Condition"

names(GroupF19)[2] <- "Condition"
GroupF19 <- group_label1(GroupF19,"POE", "Text", "POMC", "a", "b", "c","d", "e","f")


names(GroupF19)[1] <- "StudentID"
names(GroupF19)[2] <- "Group"
names(GroupF19)[4] <- "Condition"


GroupS19 <- group_label2(GroupS19, "Control", "Experimental", "52618", "93597")
GroupS20 <- group_label2(GroupS20, "Control", "Experimental", "A", "B")
GroupS21 <- group_label2(GroupS21, "Control", "Experimental", "Group A", "Group B")
```


```{r}
## Making sure both time and performance data have the same observations
dropMissingPerf <- function(timedata, perfdata){
  perfdata$keep <- 0 
  names(perfdata)[1] = "AnonID"
  for(i in 1:nrow(perfdata) ){
    if(any(timedata$AnonID == perfdata$AnonID[i])){
      perfdata$keep[i] = 1
    }
  }
  return(subset(perfdata, keep == 1))
}

dropMissingTime <- function(timedata, perfdata){
  timedata$keep <- 0
  names(perfdata)[1] = "AnonID"
    for(i in 1:nrow(timedata) ){
    if(any(perfdata$AnonID == timedata$AnonID[i])){
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

## Labeling Conditions in Perf & Time datasets

Ends with subsetting only the relevant rows
```{r}
names(GroupS20)[1] <- "StudentID"
names(GroupF19)[1] <- "StudentID"
names(GroupS19)[1] <- "StudentID"
names(GroupS21)[1] <- "StudentID"
names(GroupF18)[2] <- "Group"

label <- function(df, groupings){
  df$U1Condition <- "A"
  names(df)[1] <- "AnonID"
  for(i in 1:nrow(df)){
    row_num <- which(groupings$StudentID == df$AnonID[i])
    df$U1Condition[i] = groupings$Condition[row_num]
  }
  return(df)
}

TimeF18 <- label(TimeF18, GroupF18)
TimeF19 <- label(TimeF19, GroupF19)
TimeS19 <- label(TimeS19, GroupS19)
TimeS20 <- label(TimeS20, GroupS20)
TimeS21 <- label(TimeS21, GroupS21)

PerfF18 <- label(PerfF18, GroupF18)
PerfF19 <- label(PerfF19, GroupF19)
PerfS19 <- label(PerfS19, GroupS19)
PerfS20 <- label(PerfS20, GroupS20)
PerfS21 <- label(PerfS21, GroupS21)


##Choosing only the relevant rows to work with
cleanup_time <- function(df, condA, CondAEq, condB, CondBEq){
  df$keep2 <- 0
  for (i in 1:nrow(df)){
    if(df$condition[i] == df$U1Condition[i] | (df$condition[i] == condA & df$U1Condition[i] == CondAEq) |
       (df$condition[i] == condB & df$U1Condition[i] == CondBEq)){
      df$keep2[i] = 1
    }
  }
  return(subset(df, keep2 == 1))
}


## Not all time datasets require the use of all the arguments
TimeF18 <- cleanup_time(TimeF18, "Text",  "Baseline", "POE", "Experimental")
TimeF19 <- cleanup_time(TimeF19, "Text",  "Text", "POE", "POE")

TimeS19 <- cleanup_time(TimeS19, "POE", "Experimental", "Text", "Control")
TimeS20 <- cleanup_time(TimeS20, "POE", "Experimental", "Text", "Control")
TimeS21 <- cleanup_time(TimeS21, "POE", "Experimental", "Text", "Control")

## cleaning up time implicitly cleans up perf
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

## Marking Time outliers 
```{r}
exclude_outliers <- function(df, condA, condB, condC, conditionNum){
  df$timeOutlier <- 0
  A <- subset(df, U1Condition == condA)
  IQR_A <- IQR(A$totalTime)
  upper_A <- quantile(A$totalTime, .75) + 1.5*IQR_A
  lower_A <- quantile(A$totalTime, .25) - 1.5*IQR_A
  
  for (i in 1:nrow(A)){
    if (!(A$totalTime[i] > lower_A & A$totalTime[i] < upper_A)){
      index = which(df$AnonID == A$AnonID[i])
      df$timeOutlier[index] = 1
      }
  }
  
  B <- subset(df, U1Condition == condB)
  IQR_B <- IQR(B$totalTime)
  upper_B <- quantile(B$totalTime, .75) + 1.5*IQR_B
  lower_B <- quantile(B$totalTime, .25) - 1.5*IQR_B
  
  for (i in 1:nrow(B)){
    if (!(B$totalTime[i] > lower_B & B$totalTime[i] < upper_B)){
      index = which(df$AnonID == B$AnonID[i])
      df$timeOutlier[index] = 1
      }
  }
  if(conditionNum == 3){
    C <- subset(df, U1Condition == condC)
    IQR_C <- IQR(C$totalTime)
    upper_C <- quantile(C$totalTime, .75) + 1.5*IQR_C
    lower_C <- quantile(C$totalTime, .25) - 1.5*IQR_C
    
    for (i in 1:nrow(C)){
      if (!(C$totalTime[i] > lower_C & C$totalTime[i] < upper_C)){
        index = which(df$AnonID == C$AnonID[i])
        df$timeOutlier[index] = 1
        }
    }
  }
  return (df)
}

TimeF18 <- exclude_outliers(TimeF18, "PO", "POE", "Baseline", 3)
TimeF19 <- exclude_outliers(TimeF19 ,"POE", "POMC", "Text", 3)
TimeS19 <- exclude_outliers(TimeS19, "Control", "Experimental", NA, 2)
TimeS20 <- exclude_outliers(TimeS20, "Control", "Experimental", NA, 2)
TimeS21 <- exclude_outliers(TimeS21, "Control", "Experimental", NA, 2)
```

## Marking performance outliers 
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

remove_outliers <- function(input, upper1, lower1, upper2, lower2, upper3, lower3, condA, condB, condC){
  input <- na.omit(input)
  input$perfOutlier <- 0
  for(i in 1:nrow(input)){
    if (!(is.na(upper1)) & input$U1Condition[i] == condA & (input[2][i,] > upper1 | input[2][i,] < lower1)){
      input$perfOutlier[i] = 1
    }
    if (input$U1Condition[i] == condB & (input[3][i,] > upper2 | input[3][i,] < lower2)){
      input$perfOutlier[i] = 1
    }
    if (input$U1Condition[i] == condC & (input[4][i,] > upper3 | input[4][i,] < lower3)){
      input$perfOutlier[i] = 1
    }
  }
  return (input)
}

test <- subset(PerfF18, U1Condition == "PO")


PerfF18 <-remove_outliers(PerfF18, get_upper(subset(PerfF18, U1Condition == "PO")$PO), get_lower(subset(PerfF18, U1Condition == "PO")$PO), get_upper(subset(PerfF18, U1Condition == "POE")$POE), get_lower(subset(PerfF18, U1Condition == "POE")$POE),  get_upper(subset(PerfF18, U1Condition == "Text")$Text), get_lower(subset(PerfF18, U1Condition == "Text")$Text), "PO", "POE", "Text")


names(PerfF19)[2] <- "POMC"
PerfF19 <- remove_outliers(PerfF19, get_upper(subset(PerfF19, U1Condition == "POMC")$POMC), get_lower(subset(PerfF19, U1Condition == "POMC")$POMC), get_upper(subset(PerfF19, U1Condition == "POE")$POE), get_lower(subset(PerfF19, U1Condition == "POE")$POE),  get_upper(subset(PerfF19, U1Condition == "Text")$Text.Only), get_lower(subset(PerfF19, U1Condition == "Text")$Text.Only), "POMC", "POE", "Text")


PerfS19 <- remove_outliers(PerfS19, NA, NA, get_upper(subset(PerfS19, U1Condition == "Control")$Control), get_lower(subset(PerfS19, U1Condition == "Control")$Control),  get_upper(subset(PerfS19, U1Condition == "Experimental")$Experimental), get_lower(subset(PerfS19, U1Condition == "Experimental")$Experimental), NA, "Control", "Experimental")


PerfS20 <- remove_outliers(PerfS20, NA, NA, get_upper(subset(PerfS20, U1Condition == "control")$control), get_lower(subset(PerfS20, U1Condition == "control")$control),  get_upper(subset(PerfS20, U1Condition == "experimental")$experimental), get_lower(subset(PerfS20, U1Condition == "experimental")$experimental), NA, 'control', 'experimental')

PerfS21 <- remove_outliers(PerfS21, NA, NA, get_upper(subset(PerfS21, U1Condition == "Control")$Text), get_lower(subset(PerfS21, U1Condition == "Control")$Text),  get_upper(subset(PerfS21, U1Condition == "Experimental")$POE), get_lower(subset(PerfS21, U1Condition == "Experimental")$POE), NA, "Control", "Experimental")
```

## Extracting outlier-removed datasets for both time and performance 
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
      index = which(timedata$AnonID == perfdata$AnonID[j])
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
      index = which(perfdata$AnonID == timedata$AnonID[j])
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

# Time Plots
```{r}
F18Score <- data.frame(Type = c("Baseline", "PO", "POE"), Avg = c(mean(subset(TimeF18_cleaned, U1Condition == "Baseline")$totalTime), mean(subset(TimeF18_cleaned, U1Condition == "PO")$totalTime), mean(subset(TimeF18_cleaned, U1Condition == "POE")$totalTime)),
                       error = c(sd(subset(TimeF18_cleaned, U1Condition == "Baseline")$totalTime), sd(subset(TimeF18_cleaned, U1Condition == "PO")$totalTime), sd(subset(TimeF18_cleaned, U1Condition == "POE")$totalTime)))

F19Score <- data.frame(Type = c("Text", "POE", "POMC"), Avg = c(mean(subset(TimeF19_cleaned, U1Condition == "Text")$totalTime), mean(subset(TimeF19_cleaned, U1Condition == "POE")$totalTime), mean(subset(TimeF19_cleaned, U1Condition == "POMC")$totalTime)), 
                       error = c(sd(subset(TimeF19_cleaned, U1Condition == "Text")$totalTime), sd(subset(TimeF19_cleaned, U1Condition == "POE")$totalTime), sd(subset(TimeF19_cleaned, U1Condition == "POMC")$totalTime)))

S19Score <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(subset(TimeS19_cleaned, U1Condition == "Control")$totalTime), mean(subset(TimeS19_cleaned, U1Condition == "Experimental")$totalTime)), 
                       sd = c(sd(subset(TimeS19_cleaned, U1Condition == "Control")$totalTime), sd(subset(TimeS19_cleaned, U1Condition == "Experimental")$totalTime)))

S20Score <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(subset(TimeS20_cleaned, U1Condition == "Control")$totalTime), mean(subset(TimeS20_cleaned, U1Condition == "Experimental")$totalTime)), 
                       sd = c(sd(subset(TimeS20_cleaned, U1Condition == "Control")$totalTime), sd(subset(TimeS20_cleaned, U1Condition == "Experimental")$totalTime)))

S21Score <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(subset(TimeS21_cleaned, U1Condition == "Control")$totalTime), mean(subset(TimeS21_cleaned, U1Condition == "Experimental")$totalTime)), 
                       sd = c(sd(subset(TimeS21_cleaned, U1Condition == "Control")$totalTime), sd(subset(TimeS21_cleaned, U1Condition == "Experimental")$totalTime)))


F18Plot <- ggplot(data=F18Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Time: Unit 1 Scores Across Groups (F18)") + geom_errorbar(aes(x=Type, ymin=Avg-error/2, ymax=Avg+error/2), width=0.4, colour="orange", alpha=0.9, size=1.3)

F19Plot <- ggplot(data=F19Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Time: Unit 1 Scores Across Groups (F19)") + geom_errorbar( aes(x=Type, ymin=Avg-error/2, ymax=Avg+error/2), width=0.4, colour="orange", alpha=0.9, size=1.3)

S19Plot <- ggplot(data=S19Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Time: Unit 2 Scores Across Groups (S19)") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

S20Plot <- ggplot(data=S20Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Time: Unit 2 Scores Across Groups (S21)") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

S21Plot <- ggplot(data=S21Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Time: Unit 2 Scores Across Groups (S21)") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)


F18Plot
F19Plot
S19Plot
S20Plot
S21Plot


```


# Perf Plots
```{r}
F18Score <- data.frame(Type = c("Baseline", "PO", "POE"), Avg = c(mean(subset(PerfF18_cleaned, U1Condition == "Baseline")$Text), mean(subset(PerfF18_cleaned, U1Condition == "PO")$PO), mean(subset(PerfF18_cleaned, U1Condition == "POE")$POE)),
                       error = c(sd(subset(PerfF18_cleaned, U1Condition == "Baseline")$Text), sd(subset(PerfF18_cleaned, U1Condition == "PO")$PO), sd(subset(PerfF18_cleaned, U1Condition == "POE")$POE)))

F19Score <- data.frame(Type = c("Text", "POE", "POMC"), Avg = c(mean(subset(PerfF19_cleaned, U1Condition == "Text")$Text.Only), mean(subset(PerfF19_cleaned, U1Condition == "POE")$POE), mean(subset(PerfF19_cleaned, U1Condition == "POMC")$POMC)), 
                       error = c(sd(subset(PerfF19_cleaned, U1Condition == "Text")$Text.Only), sd(subset(PerfF19_cleaned, U1Condition == "POE")$POE), sd(subset(PerfF19_cleaned, U1Condition == "POMC")$POMC)))

S19Score <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(subset(PerfS19_cleaned, U1Condition == "Control")$Control), mean(subset(PerfS19_cleaned, U1Condition == "Experimental")$Experimental)), 
                       sd = c(sd(subset(PerfS19_cleaned, U1Condition == "Control")$Control), sd(subset(PerfS19_cleaned, U1Condition == "Experimental")$Experimental)))

S20Score <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(subset(PerfS20_cleaned, U1Condition == "Control")$control), mean(subset(PerfS20_cleaned, U1Condition == "Experimental")$experimental)), 
                       sd = c(sd(subset(PerfS20_cleaned, U1Condition == "Control")$control), sd(subset(PerfS20_cleaned, U1Condition == "Experimental")$experimental)))

S21Score <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(subset(PerfS21_cleaned, U1Condition == "Control")$Text), mean(subset(PerfS21_cleaned, U1Condition == "Experimental")$POE)), 
                       sd = c(sd(subset(PerfS21_cleaned, U1Condition == "Control")$Text), sd(subset(PerfS21_cleaned, U1Condition == "Experimental")$POE)))


F18Plot <- ggplot(data=F18Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Perf: Unit 1 Scores Across Groups(F18)") + geom_errorbar(aes(x=Type, ymin=Avg-error/2, ymax=Avg+error/2), width=0.4, colour="orange", alpha=0.9, size=1.3)

F19Plot <- ggplot(data=F19Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Perf: Unit 1 Scores Across Groups (F19)") + geom_errorbar( aes(x=Type, ymin=Avg-error/2, ymax=Avg+error/2), width=0.4, colour="orange", alpha=0.9, size=1.3)

S19Plot <- ggplot(data=S19Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Perf: Unit 2 Scores Across Groups (S19)") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

S20Plot <- ggplot(data=S20Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Perf: Unit 2 Scores Across Groups (S21)") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

S21Plot <- ggplot(data=S21Score, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Perf: Unit 2 Scores Across Groups (S21)") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)


F18Plot
F19Plot
S19Plot
S20Plot
S21Plot


```

# ANOVA Tests for F18 and F19
Time for both semesters show a significant difference.
```{r}
da<-data.frame("hi","bye")
names(da)<-c("hello","goodbye")

de<-data.frame("hola","ciao")
names(de)<-c("hello","goodbye")

newdf <- rbind(da, de)

perfGetData <- function(df, condA, condB, condC){
  final <- data.frame("AnonID","val1", "val2")
  names(final) <- c("AnonID" ,"perfZ", "U1Condition")
  for (i in 1:nrow(df)){
    if(df$U1Condition[i] == condA){
      tmp <- data.frame(df[1][i,], df[2][i,], df$U1Condition[i])
      names(tmp) <- c("AnonID" ,"perfZ", "U1Condition")
    }
    else if(df$U1Condition[i] == condB){
      tmp <- data.frame(df[1][i,], df[3][i,], df$U1Condition[i])
      names(tmp) <- c("AnonID" ,"perfZ", "U1Condition")
    }
    else if(df$U1Condition[i] == condC){
      tmp <- data.frame(df[1][i,], df[4][i,], df$U1Condition[i])
      names(tmp) <- c("AnonID" ,"perfZ", "U1Condition")
    }
    final <- rbind(final, tmp)
  }
  return(final)
}

perfANOVAF18 <- perfGetData(PerfF18_cleaned, "PO", "POE", "Baseline")[-c(1),]
perfANOVAF19 <- perfGetData(PerfF19_cleaned, "POMC", "POE", "Text")[-c(1),]

```

Fall 2018 Summaries: 
```{r}
summary(aov(perfZ ~ U1Condition, data = perfANOVAF18))
summary(aov(totalTime ~ U1Condition, data = TimeF18_cleaned))
```

Fall 2019 summaries: 
```{r}
summary(aov(perfZ ~ U1Condition, data = perfANOVAF19))
summary(aov(totalTime ~ U1Condition, data = TimeF19_cleaned))
```


# Performance Tests
```{r}
t.test(subset(PerfS19_cleaned, U1Condition == "Control")$Control, subset(PerfS19_cleaned, U1Condition == "Experimental")$Experimental, var.equal=TRUE)


t.test(subset(PerfS20_cleaned, U1Condition == "Control")$control, subset(PerfS20_cleaned, U1Condition == "Experimental")$experimental, var.equal=TRUE)

t.test(subset(PerfS21_cleaned, U1Condition == "Control")$Text, subset(PerfS21_cleaned, U1Condition == "Experimental")$POE, var.equal=TRUE)
```

# Time Tests: S19 & S21 show a significant difference
```{r}

t.test(subset(TimeS19_cleaned, U1Condition == "Control")$totalTime, subset(TimeS19_cleaned, U1Condition == "Experimental")$totalTime, var.equal=TRUE)
t.test(subset(TimeS20_cleaned, U1Condition == "Control")$totalTime, subset(TimeS20_cleaned, U1Condition == "Experimental")$totalTime, var.equal=TRUE)
t.test(subset(TimeS21_cleaned, U1Condition == "Control")$totalTime, subset(TimeS21_cleaned, U1Condition == "Experimental")$totalTime, var.equal=TRUE)
```
