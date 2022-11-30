# This is a script to perform a two-way repeated measures AVOVA of the stroop conflict resolution

library("readxl")
library("rstatix")
library("tidyverse")
library("dplyr")
library("ggpubr")
library("plyr")
library("performance")

# Load in CSV of GTS data
GTSdata <- read.csv("ApraxiaAnalysis_GraspUseConflict.csv")
GTSdata <- as.data.frame(GTSdata)
convert_as_factor(GTSdata,SubID,Response,Congruency,Group)

# run repeated measures anova.
GTSAnova <- anova_test(data = GTSdata, dv = Response, wid = SubID, between = Group, within = Congruency, effect.size = "pes")
get_anova_table(GTSAnova,correction = "GG")

# Load in CSV of Flanker data
Flankerdata <- read.csv("ApraxicAnalysis_Flanker.csv")
Flankerdata <- as.data.frame(Flankerdata)
convert_as_factor(Flankerdata,SubID,Response,Congruency,Group)
Flankerdata$LogRT <- log(Flankerdata$Response)

# run repeated measures anova.
FlankerAnova <- anova_test(data = Flankerdata, dv = LogRT, wid = SubID, between = Group, within = Congruency, effect.size = "pes")
get_anova_table(FlankerAnova,correction = "GG")

# Load in CSV of Flanker data
Simondata <- read.csv("ApraxicAnalysis_Simon.csv")
Simondata <- as.data.frame(Simondata)
convert_as_factor(Simondata,SubID,Response,Congruency,Group)
Simondata$LogRT <- log(Simondata$Response)

# run repeated measures anova.
SimonAnova <- anova_test(data = Simondata, dv = LogRT, wid = SubID, between = Group, within = Congruency, effect.size = "pes")
get_anova_table(FlankerAnova,correction = "GG")


