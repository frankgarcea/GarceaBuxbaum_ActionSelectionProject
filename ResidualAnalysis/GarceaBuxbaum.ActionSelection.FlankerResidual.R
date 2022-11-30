
library("dplyr")
library("ggplot2")
library("lme4")
library("readxl")
library("MASS")
library("performance")

# load data of high and low conflict RT from the flanker task.
data <- read_excel("ConflictResProject.FlankerData.xlsx")

# log transform RTs
data$CongruentLog <- log(data$Congruent)
data$IncongruentLog <- log(data$Incongruent)

# create factors.
Factor <- c("Congruent", "Incongruent","CongruentLog","IncongruentLog")
data[Factor] <- lapply(data[Factor], as.numeric)

### Control for variance in low conflict (congruent log RT) when investigating high conflict (incongruent log RT)
model <- lm(IncongruentLog ~ CongruentLog, na.action = na.exclude, data = data)
summary(model)
r <-cor(data$IncongruentLog, data$CongruentLog, use = "pairwise.complete.obs")

# add residuals to data structure
data$Flanker_Incongruent_Resid <- residuals(model)

# check model parameters
check_model(model)

# investigate whether model appears to be normally distributed.
check_normality(model)

# QQ plot of residuals to visualize the data
plot(check_normality(model), type = "qq")

### SAVE
write.csv(data,'ConflictResProject.Flanker.IncongruentResiduals.LogTransformed.csv')


