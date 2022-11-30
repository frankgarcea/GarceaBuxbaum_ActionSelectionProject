
library("dplyr")
library("ggplot2")
library("lme4")
library("readxl")
library("MASS")
library("performance")

# load data of hand posture accuracy scores in the test of grasp-use conflict.
data <- read_excel("ConflictResProject.GraspUseHPData.xlsx")

Factor <- c("GraspUse_HP_Conflict", "GraspUse_HP_Nonconflict")
data[Factor] <- lapply(data[Factor], as.numeric)

### Control for variance in low conflict (nonconflict GTS HP) when investigating high conflict (conflict GTS HP)
model <- lm(GraspUse_HP_Conflict ~ GraspUse_HP_Nonconflict, na.action = na.exclude, data = data)
summary(model)
r <-cor(data$GraspUse_HP_Conflict, data$GraspUse_HP_Nonconflict, use = "pairwise.complete.obs")

# add residuals to data structure
data$GraspUse_HP_ConflictResid <- residuals(model)

# check model parameters
check_model(model)

# investigate whether model appears to be normally distributed.
check_normality(model)

# QQ plot of residuals to visualize the data
plot(check_normality(model), type = "qq")

### SAVE
write.csv(data,'ConflictResProject.GraspUseHP.Residuals.csv')


