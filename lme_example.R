library(nlme)
names(Orthodont)
# [1] "distance" "age"      "Subject"  "Sex"
levels(Orthodont$Sex)
# [1] "Male"   "Female"
OrthoFem <- Orthodont[Orthodont$Sex=="Female",]

fmlOrthF <- lme(distance~age, data=OrthoFem, random = ~1|Subject)
summary(fmlOrthF)
# Linear mixed-effects model fit by REML
# Data: OrthoFem 
# AIC     BIC    logLik
# 149.2183 156.169 -70.60916
# 
# Random effects:
#       Formula: ~1 | Subject
# (Intercept)  Residual
# StdDev:     2.06847 0.7800331
# 
# Fixed effects: distance ~ age 
# Value Std.Error DF   t-value p-value
# (Intercept) 17.372727 0.8587419 32 20.230440       0
# age          0.479545 0.0525898 32  9.118598       0
# Correlation: 
#       (Intr)
# age -0.674
# 
# Standardized Within-Group Residuals:
#       Min         Q1        Med         Q3        Max 
# -2.2736479 -0.7090164  0.1728237  0.4122128  1.6325181 
# 
# Number of Observations: 44
# Number of Groups: 11 

# This gives a std = 2.06847 and intercept of 0.7800331
age <- c(8,14)
dist <- 0.7800331+2.06847*age
plot(OrthoFem$age, OrthoFem$distance, type = "p")
par(new=T)
plot(age, dist, type = "l")
