library(nlme)
# Oats example from:
# https://www.youtube.com/watch?v=VhMWPkTbXoY
data(Oats)
str(Oats)
# Classes ‘nfnGroupedData’, ‘nfGroupedData’, ‘groupedData’ and 'data.frame':	72 obs. of  4 variables:
#       $ Block  : Ord.factor w/ 6 levels "VI"<"V"<"III"<..: 6 6 6 6 6 6 6 6 6 6 ...
# $ Variety: Factor w/ 3 levels "Golden Rain",..: 3 3 3 3 1 1 1 1 2 2 ...
# $ nitro  : num  0 0.2 0.4 0.6 0 0.2 0.4 0.6 0 0.2 ...
# $ yield  : num  111 130 157 174 117 114 161 141 105 140 ...
# - attr(*, "formula")=Class 'formula'  language yield ~ nitro | Block
# .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
#       - attr(*, "labels")=List of 2
# ..$ y: chr "Yield"
# ..$ x: chr "Nitrogen concentration"
# - attr(*, "units")=List of 2
# ..$ y: chr "(bushels/acre)"
# ..$ x: chr "(cwt/acre)"
# - attr(*, "inner")=Class 'formula'  language ~Variety
# .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 

plot(Oats)

model1 <- lm(yield~Variety*nitro, data = Oats)
# Read this as, yield is a function (~) of Variety and (*) nitro(gen), take the data from the dataframe, Oats
summary(model1)
# Call:
#       lm(formula = yield ~ Variety * nitro, data = Oats)
# 
# Residuals:
#       Min      1Q  Median      3Q     Max 
# -35.950 -14.967  -1.258  12.675  52.050 
# 
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               81.900      7.342  11.155  < 2e-16 ***
# VarietyMarvellous          8.517     10.383   0.820 0.415033    
# VarietyVictory            -8.600     10.383  -0.828 0.410506    
# nitro                     75.333     19.622   3.839 0.000279 ***
# VarietyMarvellous:nitro  -10.750     27.750  -0.387 0.699718    
# VarietyVictory:nitro       5.750     27.750   0.207 0.836487    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.5 on 66 degrees of freedom
# Multiple R-squared:  0.4134,	Adjusted R-squared:  0.369 
# F-statistic: 9.303 on 5 and 66 DF,  p-value: 9.66e-07

# INTERPRETATION: The coefficients table contains the results of this linear model (lm).  Intercept, estimate shows 
# the value for the primary (in this case the variety, Golden Rain) variable (in this case, variety).  The second 
# row, VarietyMarvellous shows the difference of the estimate of the intercept (first row) for the next variety (in 
# this example, the varieties are taken in alphabetical order).  This would be interpreted as the intercept would be 
# 8.517 higher than the first intercept.  Likewise, the third row is the intercept for Victory.  Then we start with 
# fertilizer for the Golden Rain, which gives us the slope of m = 75.333.  Similarly, the next two rows give the 
# slope differences for the next two varieties.

confint(model1) # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/confint.html
#                         2.5 %    97.5 %
#       (Intercept)        67.24122  96.55878
# VarietyMarvellous       -12.21398  29.24731
# VarietyVictory          -29.33065  12.13065
# nitro                    36.15609 114.51058
# VarietyMarvellous:nitro -66.15499  44.65499
# VarietyVictory:nitro    -49.65499  61.15499

model2 <- lme(yield~Variety*nitro, data = Oats, random = ~1|Block/Variety)
# Here, this will be the linear model with fixed Variety and nitro, with random effects from the Block (duplicates) 
# and even in Variety.  I question the use of variety.  We'll see what happens.
summary(model2)
# Linear mixed-effects model fit by REML              Restricted maximum likelihood
# Data: Oats 
#      AIC      BIC    logLik                         These are goodness of fit for comparing to other models
# 581.2372 600.9441 -281.6186
# 
# Random effects:
#       Formula: ~1 | Block                           Variances due to the block
#           (Intercept)
# StdDev:      14.64485
# 
# Formula: ~1 | Variety %in% Block
#           (Intercept) Residual                      Variance and residual variance due to Variety
# StdDev:      10.39931 12.99039
# 
# Fixed effects: yield ~ Variety * nitro 
#                             Value Std.Error DF   t-value p-value
# (Intercept)              81.90000  8.570709 51  9.555802  0.0000
# VarietyMarvellous         8.51667  8.684675 10  0.980655  0.3499
# VarietyVictory           -8.60000  8.684675 10 -0.990250  0.3454      The intercept parameters don't change.
# nitro                    75.33333 11.858549 51  6.352660  0.0000
# VarietyMarvellous:nitro -10.75000 16.770521 51 -0.641006  0.5244
# VarietyVictory:nitro      5.75000 16.770521 51  0.342864  0.7331      Neither do the slopes, the S.E. changes a lot
# Correlation: 
#                         (Intr) VrtyMr VrtyVc nitro  VrtyM:
# VarietyMarvellous       -0.507                            
# VarietyVictory          -0.507  0.500                     
# nitro                   -0.415  0.410  0.410              
# VarietyMarvellous:nitro  0.294 -0.579 -0.290 -0.707       
# VarietyVictory:nitro     0.294 -0.290 -0.579 -0.707  0.500
# 
# Standardized Within-Group Residuals:
#       Min          Q1         Med          Q3         Max 
# -1.78878616 -0.64954437 -0.06301233  0.57818020  1.63463799 
# 
# Number of Observations: 72
# Number of Groups: 
#       Block Variety %in% Block 
# 6                 18

plot(ranef(model2))
plot(model2)
# Both plots should show symetry about the random effects (first) and standardized residuals (second)

# Application to the air quality study, precip may be taken as fixed or random effect.  Perhaps time as a fixed 
# effect - code different periods in air quality control at Clairton as different "blocks".

