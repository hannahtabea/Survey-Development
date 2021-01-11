#-------------------------------------------------------------------------------------------
# Template Factor analysis,
# Author: Hannah Wnendt, draft: 10.01.2020
# #--------------------------------------------------------------------------

# Confirmatory Factor Analyis = Do number of factors match what is expected on the basis of theory?
# Hypothesis-Testing of predefined structure of a latent variable



# b) CFA (check multivariate normality, cfa with lavaan,
# check loadings and summary statistics, what if it does not fit? (SEM with lavaan), plot pathplot)
#---------------------------------------------






# #--------------------------------------------------------------------------

current_date <- Sys.Date()

#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
# load data
#--------------------------------------------------------------------------

library(qgraph)
data("big5")

#---------------------------------------------------------------------------
# prepare lavaan syntax
#---------------------------------------------------------------------------

library(dplyr)
#save big5-specific item names
extra_names <- as.data.frame(big5) %>% select(starts_with("E")) %>% colnames() 
agree_names <- as.data.frame(big5) %>% select(starts_with("A")) %>% colnames() 
neuro_names <- as.data.frame(big5) %>% select(starts_with("N")) %>% colnames() 
open_names  <- as.data.frame(big5) %>% select(starts_with("O")) %>% colnames() 
con_names  <- as.data.frame(big5) %>% select(starts_with("C")) %>% colnames() 

#-----------------------------------------------------------------------------------
# prepare syntax
#-----------------------------------------------------------------------------------

library(lavaan)

big5_CFAmodel <- 'EXTRA =~ E2 + E7 + E12  + E22 + E27 + E32 + E37 + E42
                  AGREE =~ A4 + A9 + A14 + A19 + A24 + A29 + A34 + A39
                  NEURO =~ N1 + N6 + N11 + N16 + N21 + N26 + N31 + N36
                  OPEN  =~ O3 + O8 + O13 + O18 + O23 + O28 + O33 + O38 
                  CON   =~ C5 + C10 + C15 + C20 + C25 + C30 + C40 + C45'

# EXTRA ~ NEURO -> neuroticism predicts extraversion directly
# EXTRA ~~ NEURO -> extraversion is expected to covary with neuroticism
# EXTRA ~~ 0*NEURO -> extraversion and neuroticism are not expected to correlate at all

#---------------------------------------------------------------------------
# check assumptions 
#---------------------------------------------------------------------------

#save selected variables in dataframe
big5_selection <-as.data.frame(big5) %>% select(E2,E7,E12,E22,E27,E32,E37,E42,
                                                A4,A9,A14,A19,A24,A29,A34,A39, 
                                                N1,N6,N11,N16,N21,N26,N31,N36,
                                                O3,O8,O13,O18,O23,O28,O33,O38,
                                                C15,C20,C25,C30,C40,C45)

library(psych)
# Mardia test of multivariate normality
mardia(big5_selection)

# CFAs are based on chi-squared tests and therefore assume normally distributed residuals
# if p-values for skew and kurtosis are < .05,
# it appears that the distributions differ from a normal distribution.
# -> need correction

#-----------------------------------------------------------------------------------
# Fit the model to the data using robust standard errors
#-----------------------------------------------------------------------------------

big5_CFA <- cfa(model = big5_CFAmodel,
                        data = big5, estimator = "MLR")

#-----------------------------------------------------------------------------------
# Summarize results --
# Include fit measures and standardized estimates
#-----------------------------------------------------------------------------------

summary(big5_CFA, fit.measures = TRUE,
        standardized = TRUE)
# interpretation for good model fit:
# Chi-squared p-value <. 05 (depends on sample size)
# Comparative Fit index (CFI) > .90
# Tucker-Lewis index (TLI) > .90
# Root mean square error of approximation (RMSEA) < .05

# Covariance = amount by which 2 variables change together (standardized.all)
# -> overlap in the data = covariance squared

#-------------------------------
# shortcut
# Access individual fit measures
#-------------------------------
fitMeasures(big5_CFA,
            fit.measures = c("cfi","tli", "rmsea"))

#-----------------
# inspect loadings
#-----------------
# Interpretation:
# Factor loadings are similar like a regression coefficient, e.g.
# the first parameter says that for each 1 SD increase in the latent variable, 
# the model predicts a .XX-unit increase in the manifest variable. -> how strongly are manifest variables expected to change with the latent variable?
# rule of thumb : each standardized loading should be above .3 for good fitting models
inspect(big5_CFA, "std")$lambda



#fit statistics in standardized format
results <- standardizedsolution(big5_CFA)

#-----------------------------------------------------------------------------------
# Plot the factor structure
#-----------------------------------------------------------------------------------

# load semPlot
library(semPlot)
# diagram model
semPaths(big5_CFA,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree", rotation = 2)

#------------------------------------------------------------------------------------
# Improve poor-fittig model
#------------------------------------------------------------------------------------

# remove poorly loading variables and replace them
# adjust model structure
big5_CFAmodel2 <-'EXTRA =~ E7 + E12  + E22 + E27 + E37 + E42   
                 AGREE =~ A4 + A9 + A14 + A19 + A24 + A49 
                NEURO =~ N1 + N11 + N16 + N26 + N31 + N41
                OPEN  =~ O13 + O23  + O33 + O38 + O48 + O53
                CON   =~ C20 + C25 + C30 + C40 + C50 + C55
                G     =~ EXTRA + CON + OPEN + AGREE + NEURO'

# fit hopefully improved model
big5_CFA2 <- cfa(model = big5_CFAmodel2,
                data = big5, estimator = "MLR")

# get loadings
inspect(big5_CFA2, "std")$lambda


# get fit indices
fitMeasures(big5_CFA2,
            fit.measures = c("cfi","tli", "rmsea"))

# get modification indices -> suggested changes on the model and what would happen to the estimates if it was added
# change model one by one
modificationindices(big5_CFA2, sort = TRUE)

#------------------------------------------------------------------------------------
# Compare models
#------------------------------------------------------------------------------------

# only useful for models with same variables and different specification
anova(big5_CFA,big5_CFA2)

# fit index comparison
fitmeasures(big5_CFA, c("aic","ecvi"))
# smaller than the original model? :)
fitmeasures(big5_CFA2, c("aic","ecvi"))
#------------------------------------------------------------------------------------
# What if the model did not converge? 
# -> Heywood cases