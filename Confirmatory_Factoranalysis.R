#-------------------------------------------------------------------------------------------
# Template Factor analysis
# #--------------------------------------------------------------------------

# Confirmatory Factor Analyis = Do number of factors match what is expected on the basis of theory?
# Hypothesis-Testing of predefined structure of a latent variable

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

prep_formula <- function(abbrev) {
  
  x <- as.data.frame(big5) %>% 
    select(starts_with(abbrev)) %>% 
    colnames() %>% noquote() %>% 
    paste(collapse = " + ")
  
  return(x)
}

#save big5-specific item names
extra_names <- prep_formula(abbrev = "E")
agree_names <- prep_formula(abbrev = "A")
neuro_names <- prep_formula(abbrev = "N") 
open_names  <- prep_formula(abbrev = "O") 
con_names  <-  prep_formula(abbrev = "C")

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
                CON   =~ C20 + C25 + C30 + C40 + C50 + C55'

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
#------------------------------------------------------------------------------------

# -> Heywood cases = correlationns between variables are out of bounds (sum up to > 1) or cfa includes negative variances

# e.g. WARNING: covariance matrix of latent variables is not positive definite; use lavInspect(fit, "cov.lv") to investigate.
#find the problem 
# - are two of the latent variables so highly correlated they should be merged to a single one?
# change formula
summary(big5_CFA2, standardized = TRUE,
        fit.measures = TRUE)

# negative variances?
# - are manifest variables non-normal/highly skewed?
# - do the variables lie on a similar scale?

big5_neg.model <-'EXTRA =~ E7 + E12  + E22 + E27 + E37 + E42   
                 AGREE =~ A4 + A9 + A14 + A19 + A24 + A49 
                 NEURO =~ N1 + N11 + N16 + N26 + N31 + N41
                 OPEN  =~ O13 + O23  + O33 + O38 + O48 + O53
                 CON   =~ C20 + C25 + C30 + C40 + C50 + C55
                 G     =~ EXTRA + AGREE + NEURO + OPEN + CON'

# fit hopefully improved model
big5_neg.fit <- cfa(model = big5_neg.model,
                 data = big5, estimator = "MLR")

# find negative variance
summary(big5_neg.fit, standardized = TRUE,
        fit.measures = TRUE, rsquare = TRUE)

# specify variance in the model manually
as.data.frame(big5) %>% select(N1,N11,N16,N26,N31,N41) %>% var() %>% sum()

big5_neg.model.corrected <-'EXTRA =~ E7 + E12  + E22 + E27 + E37 + E42   
                             AGREE =~ A4 + A9 + A14 + A19 + A24 + A49 
                             NEURO =~ N1 + N11 + N16 + N26 + N31 + N41
                             OPEN  =~ O13 + O23  + O33 + O38 + O48 + O53
                             CON   =~ C20 + C25 + C30 + C40 + C50 + C55
                             G     =~ EXTRA + AGREE + NEURO + OPEN + CON
                             NEURO ~~ 19.76 * NEURO '

#try again
big5_neg.fit.corrected <- cfa(model = big5_neg.model.corrected,
                            data = big5, estimator = "MLR")

# find negative variance
summary(big5_neg.fit.corrected, standardized = TRUE,
        fit.measures = TRUE, rsquare = TRUE)

# does not work in this example either because model is theoretically misspecified (no g-factor)

