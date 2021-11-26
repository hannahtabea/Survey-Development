#-------------------------------------------------------------------------------------------
# Template Factor analysis
# #--------------------------------------------------------------------------

# Confirmatory Factor Analyis = Do number of factors match what is expected on the basis of theory?
# Hypothesis-Testing of predefined structure of a latent variable

#--------------------------------------------------------------------------
# load data
#--------------------------------------------------------------------------

library(data.table)
big5_huge <- fread('IPIP-FFM-data-8Nov2018.csv', select = seq(1:50))

recode_var <- function(x) (
  dplyr::recode(x,
         `1`= 5L,
         `2`= 4L,
         `3`= 3L,
         `4` = 2L,
         `5` = 1L)
)

library(hablar)
big5 <- big5_huge %>% 
        sample_n(500) %>% 
        retype() %>%
        mutate_at(c("EXT2","EXT4","EXT6","EXT8","EXT10",
                    "EST2","EST4",
                    "AGR1","AGR3","AGR5","AGR7",
                    "CSN2","CSN4","CSN6","CSN8",
                    "OPN2","OPN4","OPN6"), recode_var)
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
set.seed(27)
extra_names <- prep_formula(abbrev = "EXT")
agree_names <- prep_formula(abbrev = "AGR")
emo_names <- prep_formula(abbrev = "EST") 
open_names  <- prep_formula(abbrev = "OPN") 
con_names  <-  prep_formula(abbrev = "CSN")
#---------------------------------------------------------------------------
# check assumptions 
#---------------------------------------------------------------------------

library(psych)
# Mardia test of multivariate normality
QuantPsyc::mult.norm(big5)$mult.test

# CFAs are based on chi-squared tests and therefore assume normally distributed residuals
# if p-values for skew and kurtosis are < .05,
# it appears that the distributions differ from a normal distribution.
# -> need correction

#-----------------------------------------------------------------------------------
# specify BASIC BIG 5 model
#-----------------------------------------------------------------------------------

library(lavaan)
# latent variables can be named anything except for variable names from the dataset
# 'LATENT =~ manifest1 + manifest2 + ....'
big5_CFAmodel <- 'EXTRA =~ EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT6 + EXT7 + EXT8 + EXT9 + EXT10 
                  AGREE =~ AGR1 + AGR2 + AGR3 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10
                  EMO   =~ EST1 + EST2 + EST3 + EST4 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10 
                  OPEN  =~ OPN1 + OPN2 + OPN3 + OPN4 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10 
                  CON   =~ CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + CSN10'

# EXTRA ~ NEURO -> neuroticism predicts extraversion directly
# EXTRA ~~ NEURO -> extraversion is expected to covary with neuroticism
# EXTRA ~~ 0*NEURO -> extraversion and neuroticism are not expected to correlate at all


#-----------------------------------------------------------------------------------
# Fit the model to the data using robust standard errors
#-----------------------------------------------------------------------------------

big5_CFA <- cfa(model = big5_CFAmodel,
                        data = big5, estimator = "MLR")


#-----------------------------------------------------------------------------------
# Fit the model to the data using robust standard errors
#-----------------------------------------------------------------------------------
summary(big5_CFA, fit.measures = TRUE,
        standardized = TRUE)
# interpretation for good model fit:
# Chi-squared p-value <. 05 (depends on sample size)
# Comparative Fit index (CFI) > .90
# Tucker-Lewis index (TLI) > .90
# Root mean square error of approximation (RMSEA) < .05

# Covariance = amount by which two variables change together (standardized.all)
# -> overlap in the data  = correlation squared
# e.g., EXTRA ~~ NEURO correlate negatively by - 0.64, EXTRA ~~ OPEN by 0.4 which means that they share 16% of the variance in the data
# standardized loading show to which degree the latent variable can be measured by each indicator/item

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
standardizedsolution(big5_CFA)


#------------------------------------------------------------------------------------
# Adjust the model - HIGHER ORDER MODEL
#------------------------------------------------------------------------------------

# adjust model structure
big5_CFAmodel_higher_order<-'EXTRA =~ EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT7 + EXT8 + EXT9 + EXT10
                             AGREE =~ AGR1 + AGR2 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10
                             EMO   =~ EST1 + EST2 + EST3 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10
                             OPEN  =~ OPN1 + OPN2 + OPN3 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10
                             CON   =~ CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9
                             ALPHA =~ AGREE + CON + EMO
                             BETA =~ EXTRA + OPEN'

# # fit hopefully improved model
big5_CFA_higher_order <- cfa(model = big5_CFAmodel_higher_order,
                data = big5, estimator = "MLR")

summary(big5_CFA_higher_order, fit.measures = TRUE,
        standardized = TRUE, rsquare = TRUE)

# get fit indices
fitMeasures(big5_CFA_higher_order,
            fit.measures = c("cfi","tli", "rmsea"))

#------------------------------------------------------------------------------------
# Adjust the model - BLENDED MODEL
#------------------------------------------------------------------------------------

#EXTRA =~ AGR2 (I am interested in people.) AGR7 (I am not really interested in others.)
#OPEN =~ CSN10 (I am exacting in my work.) EXT6 (I have little to say.)
#AGREE =~ EST3 (I worry about things.) EXT3 (I feel comfortable around people.)
#EMO =~ CSN4 (I make a mess of things.) AGR10 (I make people feel at ease.??) + AGR6 (I have a soft heart.)
#CON =~ OPN7 (I am quick to understand things.) OPN9 (I spend time reflecting on things.)
big5_CFAmodel_blended <-'EXTRA =~ EXT1 + EXT2 + EXT3 + EXT4 + EXT5 + EXT7 + EXT8 + EXT9 + EXT10 + AGR2 + AGR7
                         AGREE =~ AGR1 + AGR2 + AGR4 + AGR5 + AGR6 + AGR7 + AGR8 + AGR9 + AGR10 + EST3 + EXT3
                         EMO   =~ EST1 + EST2 + EST3 + EST5 + EST6 + EST7 + EST8 + EST9 + EST10 + CSN4 + AGR6
                         OPEN  =~ OPN1 + OPN2 + OPN3 + OPN5 + OPN6 + OPN7 + OPN8 + OPN9 + OPN10 + CSN10 + EXT6
                         CON   =~ CSN1 + CSN2 + CSN3 + CSN4 + CSN5 + CSN6 + CSN7 + CSN8 + CSN9 + OPN7 + OPN9'

big5_CFA_blended <- cfa(model = big5_CFAmodel_blended,
                        data = big5, estimator = "MLR")

summary(big5_CFA_blended, fit.measures = TRUE,
        standardized = TRUE)

# get fit indices
fitMeasures(big5_CFA_blended,
            fit.measures = c("cfi","tli", "rmsea"))


#------------------------------------------------------------------------------------
# Compare models
#------------------------------------------------------------------------------------

# only useful for models with same variables and different specification
# anova(big5_CFA_blended,big5_CFA)

# fit index comparison - akaike information criterion = estimator of prediction error and thereby relative quality of statistical models for a given set of data.
# ecvi = expected cross validation index = likelihood this model will replicate with the same sample size and population
fitmeasures(big5_CFA_blended, c("aic","ecvi"))
# smaller than the original model?)
fitmeasures(big5_CFA, c("aic","ecvi"))

#-----------------------------------------------------------------------------------
# Plot the factor structure
#-----------------------------------------------------------------------------------

# load semPlot
library(semPlot)
# diagram model
semPaths(big5_CFA, layout = "tree")
semPaths(big5_CFA_higher, layout = "tree")

cormat_big5 <- cor(big5)
library(corrplot)
corrplot(cormat_big5, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.5)

#------------------------------------------------------------------------------------
# What if the model still did not converge? 
#------------------------------------------------------------------------------------
# get modification indices -> suggested changes on the model and what would happen to the estimates if it was added
# change model one by one
modificationindices(big5_CFA, sort = TRUE)

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

# adjust for negative variances by specifying variance in the model manually
as.data.frame(big5) %>% select(starts_with("AGR")) %>% var() %>% sum()
as.data.frame(big5) %>% select(starts_with("EST")) %>% var() %>% sum()
as.data.frame(big5) %>% select(starts_with("CSN")) %>% var() %>% sum()

