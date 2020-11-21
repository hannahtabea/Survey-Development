#-------------------------------------------------------------------------------------------
# Template Factor analysis,
# Author: Hannah Wnendt, draft: 30.04.2020
# #--------------------------------------------------------------------------


# #--------------------------------------------------------------------------

current_date <- Sys.Date()

#--------------------------------------------------------------------------
# THEORY DEVELOPMENT

# FACTOR ANALYSIS = a statistical method used to describe variability among 
#observed, correlated variables in terms of a potentially lower number of 
#unobserved variables called factors

# How many factors are necessary to explain relations among items?

#--------------------------------------------------------------------------
# create fake data
#--------------------------------------------------------------------------

#load data but remove empty row
d <- fread("excel.csv",skip = 2) 
# skip the last 2 columns (open questions)
d <- d[,1:36]

raw_data <- d %>% mutate(subj = 1:nrow(d))
#copy data to work with
d <- raw_data

#generate random sentences (=items) for every column except for subj,group and department (=3), 
#length 7:12 words
colnames(d) [c(1:(length(d)-3))] <- randomSentences((length(d)-3), 7:12)

#--------------------------------------------------------------------------
# deal with missing data
#--------------------------------------------------------------------------
#install.packages("mice")
library(mice)
imputed_Data <- mice(raw_data, m=5,meth='pmm', seed = 5)
summary(imputed_Data)
# 
data_wide<-mice::complete(imputed_Data,1)
colnames(data_wide) <- colnames(d)

#-----------------------------------------------------------------------
# CORRELATION MATRIX
#-----------------------------------------------------------------------
#to which extent do the scores for dimensions correlate with each other?

# exclude subj ID and open questions
df <- data_wide[,1:35]

# calulate the correlations
r <- round(cor(df, use= "pairwise.complete.obs"),2) 
r


library(ggplot2)
library(ggcorrplot)

ggcorrplot(round(cor(raw_data[,1:35], use= "pairwise.complete.obs"),2), 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

#alternatives
library(corrplot)
library(RColorBrewer)
corrplot(round(cor(raw_data[,1:35], use= "pairwise.complete.obs"),2), type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

# with histograms, significance levels and scatterplot
library(PerformanceAnalytics)
chart.Correlation(df, histogram=TRUE, pch=19)

#---------------------------
# Reliability coefficients
# Estimate coefficient alpha
alpha(data_wide)

# Calculate split-half reliability
splitHalf(r)


#-------------------------------------------------------------------------------------
# CALCULATE EIGENVALUES AND SCREE PLOT
#-------------------------------------------------------------------------------------
# how many dimensions are reflected in the data?
# determine the number of factors to extract

# Eigenvalues - measure the amount of variation in the total sample accounted for by each factor. 
# require(nFactors)
# ev <- eigen(cor(df)) # get eigenvalues
# ap <- parallel(subject=nrow(df),var=ncol(df),
#                rep=100,cent=.05)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# plotnScree(nS) 

# Screeplot (based on Eigenvalues) -> look for the point where the graph hits the line
scree(r, factors = FALSE)

#-------------------------------------------------------------------------------------
# ACTUAL FACTOR ANALYSIS
#-------------------------------------------------------------------------------------
EFA_model <- fa(df, nfactors = 8)

# Interpretation Fit statistics
# Chi-square test: Non-significant result
# Tucker Lewis Index (TLI): > 0.90
# Root Mean Square Error of Approximation (RMSEA): < 0.05

# see results
EFA_model

# loadings 
# are there items that do not clearly load onto one factor? (<0.2 on primary factor)
EFA_model$loadings

#-------------------------------------------------------------------------------------
# REFINE FACTOR ANALYSIS
#-------------------------------------------------------------------------------------
# drop poorly loading item
df_34 <- df[,-35]

# Create new factor analysis
EFA_model_34 <- fa(df_34,nfactors = 8)

# Factor loadings
EFA_model_34$loadings

#-------------------------------------------------------------------------------------
# SELECT BEST FITTING MODEL
#-------------------------------------------------------------------------------------

theory <- fa(df, nfactors = 4)
eigen_35 <- fa(df, nfactors = 8)
eigen_34 <- fa(df_34,nfactors = 8)

# Compare the BIC values, model with the lowest BIC is preferred, penalizes the complexity of the model
theory$BIC
eigen_34$BIC
eigen_35$BIC

anova(eigen_34,eigen_35)



#----------------------------------------------
# BACKUP
# # Principal component analysis
# # entering raw data and extracting 8 factors,
# # with varimax rotation
# fit <- factanal(df, 8, rotation="varimax")
# print(fit, digits=2, cutoff=.3, sort=TRUE)
# # loadings = squared factor loading is the percent of variance in that indicator variable 
# # explained by the factor
# 
# 
# #----------------------------------------------------
# #principal components analysis - PCA
# # Factor weights are computed to extract the maximum possible variance, with successive factoring 
# #continuing until there is no further meaningful variance left
# require(GPArotation)
# # if you already know the number of components
# pa <-principal(df, nfactors = 3, rotate = "varimax")
# pa
# 
# 
# #library(devtools)
# #install_github("vqv/ggbiplot")
# #https://blog.bioturing.com/2018/06/18/how-to-read-pca-biplots-and-scree-plots/
# library(ggbiplot)
# role.pca <- prcomp(df,
#                    scale. = TRUE) 
# #how strongly do the items influence each of the principal components?
# ggbiplot(role.pca, obs.scale = 1, var.scale = 1,
#          ellipse = TRUE, circle = TRUE) +
#   scale_color_discrete(name = '') +
#   theme(legend.direction = 'horizontal', legend.position = 'top')
