#--------------------------------------------------------------------------
# ITEM ANALYSIS 
# Author: Hannah Wnendt, 08.10.2020, 21.11.2020
#--------------------------------------------------------------------------

current_date <- Sys.Date()
#path_plot <-("C:/Users/Hannah Wnendt/Desktop/Mediatum/R/Plots")
path_plot <- ("C:/Users/hanna/Dropbox/Methods_2019_2020/R-Scripts/Survey Development/Plots")
#------------------------------------------------------------------------------------------------------
# LIBRARY
#------------------------------------------------------------------------------------------------------
library(data.table)
library(tidyverse)
library(stringi)
library(OpenRepGrid)
library(dplyr)
library(reshape2)

library(psych)
library(car)
library(likert)
library(naniar)
library(skimr)
library(mice)



#------------------------------------------------------------------------------------------------------
# LOAD DATA
#------------------------------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------------------------------
# SKIM DATA AND GET THRESHOLDS (PERCENTILES)
#------------------------------------------------------------------------------------------------------
skim(d)

#------------------------------------------------------------------------------------------------------
# WRANGLE DATA 
#------------------------------------------------------------------------------------------------------

# IS THERE A REVERSE PATTERN?
# Get response frequencies, but skip non-likert variables
response.frequencies(d[,c(1:(length(d)-3))])


data_wide <- d %>% 
  # reverse code items
  # !!! not necessary if recoding has already be done by 2ask!!!
  # mutate(c("item1","item2"), funs(recode(., `1`= 7, `2`=6, .default = NaN)))
  
  # deselect irrelevant columns
  select(-c(V35,V36))

data <- data_wide %>%
        gather(key = "names", value ="value",-subj) %>%
        # asign topics
            # !!! not necessary if scale sums are already built by 2ask!!!
        mutate(names = as.factor(names),
               topic = ifelse(names == colnames(data_wide)[1] | names == colnames(data_wide) [2] | names == colnames(data_wide) [3] | names == colnames(data_wide) [4] | names == colnames(data_wide) [5] | names == colnames(data_wide) [6] | names == colnames(data_wide) [7], "Generell",
                              ifelse(names == colnames(data_wide)[8] | names == colnames(data_wide) [9] | names == colnames(data_wide) [10] |  names == colnames(data_wide) [11]|  names == colnames(data_wide) [12] | names == colnames(data_wide) [13] | names == colnames(data_wide) [14] | names == colnames(data_wide) [15], "Einflussnahme",
                                     ifelse(names == colnames(data_wide)[16] | names == colnames(data_wide) [17] | names == colnames(data_wide) [18] | names == colnames(data_wide) [19] | names == colnames(data_wide) [20] | names == colnames(data_wide) [21] | names == colnames(data_wide) [22] | names == colnames(data_wide) [23] | names == colnames(data_wide) [24], "Persoenlichkeit",
                                            ifelse(names == colnames(data_wide)[25] | names == colnames(data_wide) [26] | names == colnames(data_wide) [27] | names == colnames(data_wide) [28] | names == colnames(data_wide) [29] |names == colnames(data_wide) [30] | names == colnames(data_wide) [31] | names == colnames(data_wide) [32] | names == colnames(data_wide) [33] | names == colnames(data_wide) [34] , "Innovation", "")))),
               response = factor(case_when(
                 #value == 0 ~ "nicht beantwortet",
                 value == 1 ~ "nie",
                 value == 2 ~ "selten",
                 value == 3 ~ "manchmal",
                 is.na(value) ~ "nicht beantwortet",
                 value == 4 ~ "teilweise",
                 value == 5 ~ "oft",
                 value == 6 ~ "immer"
               ), levels = c("nie","selten","manchmal","nicht beantwortet","teilweise", "oft", "immer"))) 


#------------------------------------------------------------------------------------------------------
# DEAL WITH MISSINGS
#------------------------------------------------------------------------------------------------------

#get an overview over missings
#--> variables
miss_var_summary <-miss_var_summary(d)
View(miss_var_summary)
# --> subjects
miss_case_summary <- miss_case_summary(d)
View(miss_case_summary)

#----------------------------------------
# visualize missings
#----------------------------------------
# detect the generall patterns
vis_miss(d)
# detect the problematic variables
gg_miss_var(d)
# does the missingness in one variable coincide with the missingness in another?
gg_miss_upset(d)

#-----------------------------------
# imputation
#create shadow matrix and add bad imputation method (mean imputation)
raw_miss_mean <- raw %>%
  bind_shadow(only_miss = TRUE) %>%
  impute_mean_all() %>%
  add_label_shadow()

#------------------
# impute data with mice
#install.packages("mice")
# library(mice)
# 
# # create shadow matrix so we keep track of the values that were missing before imputation
# # but need to use raw data so we have no weird column names
# data_shadow <- raw_data %>% bind_shadow() %>% add_label_shadow()
# 
# 
# imputed_Data <- mice(raw_data, m=5,meth='pmm', seed = 5)
# summary(imputed_Data)
# 
# data_wide<-mice::complete(imputed_Data,1)
# colnames(data_wide) <- colnames(d)
#these are the data we will work with

#----------------------------------------------------------------------------------------------------------
# CREATE STACKED BAR CHARTS
#----------------------------------------------------------------------------------------------------------

#response frequencies
df_likert <- data_wide %>%
  response.frequencies()


# data preparation
data_counts <- data %>%
  select(-subj) %>%
  group_by(names,topic) %>%
  count(response,topic) %>%
  tidyr::complete(response) %>%
  # replace with 0 for no occurences
  mutate(n = ifelse(is.na(n),0,n),
         # take n and devide through the number of particants
         percent = n / nrow(d) * 100,
         # prepare percentage labels
         label = paste0(round(percent,2), " %")) %>%
  ungroup()


# select number of rows
likert_plots <-  data_counts %>%
                 group_by(topic) %>%
                  do(plots = ggplot(data = .)+
                       aes(x = names, y = n, fill = response)+
                       #geom_bar(position = "fill")+
                       geom_bar(stat = "identity")+
                       #reverse color scale to make blues good and reds bad in polarity
                       scale_fill_brewer(palette = "RdBu", direction = -1)+
                       geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2)+
                       coord_flip()+
                       theme_classic(base_size = 12)+
                       labs(fill = "Aussage", title = "Prozentuale Verteilung der Antworten")+
                       theme(axis.title.y= element_blank(), 
                             axis.title.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.text.x = element_blank(),
                             panel.background = element_rect(fill = "white", color = "black"),
                             plot.title = element_text(color = "black", size = 18, face = "bold")))


# export visualizations to path_plot
for (i in 1:nrow(likert_plots)) {
  ggsave(likert_plots$plots[[i]], file=paste0(current_date,"_","Distribution_", likert_plots$topic[[i]],".png"), path=paste0(path_plot, "/Kommunikation/", likert_plots$topic[[i]]),width = 15, height = 10, units = "cm")
}

#----------------------------------------------------------------------------------------------------------
# PLOT LIKERT DATA
#----------------------------------------------------------------------------------------------------------

#select only relevant columns and change to factor
data_l <- as.data.frame(data_wide) %>%
  select(-c(subj)) %>%
  mutate_if(is.numeric,as.factor)

# correct for unequal number of factor levels
data_l[] <- lapply(data_l, function(x){
  levels(x) <- c("nie","selten","manchmal","teilweise", "oft", "immer")
  x
})

# subset data by topic
Generell <- data_l[,c(1,2,3,4)]
Einflussnahme <- data_l[,c(5,6,7,8)]
Fuehrung <- data_l[,c(9,10,11)]
Innovation <- data_l[,c(12,13,14,15,16,17)]

col_names <- c("Generell","Einflussnahme","Fuehrung","Innovation")

# create and export plots per topic
for (i in 1:length(col_names)) {
  # get data for each topic
  x <- get(col_names[i])
  # plot likert data
  current <- plot(likert(x),low.color = "#FF6665", high.color = "#5AB4AC", neutral.color.ramp = "white", neutral.color = "grey90")+
    ggtitle(paste0("Antworten zum Thema ",col_names[i]))+
    theme_classic(base_size = 13)
  
  # save graph in prespecified folder
  ggsave(current, file=paste0(current_date,"_","Likert_", col_names[i],".png"), path=paste0(path_plot),width = 25, height = 10, units = "cm")
  
}


#---------------------------------------------------------------------------------------------
#  ITEMANALYSIS - SChWIERIGKEIT
#---------------------------------------------------------------------------------------------


library(psychometric)

#check all psychometric properties but exclude subj-id
item.exam(data_wide[,-35], y = NULL, discrim = FALSE)

#Sample.SD = Standard deviation of the item
#Item.total = Correlation of the item with the total test score
#Item.Tot.woi = Correlation of item with total test score (scored without item)
#Difficulty = Mean of the item (p)
#Discrimination = Discrimination of the item (u-l)/n
#Item.Criterion = Correlation of the item with the Criterion (y)
#Item.Reliab = Item reliability index -> Cronbachs alpha
#Item.Rel.woi = Item reliability index (scored without item)
#Item.Validity = Item validity index

