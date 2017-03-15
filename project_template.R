#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:/project/DataMiningUNC/permian/trunk/Src/Adhoc")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory
library(tidyverse)

source("tools.R")

#--------------------------------------------------------
# Paths
#-------------------------------------------------------- 
d_path = "Z:/project/Adhoc/data/d.csv"
b_path = "Z:/project/Adhoc/data/b.csv"

#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
d <- read.csv(b_path, header=TRUE, na.strings="N/A")

# correct names
colnames(d) <- gsub("\\.+", ".", colnames(d))
colnames(d) <- gsub("\\.$", "", colnames(d))

# order columns alphabetically in R
d[,order(colnames(d))]

# select vars and filter obs
d <- d %>% 
        select(varA:varB) %>%
        select(iris,contains("Sepal")) %>%
        filter(varC=="Y")

      

#--------------------------------------------------------
# EDA
#-------------------------------------------------------- 
# summary stats
summary(dat)

# missing values
sapply(dat, function(x) sum(is.na(x)))  # count
sapply(dat, function(x) sum(is.na(x))/nrow(dat))  # proportion
        
## Numerical vars
# hist
plot_HistDensity(dat$v1, "xxx")

# boxplot
plot_Box(dat, x="v1", y="v2", title="vvv")


#--------------------------------------------------------
# Modelling
#-------------------------------------------------------- 
