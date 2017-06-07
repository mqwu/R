#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:/project/DataMiningUNC/permian/trunk/Src/Adhoc")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory

source("tools.R")

# load the required packages and try to install them if they are not available
reqPackages <- c("tidyverse", "mlr", "corrplot")
load_libs(reqPackages)


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

# correct var type
d$Well.Name <- as.character(d$Well.Name)
gas.d$Observation.Date <- as.Date(gas.d$Observation.Date, "%m/%d/%Y")
gas.d$Observation.Date <- mdy(gas.d$Observation.Date, "%m/%d/%Y")  # use lubridate package

# parse time data
d <- d %>% mutate(date=as.Date(parse_date_time(time, orders="mdy HM")))

# order columns alphabetically in R
d[,order(colnames(d))]

# move some variable to front
gas.d <- gas.d %>% select(Observation.Date, everything())

# select vars and filter obs
d <- d %>% 
        select(varA:varB) %>%
        select(iris,contains("Sepal")) %>%
        filter(varC=="Y")

      

#--------------------------------------------------------
# EDA
#-------------------------------------------------------- 
# summary stats
dim(dat)
summary(dat)
sum(dat$v < 0) 
sum(dat$v == 0)
sum(duplicated(dat))


# corrlation
corr.M <- cor(x, use="pairwise.complete.obs")
corrplot(corr.M, type="upper")
corrplot(corr.M, method="number", type="upper")

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
