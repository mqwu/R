rm(list=ls(all=TRUE))  # clean memory

#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:/project/DataMiningUNC/permian/trunk/Src/Adhoc")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
source("tools.R")

# load the required packages and try to install them if they are not available
reqPackages <- c("tidyverse", "mlr", "corrplot", "lubridate", "stringr")
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
#d = read.csv(data_user,as.is=TRUE, sep='\t')

# correct names
colnames(d) <- gsub("\\.+", ".", colnames(d))
colnames(d) <- gsub("\\.$", "", colnames(d))

# correct var type
d$Well.Name <- as.character(d$Well.Name)
gas.d$Observation.Date <- as.Date(gas.d$Observation.Date, "%m/%d/%Y")
gas.d$Observation.Date <- mdy(gas.d$Observation.Date, "%m/%d/%Y")  # use lubridate package

# parse time data
d <- d %>% mutate(date=as.Date(parse_date_time(time, orders="mdy HM")))
# parse date time (lubridate)
d$createtime = parse_date_time(d$createtime, orders="y-m-d H:M:S")

# order columns alphabetically in R
d[,order(colnames(d))]

# move some variable to front
gas.d <- gas.d %>% select(Observation.Date, everything())

# select vars and filter obs
d <- d %>% 
        select(varA:varB) %>%
        select(iris,contains("Sepal")) %>%
        filter(varC=="Y")

# left join
left_join(A, B, by = c("first_name" = "second_name"))

# filter string contains
mtcars %>% 
  filter(str_detect(colname, "^L"))

## Tidy data
# gather: gather cols into key-val pair
# wide -> long (tidy)       
gather(data, key, val, name of cols to gather or not)  
gather(wide_df, key, value, -col)
weather2 <- gather(weather, day, value, X1:X31, na.rm = TRUE)       
# spread: spread key-val pair into cols
# long -> wide
spread(data, key, val)
spread(long_df, key, val)

# separate: separate one col into multiple cols
separate(data, col, into, sep=)
separate(data, year_month, c("year", "month"), sep="_")
# unite: united multiple cols into one
unite(data, col, ...)
unite(data, year_month, year, month, sep="_")

# library(stringr)
str_detect(friends, "Alice")
str_replace(friends, "Alice", "Derek")
str_trim("   aaa b c d   ")  # trim trailing and leading white space


#--------------------------------------------------------
# EDA
#-------------------------------------------------------- 
# summary stats
dim(dat)
summary(dat)
sum(dat$v < 0) 
sum(dat$v == 0)
sum(duplicated(dat))

# missing data
sum(is.na(df))  # count
# find rows with no missing values
complete.cases(df)
df[complete.cases(df), ]  # subset df with complete cases

# group by then aggregate
flights %>%
	group_by(Dest) %>%  # group_by does not create a copy of data and change the order of the data
	summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))


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
# outlier detection
boxplot(x, horizontal=True)
boxplot(df)


#--------------------------------------------------------
# Modelling
#-------------------------------------------------------- 
