#----------------------------------------------------------
# Cleaning data in R (DataCamp course)
#----------------------------------------------------------
## Getting feel of your data
class()
dim()
names()
str()
glimpse()
summary()

head()
tail()
hist()
plot()

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
       
## type conversion
class() # check type
class(99L)  # integer end with L
       
as.numeric()
as.integer()
as.character()
as.factor()
as.logical()

weather6 <- mutate_each(weather5, funs(as.numeric), CloudCover:WindDirDegrees)
       
# time
library(lubridate)
ymd_hms()
dmy("17 Sep 2015")   
mdy_hm("July 15, 2012 12:56")
students2$nurse_visit <- ymd_hms(students2$nurse_visit)
       
# string
library(stringr)
str_trim("   aaa b c d   ")  # trim trailing and leading white space
str_pad("24493", width=7, side="left", pad="0")
str_detect(friends, "Alice")
str_replace(friends, "Alice", "Derek")
       
tolower("XXX")  # base R
toupper("xxx")
 
## missing data
# check
any(is.na(df))       
is.na(df)
sum(is.na(df))  # count
summary(df)
       
# find rows with no missing values
complete.cases(df)
df[complete.cases(df), ]  # subset df with complete cases

na.omit(df)  # rm rows with missing value na

# Find indices of NAs in Max.Gust.SpeedMPH
ind <- which(is.na(weather6$Max.Gust.SpeedMPH))
# Look at the full rows for records missing Max.Gust.SpeedMPH
weather6[ind, ]
       
# Find row with Max.Humidity of 1000
ind <- which(weather6$Max.Humidity==1000)
# Look at the data for that day
weather6[ind, ]
# Change 1000 to 100
weather6$Max.Humidity[ind] <- 100        
       
## outlier detection
boxplot(x, horizontal=True)
boxplot(df)
hist(x)
summary(df)
       
