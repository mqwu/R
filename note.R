
 # R code style 
 # -----------------------------------------------------------
 # Google R style
 https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
 
 # Wickham (RStudio)
 http://adv-r.had.co.nz/Style.html#undefined

 # efficient R
 https://csgillespie.github.io/efficientR/performance.html
 
 # Save/load workspace; Save object
 #-----------------------------------------------------------
 save.image(file="./xx/a.RData")
 load("./xx/a.RData")
 
 saveRDS(obj, "obj.rds")
 obj.new <- readRDS("obj.rds") # can assign to a new obj name
 
 # R Startup Setting 
 # ----------------------------------------------------------
 # Change default library path
 # 1. Create an “Renviron.site” file under ..R/etc/
 # 2. set R_LIBS as the desired lib path, 
 #    e.g. R_LIBS=C:/Apps/software/R-3.0.2/library


 # RStudio Setting
 # ---------------------------------------------------------
 # Change Repositories (Packages source)
 # 1. chooseCRANmirror() 
 # 2. pick one...
 # note: 
 # When you install the package using the RStudio package installer or directly from CRAN, 
 # it doesn't install the dependencies and hence, R keeps throwing the load namespace error.
 # Solution: install.packages("forecast", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
  
 
 # Load data & clean name
 d <- read.csv("./data/Gas_Engine.csv", header=T, as.is=T)
 colnames(d) <- gsub("\\.+", ".", colnames(d))
 colnames(d) <- gsub("\\.$", "", colnames(d))


 # Matrix Operation
 # ---------------------------------------------------------
 # Delete a row in a matrix
 a = matrix(1:100, nc=5)
 b = a[-5,]
 
 # Delete row 5 and 7
 ind=c(5,7)
 d = a[-ind, ]

 
 # Shiny Package
 # -----------------------------------------------------------------------------------
 # Deployment tips
 # Deploy two application in one folder
 
 # 1. Put source code (ui.R, server.R) in 
 #    ./appfolder/src/app1
 #    ./appfolder/src/app2
 
 # 2. Create two run files (run1.R, run2.R) in  
 #    ./appfolder/src/
 #    with code below
  .libPaths(unique(c("../lib",.libPaths())))
  library(shiny)
 
  if (file.exists ("C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")) {
    options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  } else {
    options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")
  }

  runApp('C:/Apps/SampleSize/src/app1', launch.browser=TRUE)  # run1.R
  runApp('C:/Apps/SampleSize/src/app2', launch.browser=TRUE)  # run2.R
 
 # 3. Creat R shell short cut under ./appfolder/ and modify its properties (e.g. app1)
 # Target: "C:\Program Files (x86)\R\R-3.0.2\bin\i386\Rterm.exe" --file="run1.R"
 # Start in: .\appfolder\src\
 
 # Running a Shiny app using a shortcut
 http://rstudio-pubs-static.s3.amazonaws.com/3269_a6682dfda37e411fb5e0e6699495cdc4.html
 
 
 # Shade tails of a distribution plot
 # --------------------------------------------------------------------------------------
 x <- seq(-7, 7, length=2000)
 hx <- dnorm(x, mean=0, sd=1.7)
 
 q1 <- -3
 x.min <- -7
 q2 <-  3
 x.max <- 7
 
 x1 <- min(which(x > x.min))
 x2 <- max(which(x <= q1))
 
 x3 <- min(which(x >= q2))
 x4 <- max(which(x < x.max))
 
 plot(x, hx, type="l", lty=1, lwd=2,
      xlab="",
      ylab="",
      xaxt="n",
      yaxt="n",
      axes=F,
      xlim=c(-7,7), ylim=c(0,0.4))
 abline(h=0)
 
 polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, hx[x1:x2], 0), col="gray")  # shade left tail
 polygon(x=c(x[c(x3,x3:x4,x4)]), y= c(0, hx[x3:x4], 0), col="gray")  # shade right tail
 
 text(0, 0.1, expression(1-alpha), cex=1.6)
 text(-3.5, 0.01, expression(alpha/2), cex=1.6)
 text(3.5, 0.01, expression(alpha/2), cex=1.6)
 
 axis(side = 1, at = 0, labels = 0, pos=0.004)  # customize axis
 axis(side = 1, at = 3, labels = expression(Z[alpha/2]), pos=0.004)  
 axis(side = 1, at = -3, labels = expression(-Z[alpha/2]), pos=0.004)
 
 
 # SparkR
 # --------------------------------------------------------------------------------------
 http://amplab-extras.github.io/SparkR-pkg/
 
 # Correlation plot
 # --------------------------------------------------------------------------------------
http://www.sthda.com/french/wiki/matrice-de-correlation-avec-r-analyse-et-visualisation?title#at_pco=smlwn-1.0&at_si=58d3dbe36b6b1baa&at_ab=per-2&at_pos=0&at_tot=1
 http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
 

# Data Structure
# 1D: atomic vector and list
# 2D: matrix
# 3D+: array
# Data frame:  a list of equal-length vectors
#----------------------------------------------------------------------------------------
# 1D : atomic vector and list
# You construct lists by using list() instead of c()
x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))

# factor
# Factors are useful when you know the possible values a variable may take, 
# Factors are built on top of integer vectors using two attributes: class(),levels()
x <- factor(c("a", "b", "b", "a"))
sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))
table(sex_factor)
#> sex_factor
#> m f 
#> 3 0
a <- read.csv(file, stringsAsFactors = FALSE )

# add attribute to a vector
structure(1:5, aaa="c")

# matrix(2D) and array (nD)
a <- matrix(1:6, ncol = 3, nrow = 2)
b <- array(1:12, c(2, 3, 2))
b
#, , 1
#     [,1] [,2] [,3]
#[1,]    1    3    5
#[2,]    2    4    6
#, , 2
#     [,1] [,2] [,3]
#[1,]    7    9   11
#[2,]    8   10   12
dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A", "B"))

#the dimension attribute can also be set on lists to make list-matrices or list-arrays:
l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2, 2)

# data frame
df <- data.frame(x = 1:3, y = c("a", "b", "c"))

df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"), # default will turn string into factor
  stringsAsFactors = FALSE)

# You can combine data frames using cbind() and rbind():
cbind(df, data.frame(z = 3:1))
rbind(df, data.frame(x = 10, y = "z")) # col name must match


# Subsetting
#---------------------------------------------------------------
# Subsetting a list works in the same way as subsetting an atomic vector. 
# Using [ will always return a list; [[ and $, as described below, let you pull out the components of the list.
# $ = [[
# x$y is the same as x[["y", exact=F]]
var <- "cyl"
# Doesn't work - mtcars$var translated to mtcars[["var"]]
mtcars$var
#> NULL

# Instead use [[
mtcars[[var]]

#S3 objects are made up of atomic vectors, arrays, and lists
#S4 objects: @ (equivalent to $), and slot() (equivalent to [[). @ is more restrictive than $ in that it will return an error if the slot does not exist.
#If list x is a train carrying objects, then x[[5]] is the object in car 5; x[4:6] is a train of cars 4-6.

# Data frame: if output is a single column, returns a vector instead of a data frame.
str(df[, "a", drop = FALSE])
#> 'data.frame':    2 obs. of  1 variable:
#>  $ a: int  1 2
str(df[, "a"])
#>  int [1:2] 1 2

#lookup table
grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)

# Using match
id <- match(grades, info$grade)
info[id, ]

#samples/bootstrap
# Randomly reorder
df[sample(nrow(df)), ]
# Select 3 random rows
df[sample(nrow(df), 3), ]
# Select 6 bootstrap replicates
df[sample(nrow(df), 6, rep = T), ]

#order() takes a vector as input and returns an integer vector describing how the subsetted vector should be ordered
#sort return the same type of as input
x <- c("b", "c", "a")
order(x)
#> [1] 3 1 2
x[order(x)]
#> [1] "a" "b" "c"

#If you know the columns you don’t want, use set operations to work out which colums to keep:
df[setdiff(names(df), "z")]

#Remember to use the vector boolean operators & and |, not the short-circuiting scalar operators && and || which are more useful inside if statements.
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]
subset(mtcars, gear == 5 & cyl == 4)
#which() allows you to convert a boolean representation to an integer representation
#Give the TRUE indices of a logical object
#which(x, arr.ind = FALSE, useNames = TRUE)

# <- 
# <<-
http://stackoverflow.com/questions/10904124/global-and-local-variables-in-r

# apply function
http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega

# apply
#When you want to apply a function to the rows or columns of a matrix (and higher-dimensional analogues); 
#not generally advisable for data frames as it will coerce to a matrix first.
# Two dimensional matrix
M <- matrix(seq(1,16), 4, 4)

# apply min to rows
apply(M, 1, min)
[1] 1 2 3 4

# apply max to columns
apply(M, 2, max)
[1]  4  8 12 16

# 3 dimensional array
M <- array( seq(32), dim = c(4,4,2))

# Apply sum across each M[*, , ] - i.e Sum across 2nd and 3rd dimension
apply(M, 1, sum)
# Result is one-dimensional
[1] 120 128 136 144

# Apply sum across each M[*, *, ] - i.e Sum across 3rd dimension
apply(M, c(1,2), sum)
# Result is two-dimensional
     [,1] [,2] [,3] [,4]
[1,]   18   26   34   42
[2,]   20   28   36   44
[3,]   22   30   38   46
[4,]   24   32   40   48

# lapply - When you want to apply a function to each element of a list in turn and get a list back.
 x <- list(a = 1, b = 1:3, c = 10:100) 
 lapply(x, FUN = length) 
 $a 
 [1] 1
 $b 
 [1] 3
 $c 
 [1] 91

lapply(x, FUN = sum) 
$a 
[1] 1
$b 
[1] 6
$c 
[1] 5005

# sapply - When you want to apply a function to each element of a list in turn, but you want a vector back, rather than a list.
x <- list(a = 1, b = 1:3, c = 10:100)
#Compare with above; a named vector, not a list 
sapply(x, FUN = length)  
a  b  c   
1  3 91

sapply(x, FUN = sum)   
a    b    c    
1    6 5005 

sapply(1:5,function(x) rnorm(3,x))
           [,1]     [,2]     [,3]     [,4]     [,5]
[1,] 1.6622346 2.463587 3.303416 4.248478 5.781673
[2,] 1.7763855 3.241603 1.370866 5.912452 5.506866
[3,] 0.4434785 2.941029 3.045867 3.054171 3.502678
 
sapply(1:5,function(x) matrix(x,2,2))
     [,1] [,2] [,3] [,4] [,5]
[1,]    1    2    3    4    5
[2,]    1    2    3    4    5
[3,]    1    2    3    4    5
[4,]    1    2    3    4    5
 
sapply(1:5,function(x) matrix(x,2,2), simplify = "array")

# vapply - When you want to use sapply but perhaps need to squeeze some more speed out of your code.
# For vapply, you basically give R an example of what sort of thing your function will return, 
x <- list(a = 1, b = 1:3, c = 10:100)
#Note that since the advantage here is mainly speed, this
# example is only for illustration. We're telling R that
# everything returned by length() should be an integer of 
# length 1. 
vapply(x, FUN = length, FUN.VALUE = 0L) 
a  b  c  
1  3 91

 # mapply - For when you have several data structures (e.g. vectors, lists) and you want to apply 
 # a function to the 1st elements of each, and then the 2nd elements of each, etc., coercing the 
 # result to a vector/array as in sapply.
 #Sums the 1st elements, the 2nd elements, etc. 
mapply(sum, 1:5, 1:5, 1:5) 
[1]  3  6  9 12 15
#To do rep(1,4), rep(2,3), etc.
mapply(rep, 1:4, 4:1)   
[[1]]
[1] 1 1 1 1

[[2]]
[1] 2 2 2

[[3]]
[1] 3 3

[[4]]
[1] 4

lapply is a list apply which acts on a list or vector and returns a list.
sapply is a simple lapply (function defaults to returning a vector or matrix when possible)
vapply is a verified apply (allows the return object type to be prespecified)
rapply is a recursive apply for nested lists, i.e. lists within lists
tapply is a tagged apply where the tags identify the subsets
apply is generic: applies a function to a matrix's rows or columns (or, more generally, to dimensions of an array)
 
 
 
 
 
 
 

# seq
#---------------------------------------------------------------
seq(1, 10, length.out=5)
[1]  1.00  3.25  5.50  7.75 10.00
> seq(1, 10, by=5)
[1] 1 6
a <- rnorm(10)
seq_along(a)
# [1]  1  2  3  4  5  6  7  8  9 10
seq_len(10)
# [1]  1  2  3  4  5  6  7  8  9 10
# with () print to the screen automatically
(y <- seq(1, 10, length.out=3))


# tidyverse
#ggplot2, for data visualisation.
#dplyr, for data manipulation.
#tidyr, for data tidying.
#readr, for data import.
#purrr, for functional programming.
#tibble, for tibbles, a modern re-imagining of data frames.
#---------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)
#plot
ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy, color=size)) # size=class alpha=class shape=class

ggplot(data = mpg) + 
  geom_point(aes(x = displ, y = hwy), color="blue") # size=class alpha=class shape=class

# facet plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
# by 2 vars
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#geom
# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# multiple geom
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# global mapping & local mapping
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

# global data & local data
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

# bar stat=count as y
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

# bar  y=var in data
demo <- tribble(
  ~a,      ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = a, y = b), stat = "identity")

# proportion as y
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

# summary
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# data transformation
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))

# filter
#filter() only includes rows where the condition is TRUE; it excludes both FALSE and NA values. If you want to preserve missing values, ask for them explicitly:
 filter(df, is.na(x) | x > 1)

# arrange
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

# select
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

#There are a number of helper functions you can use within select():
#starts_with("abc"): matches names that begin with “abc”.
#ends_with("xyz"): matches names that end with “xyz”.
#contains("ijk"): matches names that contain “ijk”.

# move timer_hour, air_time to the start of the data
select(flights, time_hour, air_time, everything())

# rename
rename(flights, tail_num = tailnum)

# mutate
mutate(flights_sml,
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

# transmute: only want to keep the new variables
transmute(flights,
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

# %/% integer division %% remainder
transmute(flights,
  dep_time,
  hour = dep_time %/% 100,
  minute = dep_time %% 100
)
#> # A tibble: 336,776 × 3
#>   dep_time  hour minute
#>      <int> <dbl>  <dbl>
#> 1      517     5     17
#> 2      533     5     33
#> 3      542     5     42
#> 4      544     5     44

# summarise
# The last key verb is summarise(). It collapses a data frame to a single row
# summarise() is not terribly useful unless we pair it with group_by()
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
  count = n(),
  dist = mean(distance, na.rm = TRUE),
  delay = mean(arr_delay, na.rm = TRUE)
)

# pipe
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# missing value
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

# count
# a count (n()), or a count of non-missing values (sum(!is.na(x)))
# n_distinct(carrier)
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
    geom_point(alpha = 1/10)

# Counts and proportions of logical values: sum(x > 10), mean(y == 0). 
# When used with numeric functions, TRUE is converted to 1 and FALSE to 0. 
# This makes sum() and mean() very useful: sum(x) gives the number of TRUEs in x, and mean(x) gives the proportion.

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

# When you group by multiple variables, each summary peels off one level of the grouping. 
# That makes it easy to progressively roll up a dataset:
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

# ungroup()
# If you need to remove grouping, and return to operations on ungrouped data, use ungroup().
daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights

# grouped mutate and filter
#Grouping is also do convenient operations with mutate() and filter():

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)


# tibble:  variant of the data frame 
# augmented lists: they have class “tbl_df” + “tbl” + “data.frame”
as_tibble() # coerce a data frame to be tibble format
tb <- tibble(x = 1:5, y = 5:1)
typeof(tb)
#> [1] "list"
attributes(tb)

#tribble(), short for transposed tibble. 
a <- tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

# extract by name
a$x
a[["x"]]
#by position
a[[1]]


# set string as var name
# assign + eval(as.name())

for (i in 1:20){
  path <- paste0("d", i,"_path")
  d.frame <- paste0("d", i)
  assign(d.frame, read.csv(eval(as.name(path)), header=F))
  
}

# change multiple data.frame col names using setNames
header <- read_excel(header_path)
for (i in 1:20){
  path <- paste0("d", i,"_path")
  d.frame <- paste0("d", i)
  assign(d.frame, setNames(read.csv(eval(as.name(path)), header=F), names(header)) )
}


# select factor variable from a data frame and then count frequency
is.fact <- sapply(dat, is.factor)
lapply(dat[,is.fact], table)

# order columns alphabetically in R
df[,order(colnames(df))]
df %>% select(noquote(order(colnames(df))))

# using proxy to install package from github
library(httr)
with_config(use_proxy(url="proxy-eu.shell.com", port=8080), install_github("kassambara/easyGgplot2"))


# multiple lines by group
library(reshape)
library(ggplot2)
library(plotly)

x <- seq(0, 4 * pi, 0.1)
n <- length(x)
y1 <- 0.5 * runif(n) 
y2 <- 2 * runif(n) 
y3 <- 0.2 * runif(n) + cos(x) - sin(x)
y4 <- rnorm(n)
df <- data.frame(x, y1, y2, y3, y4)

#----------------------------------------------------
df.melted <- melt(df, id = "x")
df.melted[is.na(df.melted$value),3] <- 0  # fill NA value

#----------------------------------------------------
wear <- c("y1", "y3") # separate lines into groups
df.melted$col <- as.factor(ifelse(df.melted$variable%in%wear, 1, 0))
#----------------------------------------------------
p <- ggplot(data = df.melted, aes(x = x, y = value, group=variable, color=col)) +
      geom_line() #+ geom_point()
ggplotly(p)


#-------------------------------------------------------
# Debug
#-------------------------------------------------------
# Shiny debug
# print object in server.R
cat(file=stderr(), timesheet[1,1], str(a), glimpse(a),  "\n")


#----------------------------------------------------------
# Writing Functions in R (DataCamp course)
#----------------------------------------------------------
# seq_along: loop each col

for (i in seq_along(df)) {
  print(median(df[[i]]))
}

## allocate space for faster performance
# Create new double vector: output
output <- vector("double", ncol(df))

# Alter the loop
for (i in seq_along(df)) {
  # Change code to store result in output
  output[i] <- median(df[[i]])
}

# Define example vector x
x <- 1:10 


# Use the function template to create the rescale01 function
rescale01 <- function(x) {
  # body
  rng <- range(x, na.rm = TRUE) 
  (x - rng[1]) / (rng[2] - rng[1])
}

# Test your function, call rescale01 using the vector x as the argument
rescale01(x)


## use message() to highlight output info
replace_missings <- function(x, replacement) {
  is_miss <- is.na(x)
  x[is_miss] <- replacement
  
  # Rewrite to use message()
  #cat(sum(is_miss), replacement, "\n")
  message(sum(is_miss), " missings replaced by the value ", replacement)
  x
}

# Check your new function by running on df$z
replace_missings(df$z, replacement = 0)

## functional programming
# give equal weight to verbs and nouns
# abstract away the details of implementation
# functions + vectors

# Add a second argument called power
f <- function(x, power) {
    # Edit the body to return absolute deviations raised to power
    abs(x - mean(x))^power
}

# function can be argument as well
col_summary <- function(df, fun) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- fun(df[[i]])
  }
  output
}

# library(purrr)
# Find the 5th percentile of each column, excluding missing values
map_dbl(planes, quantile, probs=0.05, na.rm=T)

# Find the columns that are numeric
map_lgl(df3, is.numeric)

# Find the type of each column
map_chr(df3, typeof)

# Find a summary of each column
map(df3, summary)

## shortcut to write a function on fly
# Rewrite to call an anonymous function
map(cyl, ~lm(mpg ~ wt, data=.))

# Save the result from the previous exercise to the variable models
models <- map(cyl, ~ lm(mpg ~ wt, data = .))
# Use map and coef to get the coefficients for each model: coefs
coefs <- map(models, coef)
# Use string shortcut to extract the wt coefficient 
map_dbl(coefs, "wt")

coefs <- map(models, coef)
# use map_dbl with the numeric shortcut to pull out the second element
map_dbl(coefs, 2)

##pipe
# Define models (don't change)
models <- mtcars %>% 
  split(mtcars$cyl) %>%
  map(~ lm(mpg ~ wt, data = .))

# Rewrite to be a single command using pipes, last chain results become the first argument of next function
models %>% map(summary) %>% map_dbl("r.squared")


