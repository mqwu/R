
 # R code style 
 # -----------------------------------------------------------
 # Google R style
 https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
 
 # Wickham (RStudio)
 http://adv-r.had.co.nz/Style.html#undefined
 
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
