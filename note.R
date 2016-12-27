
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



