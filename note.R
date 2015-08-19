
 # R code style 
 # -----------------------------------------------------------
 # Google R style
 https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
 
 # Wickham (RStudio)
 http://adv-r.had.co.nz/Style.html#undefined
 
 
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
 
