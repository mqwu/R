
 ### R Startup Setting
 
 ## Change default library path
  # 1. Create an “Renviron.site” file under ..R/etc/
  # 2. set R_LIBS as the desired lib path, 
  #    e.g. R_LIBS=C:/Apps/software/R-3.0.2/library


 ### RStudio Setting
 
 ## Change Repositories (Packages source)
  # 1. chooseCRANmirror() 
  # 2. pick one...
  
 ### Matrix Operation
  
 ## Delete a row in a matrix
 a = matrix(1:100, nc=5)
 b = a[-5,]
 
 # delete row 5 and 7
 ind=c(5,7)
 d = a[-ind, ]

 
 ### Shiny Package
 
 ## Deployment tips
 
 # Deploy two application in one folder
 
 # 1. Put source code (ui.R, server.R) in 
 #    ./appfolder/src/app1
 #    ./appfolder/src/app2
 
 # 2. Create two run files (run1.R, run2.R) in  
 #    ./appfolder/src/
 #    with code below
  .libPaths(unique(c("./lib",.libPaths())))
  library(shiny)
 
  if (file.exists ("C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")) {
    options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  } else {
    options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")
  }

  runApp('C:/Apps/projects/TED/SampleSize/src/app1', launch.browser=TRUE) # run1.R
 #runApp('C:/Apps/projects/TED/SampleSize/src/app2', launch.browser=TRUE) # run2.R
 
 # 3. Creat R shell short cut under ./appfolder/ and modify its properties (e.g. app1)
 # Target: "C:\Program Files (x86)\R\R-3.0.2\bin\i386\Rterm.exe" --file="run1.R"
 # Start in: .\appfolder\src\
 
 # http://rstudio-pubs-static.s3.amazonaws.com/3269_a6682dfda37e411fb5e0e6699495cdc4.html
 
 
 
 