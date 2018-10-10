
#--------------------------------------------------------
# Tools: collection of useful functions
#-------------------------------------------------------- 
load_libs <- function(requiredPackages) {
  # load all required libs, if they are not installed try to install them automatically
  #
  # Args:
  #  requiredPackages: List of strings with package names that are about to be loaded 
  #                    If they are not installed automated installation is attempted
  missingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
  if (length(missingPackages)) {
    install.packages(missingPackages, dependencies = TRUE)
  }

  for (package in requiredPackages) {
    library(package, character.only = TRUE)
  }
}

