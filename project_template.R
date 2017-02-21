#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:/project/DataMiningUNC/permian/trunk/Src/Adhoc")


#--------------------------------------------------------
# Libs
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory
library(tidyverse)


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

# select vars and filter obs
d <- d %>% 
        select(varA:varB) %>%
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



#--------------------------------------------------------
# Tools (should be sourced first before the above procedure)
#-------------------------------------------------------- 
plot_HistDensity <- function(x, title){
  # plot histogram with density curve overlaid
  # Arg:
  #   x: numeric vector 
  # Return:
  #   plot of histogram with density curve overlaid
  x <- x[!is.na(x)]  # rm NA records
  hist(x, prob=TRUE, col="grey", nclass=50, main=NA) # prob=TRUE for probabilities not counts
  lines(density(x, adjust=1), col="blue", lty="solid", lwd=2) # add a density est
  title(main=title)
  
  dev.copy(png,paste0(title,"_hist.png"))
  dev.off()
}


plot_Box <- function(d, x="1", y, title=""){
  # plot boxplot
  # Arg:        
  #   d: data frame
  #   x: catergorical var 
  #   y: numerical var
  # return: 
  #   box plot of y catergoried by x
  p <- d %>%  
    filter_(!is.na(y)) %>%  # rm NA records
    ggplot(., aes_string(x=x, y=y)) +
    geom_boxplot(aes_string(fill=x)) + 
    ggtitle(title)
  
  if(x=="1"){ # categorical var with 1 level
    p + theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              plot.title = element_text(hjust = 0.5),
              legend.position="none")
  } else { # categorical var with multiple level
    p + theme(plot.title = element_text(hjust = 0.5))
  }
  ggsave(paste0(title,"_box.png"))
}


plot_StackingProp <- function(d, x="1", y, title=""){
  # stacking proportion plot 
  # Arg:        
  #   d: data frame
  #   x: catergorical var 
  #   y: catergorical var for count (proportion)
  # return: 
  #   stacking proportion plot of y for different x catergory
  ggplot(data = d) + 
    geom_bar(mapping = aes_string(x = x, fill = y), position = "fill") +
    guides(fill=guide_legend(title=title))
  ggsave(paste0(title,"_stackingProp.png"))
}


plot_BarCount <- function(d, x="1", y, title=""){
  # Side by side Bar count plot category on x and y
  # Arg:        
  #   d: data frame
  #   x: main catergorical var 
  #   y: catergorical var within x 
  # return: 
  #   Side by side Bar count plot category on x and y
  ggplot(data = d) + 
    geom_bar(mapping = aes_string(x = x, fill = y), position = "dodge")
  ggsave(paste0(title,"_barcount.png"))
}
