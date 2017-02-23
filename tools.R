
#--------------------------------------------------------
# Tools: collection of useful functions
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


plotRFVarImp <- function(rf.mod){
  # Plot variable importance of a RF model 
  #
  # Args:
  #   rf.mod: a rf model obj
  #
  # Returns:
  #   Two Plots: 1. based on pred accuracy 2. based on gini index 
  
  # Importance data
  dat <- data.frame(rownames(importance(rf.mod)), round(importance(rf.mod),2))
  
  names(dat)[c(1, ncol(dat)-1, ncol(dat))] <- c("Predictor","mda","mdg")
  rownames(dat) <- NULL
  
  pred.acc  <- select(dat, Predictor, mda)  # mean decrease in accuracy
  pred.gini <- select(dat, Predictor, mdg)  # mean decrease in gini
  
  # Var importance plot function
  importancePlot <- function(d,ylb,fontsize){
    fontsize <- as.numeric(fontsize)
    d <- d[order(d[,2],decreasing=T),]
    d$Predictor <- factor(as.character(d$Predictor),levels=rev(as.character(d$Predictor)))
    rownames(d) <- NULL
    
    d[,2] <- d[,2]/abs(max(d[,2])) * 100  # normalize relative to the variable with maximum score
    abs.min <- abs(min(d[,2]))
    
    g1 <- ggplot(data=d,aes_string(x="Predictor",y=ylb,group="Predictor")) + 
      geom_bar(stat="identity", colour="#d62d20", fill="#d62d20") + theme_grey(base_size=fontsize)
    #geom_bar(stat="identity", colour="#639f89", fill="#639f89") + theme_grey(base_size=fontsize)
    
    #if(ylb=="mda")      g1 <- g1 + labs(y="Mean decrease in accuracy") 
    #else if(ylb=="mdg") g1 <- g1 + labs(y="Mean decrease in Gini")
    g1 <- g1 + labs(y="Variable Importance") # Simplify for presentation purpose
    
    
    g1 <- g1 + theme(axis.title=element_text(size=25,face="bold"), 
                     axis.text.x=element_text(angle=0,hjust=1,vjust=0.4,colour='black'),
                     axis.text.y= element_text(colour='black', size=25)) + 
      geom_hline(yintercept=abs.min,linetype="dashed",colour="black") + coord_flip()
    print(g1)
  }
  
  importancePlot(d=pred.acc, ylb="mda", 20)
  importancePlot(d=pred.gini, ylb="mdg", 20)
}
