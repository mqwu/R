######################################################################################################################
# A collection of plot functions 
#
# Written and tested under R version 3.2.0 and ggplot2_1.0.1
#
# Author: Mingqi Wu (Mingqi.Wu@shell.com)
######################################################################################################################

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

#---------------------------------------------------------------------------------------------------------------------
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

#---------------------------------------------------------------------------------------------------------------------
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

#---------------------------------------------------------------------------------------------------------------------
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

#---------------------------------------------------------------------------------------------------------------------
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

#---------------------------------------------------------------------------------------------------------------------

plotWellProd <- function(dat){
  ## Plot producers' location and production
  #
  # Args:
  #   dat: dataframe with cols:  Longitude, Latitude, Production
  #
  # Returns:
  #   Scatter plot of producers' production with color represent the production
  myPaletter <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  
  g <- ggplot(dat=dat, aes(x=Longitude, y=Latitude, colour=Production)) + 
       geom_point(size=3, alpha=0.7) + 
       scale_colour_gradientn(colours = myPaletter(100), limits=c(0, 60)) +
       xlim(-100.5,-96) + ylim(27.5,31.4) + 
       theme(
          legend.text = element_text(size=16),
          legend.title = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.justification=c(1,0), legend.position=c(1,0)
       ) 
  
  return(g) 
}

#---------------------------------------------------------------------------------------------------------------------

plotCoreProd <- function(dat){
  ## Plot cored Well and producers location
  #
  # Args:
  #   dat: dataframe with cols:  Longitude, Latitude, ID(=core/prod)
  #
  # Returns:
  #    Plot of cored Well and producers location
  g <- ggplot(dat=dat, aes(x=Longitude, y=Latitude, colour=ID, size=ID)) + geom_point(alpha=0.7) + 
       scale_color_manual(name="Dataset", labels = c("Cored wells","Production wells"), values=c("red", "grey30")) +
       scale_size_manual(name="Dataset", labels= c("Cored wells","Production wells"), values=c(5, 3)) + 
       xlim(-100.5,-96) + ylim(27.5,31.4) +
       theme(
          legend.text = element_text(size=16),
          legend.title = element_text(size=20),
          axis.title.x = element_text(size=28),
          axis.title.y = element_text(size=28),
          legend.justification=c(1,0), legend.position=c(0.95,0)
        ) 
  
  return(g) 
}

#---------------------------------------------------------------------------------------------------------------------

plotMLine <- function(dat, xlab="", ylab="", xlim=NULL, ylim=NULL){
  ## Plot multiple lines 
  #
  # Args:
  #   dat: a data frame, x, y, method
  #   xlab: a string
  #   ylab: a string
  #   xlim: x axis limit e.g. c(0,20)
  #   ylim: y axis limit e.g. c(0,50)
  #
  # Returns:
  #   x-y multiple lines plot, each line represent one method
  #
  # Note: 
  #   1. User need to customize colors by modifying the scale_color_manual()
  #   2. If points are needed, please uncomment geom_point(size=4)
  a <- names(dat) 
  g <- ggplot(data=dat, aes_string(x=a[1],y=a[2], colour=a[3], group=a[3])) 
  
  if(length(xlim)!=0) { g <- g + xlim(xlim[1], xlim[2]) }
  if(length(ylim)!=0) { g <- g + ylim(ylim[1], ylim[2]) }
  
  g <- g + geom_line(lwd=1.8) + #geom_point(size=4) + 
       ## customized colors by modifying the following line 
       scale_color_manual(values=c("#cd0000", "#ffcc00")) + # Shell color
       xlab(xlab) + ylab(ylab) + 
       theme(
          axis.title.x = element_text(size=24),
          axis.title.y = element_text(size=24),
          axis.text.x = element_text(angle=0, hjust=0.5, colour='black', size=15),
          axis.text.y = element_text(colour="black",size=15),
          legend.title=element_blank(),
          legend.text = element_text(size = 20),
          legend.justification=c(1,0), legend.position=c(1,0),
          legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
       )
  
  return(g)
}

#---------------------------------------------------------------------------------------------------------------------

plotPredvsAct <- function(dat, xlim=NULL, ylim=NULL, title=NULL){
  ## Plot cross-plot (Predict vs Actual)
  #
  # Args:
  #   dat: a data frame: predict, actual
  #   xlim: x axis limit e.g. c(0,20)
  #   ylim: y axis limit e.g. c(0,50)
  #   title: a string, plot title
  #
  # Returns:
  #   Cross-plot (Predict vs Actual)
  a <- names(dat) 
  g <- ggplot(data=dat, aes_string(x=a[2],y=a[1])) + geom_point(alpha=0.7, size=2.5) +
       geom_abline(intercept=0, size=1, colour='red') + coord_fixed(ratio=1) +
       xlab('Observed') + ylab('Predicted') +
       theme(
             axis.title.x = element_text(size=28),
             axis.title.y = element_text(size=28),
	     axis.text.x = element_text(colour="grey20",size=24),
	     axis.text.y = element_text(colour="grey20",size=24),
	     plot.title = element_text(lineheight=.8, face="bold", size=32, vjust=2)
        ) 
  
  if(length(xlim)!=0) { g <- g + xlim(xlim[1], xlim[2]) }
  if(length(ylim)!=0) { g <- g + ylim(ylim[1], ylim[2]) }
  if(length(title)!=0) { g <- g + ggtitle(title) }

  return(g)
}

#---------------------------------------------------------------------------------------------------------------------

plotHeatmapProd <- function(dat, train_well_loc, long.range, lat.range, time){
  # Plot heatmap of well production
  #
  # Args:
  #   dat: dataframe with 3 cols:  Longitude Latitude Production(prediction)
  #   long.range: vector with long.min and long.max
  #   lat.range: vector with lat.min and lat.max
  #
  # Returns:
  #    Heatmap of well production with county boundary as backgroud
  
  myPaletter <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  
  all_states <- map_data("county")
  b <- c('pennsylvania', 'ohio', 'west virginia', 'maryland', 'virginia', 'kentucky')
  county <- subset(all_states,(region %in% b))
  
  # Heatmap
  g <- ggplot() + geom_polygon(data=county, aes(x=long, y=lat, group=group), colour="#696969", fill="white", size=1) +
       geom_tile(data=dat, aes(x=Longitude, y=Latitude, z=Production, fill=Production), alpha = 0.8) + 
       scale_fill_gradientn(colours = myPaletter(100), limits=c(0, 1000)) + 
       annotate(geom="text", x=long.range[1]+2, y=lat.range[2]+0.5, label=time, color= "black", size= 18) +
       theme_bw() +  theme(line = element_blank(),
                           panel.border = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           legend.key.height = unit(2.8, "cm"),
                           legend.text = element_text(size=26,face = 'bold'),
                           line = element_blank(),
                           title = element_blank()
						   )
						   
  return(g) 
}

#---------------------------------------------------------------------------------------------------------------------

grid4HeatmapProd <- function(d){
  # Generate grid points for heatmap of well production within the boundary
  #
  # Args:
  #   d: dataframe use to define heatmap boundary with 2 cols: Longitude Latitude
  #
  # Returns:
  #    Grid points dataframe (Longitude, Latitude) within the boundary 

  aq.ch <- chull(d$Longitude, d$Latitude)
  aq.ch <- c(aq.ch, aq.ch[1])
  aq.border <- cbind(d$Longitude[aq.ch], d$Latitude[aq.ch])
  aq.bbox <- sbox(as.points(d$Longitude, d$Latitude))
  aq.grid <- gridpts(aq.bbox, npts=50000)  
  inside <- inout(aq.grid, aq.border, bound=TRUE)
  aq.Grid <- aq.grid[inside,]
  
  grid <- as.data.frame(aq.Grid)
  names(grid)<-c('Longitude', 'Latitude')
  
  return(grid)
}

#---------------------------------------------------------------------------------------------------------------------

idw <- function(z,distance,k,num.neighs){
	# Nearest Neibor function
	#
  # Args:
  #   z: known values
  #		distance: distrance matrix between known value locations and target locations
  #   k: dampen factors
  #   num.neighs: number of neighbors
  #
  # Returns:
  #    Nearest neibor interpolations 

  
  idw.z <- rep(0,length(distance[,1]))
  for (i in 1:length(distance[,1]))
  {
    d <- sort(distance[i,],index.return=TRUE)
    w <- 1/d$x[1:num.neighs]^k
    idw.z[i] <- sum(z[d$ix[1:num.neighs]]*w)/sum(w)
  }
  return(idw.z)
}  

#---------------------------------------------------------------------------------------------------------------------

plotXYScatter <- function(dat, xlab="", ylab="", title=NULL, xlim=NULL, ylim=NULL, 
                          reg=FALSE, regcol="blue", size=4, color="black", text=data.frame(x=NULL, y=NULL, string=NULL)){
  ## Scatter plot for bivariates with/without regression line  
  #
  # Args:
  #   dat: a data frame, x, y
  #   xlab: a string 
  #   ylab: a string
  #   xlim: x axis limit e.g. c(0,20)
  #   ylim: y axis limit e.g. c(0,50)
  #   reg: if TRUE then add regression line
  #   regcol: regression line color
  #   size: point size
  #   color: point color
  #   text: annotation text, eg. x=0, y=0, string="stat" 
  #
  # Returns:
  #   Scatter plot object for bivariates with/without regression line  
  #
  a <- names(dat) 
  g <- ggplot(data=dat, aes_string(x=a[1],y=a[2])) 
  
  # Set x,y axis limits
  if(length(xlim)!=0) { g <- g + xlim(xlim[1], xlim[2]) }
  if(length(ylim)!=0) { g <- g + ylim(ylim[1], ylim[2]) }
  
  # scatter plot
  g <- g + geom_point(size=size, color=color) + xlab(xlab) + ylab(ylab) + ggtitle(title)
  
  # add regression line
  if(reg==TRUE) { g <- g + geom_smooth(method="lm", se=FALSE, color=regcol) }
  
  g <- g + theme(
            axis.title.x = element_text(size=22),
            axis.title.y = element_text(size=22),
            axis.text.x = element_text(angle=0, hjust=0.5, colour='black', size=12),
            axis.text.y = element_text(colour="black",size=12),
            legend.title=element_blank(),
            legend.text = element_text(size = 20),
            legend.justification=c(1,0), legend.position=c(1,0),
            legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
            plot.title = element_text(size = 22, face = "bold", colour = "black")
  )
  
  if(!is.null(text$string)){
    g <- g + annotate("text", x=text$x, y=text$y, label=paste0("Corr=",text$string), size=7, fontface="bold")
  }

  return(g)
}

#---------------------------------------------------------------------------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#---------------------------------------------------------------------------------------------------------------------
# tornado plot example
dat <- data.frame(
     Age= c("a","b","c","d","e","f","g","h","i","j","k","l"),
     Sex= c("a","b","c","d","e","f","g","h","i","j","k","l"),
     value=c(.2,.3,.4,.1,.05, .04, -.24,-.28,-.29,-.12,-.06, -.03)
)

ggplot(dat, aes(x=Age, y=value, fill=Sex)) + 
     geom_bar(stat="identity", position="identity") +  # error if stat = not included.
     coord_flip()

#---------------------------------------------------------------------------------------------------------------------
# bar plot
plot_bar = function(dat, 
                    title=NULL, 
                    xlab="", xlab_angle=0, ylab="", 
                    xlim=NULL, ylim=NULL,
                    fill=NULL, dodge = FALSE,  
                    flip_coord=FALSE) 
{
  a = names(dat) 
  if(!is.null(fill)) {col = fill} else {col = NULL}
  
  g = ggplot(data=dat, aes_string(x=a[1],y=a[2], fill=col))
  
  if(dodge==TRUE) {
    g = g + geom_bar(stat = "identity", position = 'dodge') 
  } else {
    g = g + geom_bar(stat = "identity") 
  }
  
  if (flip_coord == TRUE) g = g + coord_flip()
  
  g = g + theme_few() + scale_fill_tableau()
  g = g + ggtitle(title) + xlab(xlab) + ylab(ylab) + 
    theme(
      legend.text = element_text(size=16),
      legend.title = element_text(size=20),
      axis.title.x = element_text(size=24),
      axis.title.y = element_text(size=24),
      axis.text.x = element_text(angle=xlab_angle, hjust=0.5, colour='black', size=15),
      axis.text.y = element_text(colour="black",size=15),
      plot.title = element_text(hjust=0.5, lineheight=.8, face="bold", size=28, vjust=2)
      #legend.position="none"
      #legend.justification=c(1,0), legend.position=c(0.95,0),
    )
  return(g)
}
