gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Erstelle den gg-Plot eines XTS Objekts 
plotXTS <- function(xtsObject, title, xlab = "time", ylab = "value"){
  d <- data.frame(time = index(xtsObject), value = drop(coredata(xtsObject)))
  if (dim(xtsObject)[2]) {
    d <- melt(d, id.vars = "time", varnames = names(dimnames(xtsObject)))
  }
  temp <- ggplot(d, aes(time, value)) +
    xlab(xlab) +
    ylab(ylab) + 
    scale_colour_hue() +
    theme(plot.title = element_text(lineheight = .8, face = "bold"), text = element_text(size = 14))
  
  if (dim(xtsObject)[2] > 1) {
    temp <- temp + geom_line(aes(colour = variable))
  }
  else {
    temp <- temp + geom_line(colour = gg_color_hue(1))
  }
  
  if (!missing(title)) {
    temp + ggtitle(title)
  }
  else {
    temp
  }
}

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

xts2df <- function(xts) {
  stopifnot(is.xts(xts))
  data.frame(time = index(xts),drop(coredata(xts)))
}

df2xts <- function(df) {
  stopifnot(is.data.frame(df))
  xts(df[,("time" != colnames(df))],df[,("time" == colnames(df))])
}