gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# Erstelle den gg-Plot eines XTS Objekts 
plotXTS <- function(xtsObject,title,xlab="time",ylab="value"){
  d <- data.frame(time=index(xtsObject), value=drop(coredata(xtsObject)))
  if(dim(xtsObject)[2]) {
    d <- melt(d, id.vars="time", varnames= names(dimnames(xtsObject)))
  }
  temp <- ggplot(d, aes(time, value)) +
    xlab(xlab) +
    ylab(ylab) + 
    scale_colour_hue() +
    theme(plot.title = element_text(lineheight=.8, face="bold"), text = element_text(size=14))
  
  if(dim(xtsObject)[2]>1){
    temp <- temp + geom_line(aes(colour=variable))
  }
  else {
    temp <- temp + geom_line(colour = gg_color_hue(1))
  }
  
  if(!missing(title)){
    temp + ggtitle(title)
  }
  else {
    print(temp)
  }
}
