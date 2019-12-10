#' res_plotter
#' @description creates a y-values vs residuals plot, with reach column of res filling one sub-plot
#' @param res a matrix of residuals
#' @param y the response vector
#' @param xlab label for x-axis, defaults to "y-values"
#' @param ylab label for y-axis, defaults to "residuals"
#' @param ... additional graphical parameters for use in title() function, such as main="Title of Plot"
#' @import graphics
#' @importFrom grDevices hcl

res_plotter <- function(res,y,xlab="y-values",ylab="residuals",...){
  n <- ncol(res)
  col <- 1:n/n * 360
  col <- hcl(h=col,c=100,l=85,alpha=0.3,fixup=TRUE)
  plot.new()
  title(xlab=xlab,ylab=ylab,...)
  bounds <- n:0/n
  abline(h = bounds)
  centers <- n:1/n - 1/n/2
  abline(h = centers,lty=3)
  par(pch=19,cex=1.4)
  for (i in 1:n){
    points(y=centers[i] + res[,i]/max(abs(res))/n/2,x=(y-min(y))/(max(y)-min(y)),col=col[i])
    here <- (bounds[i]*4 + centers[i])/5
    text(x=-0.03,y=here,labels=colnames(res)[i], pos = 4,cex=0.6)
    here <- (bounds[i] + centers[i])/2
    text(x=-0.03,y=here,labels=paste("MSE:",round(sum((res[,i])^2),3)),cex=0.6, pos = 4)
  }
  par(cex=0.7)
  m <- round(max(abs(res)),3)
  
  axis(side=2,at=bounds[c(1,n+1)],labels=c(m,-m),lwd=0,lwd.ticks=0,col="black")
  axis(side=2,at=bounds[c(-1,-(n+1))],labels=rep(paste("+/-",m),n-1),lwd=0,lwd.ticks=0,col="black")
  axis(side=2,at=centers,labels=rep(0,length(centers)),lwd=0,lwd.tcks=0,col="black")
  par(cex=1)
  
  axis(side=1,tick=TRUE ,line=NA,labels=FALSE,at=(y-min(y))/(max(y)-min(y)),col.ticks='red')
  labels <- range(y)
  labels <- c(labels[1], (labels[1]*3 + labels[2]*1)/4, (labels[1] + labels[2])/2, (labels[1] * 1 + labels[2] * 3)/4, labels[2])
  here <- (labels - labels[1])/(labels[5]-labels[1])
  print(here)
  axis(side=1,tick=TRUE,line=NA,at=here,col.ticks='black',labels=round(labels,3))
}


#' res_plotter_double
#' @description creates a y-values vs residuals plot, with one sub-plot per column in a residuals matrix.  Sub-plots are organized in two columns.  
#' @param res a matrix of residuals
#' @param y the response vector
#' @param xlab label for x-axis, defaults to "y-values"
#' @param ylab label for y-axis, defaults to "residuals"
#' @param ... additional graphical parameters for title(), useful for adding main="my plot"
#' @import graphics
#' @importFrom grDevices hcl

res_plotter_double<-function(res,y,xlab="y-values",ylab="residuals",...){
  n<-ncol(res)
  col <- 1:n/n * 360
  col <- hcl(h=col,c=100,l=85,alpha=0.3,fixup=TRUE)
  plot.new()
  title(xlab=xlab,ylab=ylab,...)
  #bounds
  lb <- -0.025;  rb <-  1.025;  ad <- 0.485  # slight gap even with 0.5 due to lb and rb not being 0 and 1
  ml <- lb + ad;  mr <- rb - ad
  n <- n/2; if (n%%1!=0) n <- n+0.5
  bounds <- n:0/n;    centers <- n:1/n - 1/n/2
  segments(x0=lb,x1=ml,
           y0 = bounds, y1 = bounds)
  segments(x0=rb,x1=mr,
           y0 = bounds, y1 = bounds)
  segments(x0=lb,x1=ml,
           y0 = centers, y1 = centers,lty=3)
  segments(x0=rb,x1=mr,
           y0 = centers, y1 = centers,lty=3)
  m <- round(max(abs(res)),3)
  par(cex=0.8)
  text(x=0.5,y=centers,labels="0")
  text(x=0.5,y=0:1,labels=c(-m,m))
  text(x=0.5,y=(1:(n-1))/n,labels=paste0("+/- ",m))
  adjust<-max(abs(res))*n*2
  par(pch=19)
  for (i in 1:n){
    par(cex=1.4)
    points(y=centers[i] + res[,i]/adjust,
           x=(y-min(y))/(max(y)-min(y))*ad + lb,
           col=col[i])
    par(cex=0.8)
    here <- (bounds[i]*3 + centers[i])/4
    text(x=lb,y=here,labels=paste("MSE:",round(sum((res[,i])^2),3)), pos = 4)
    if ((i + n)<=ncol(res)){
      par(cex=1.4)
      points(y=centers[i] + res[,i + n]/adjust,
             x=(y-min(y))/(max(y)-min(y))*ad + mr,
             col=col[i+n])
      here <- (bounds[i]*3 + centers[i])/4
      par(cex=0.8)
      text(x=mr,y=here,labels=paste("MSE:",round(sum((res[,i+n])^2),3)), pos = 4)
    }
  }
  par(cex=1)
  axis(side=2,tick=TRUE,line=NA,labels =colnames(res)[1:n]        ,at=centers,lwd=0)
  axis(side=4,tick=TRUE,line=NA,labels =colnames(res)[(n+1):(n*2)],at=centers,lwd=0)
  here <- (y-min(y))/(max(y)-min(y))/2
  here <- c(here,here) + c(rep(lb,length(y)),rep(mr,length(y)))
  axis(side=1,tick=TRUE ,line=NA,labels=FALSE,at=here,col.ticks='red')
  labels <- range(y)
  labels <- c(labels[1], (labels[1]*3 + labels[2]*1)/4, (labels[1] + labels[2])/2, (labels[1] * 1 + labels[2] * 3)/4, labels[2])
  labels <- c(labels,labels)
  here <- (labels - labels[1])/(labels[5]-labels[1])/2
  here <- here + c(rep(lb,5),rep(mr,5))
  print(here)
  axis(side=1,tick=TRUE,line=NA,at=here,col.ticks='black',labels=round(labels,3))
}