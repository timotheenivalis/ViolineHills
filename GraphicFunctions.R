#' Violine polygon from a distribution
#'
#' Creates violine polygon from a distribution
#'
#' @param distri A vector containing the distribution to plot
#' @param color Color to fill the shape
#' @param ysh Shift on the y-axis
#' @param maxdensity NULL
#' @param logy Boolean
#' 
#' @return None
#'
#' @examples
#' distri <- rnorm(2000, 0,1)
#' plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
#' distripolygonVioline(distri=distri, color="red")
#'
#' @export
distripolygonVioline <- function(distri, color, ysh=0, maxdensity=NULL, logy=FALSE)
{
  dd <- density(distri)
  if(logy)
  {
    dd$y <- log(dd$y+1)
  }
  if(is.null(maxdensity))
  {
   dd$y <- dd$y/max(dd$y)
  }else{
    dd$y <- dd$y/maxdensity
  }

  polygon(x=c(min(distri),
              dd$x,
              rev(dd$x),
              min(distri)),
          y=c(0,dd$y,rev(-dd$y),0)+ysh,
          fillOddEven = TRUE,
          col=color)
}#end function()

#' One-side polygon from a distribution
#'
#' Creates a polygon from a distribution
#'
#' @param distri A vector containing the distribution to plot
#' @param color Color to fill the shape
#' @param ysh Shift on the y-axis
#' @param maxdensity NULL
#' @param logy Boolean
#'
#' @return None
#'
#' @examples
#' distri <- rnorm(2000, 0,1)
#' plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
#' distripolygonHill(distri=distri, color="red")
#'
#' @export
distripolygonHill <- function(distri, color, ysh=0, maxdensity=NULL, logy=FALSE)
{
  dd <- density(distri)
  if(logy)
  {
    dd$y <- log(dd$y+1)
  }
  if(is.null(maxdensity))
  {
    dd$y <- dd$y/max(dd$y)
  }else{
    dd$y <- dd$y/maxdensity
  }

  polygon(x=c(min(distri),
              dd$x,
              max(distri),
              min(distri)),
          y=c(0,dd$y,0,0)+ysh,
          fillOddEven = TRUE,
          col=color)
}#end function()

#' Switch between violine and one sided distribution
#'
#' Switches between violine and one sided distribution, for internal use
#'
#' @param violine Boolean, if TRUE (default) uses a violine distribution, otherwise, uses a one sided distribution
#'
#' @return distrifunction
#'
#' @export
PolyViolineSwitch <- function(violine=TRUE)
{
  if(violine)
    {
    distrifunction <- distripolygonVioline
  }else{
      distrifunction <- distripolygonHill
    }
  return(distrifunction)
}#end function()


#' Plots multiple density distributions
#'
#' To plot multiple density distributions
#'
#' @param distributions A vector containing the distribution to plot
#' @param distrilayout Color to fill the shape
#' @param xdis X-label 
#' @param violine Boolean. If TRUE draws a violine plot, otherwise, draws a one-sided distribution
#' @param logy Boolean
#' @param dlegend Null or legend caption
#'
#' @return None
#'
#' @examples
#' distri1 <- rnorm(2000, 0,1)
#' distri2 <- rnorm(2000, 1, 1)
#' distributions
#' plotdensities(distri=distri, color="red")
#'
#' @export
plotdensities <- function(distributions, distrilayout=NULL, xdis="Value", violine=TRUE, logy=FALSE, dlegend = NULL)
{
  nbdistr <- length(distributions)
  if(is.null(distrilayout))
  {
    distrilayout <- matrix(1:nbdistr,nrow =nbdistr, ncol = 1)
  }
  #take the global bounds on x-axis
  Xglobalmin <- min(unlist(lapply(distributions, FUN = function(x)min(x, na.rm = TRUE))))
  Xglobalmax <- max(unlist(lapply(distributions, FUN = function(x)max(x, na.rm = TRUE))))
  
  #take the global bounds on y-axis
  Yglobalmax <- max(unlist(lapply(distributions, FUN =function(x) max(density(x)$y, na.rm = TRUE))))
  if(logy) {
    Yglobalmax <- log(Yglobalmax+1)
  }
  
  #spreading the rows vertically
  ran <- seq(-(nrow(distrilayout)-1), nrow(distrilayout)-1, length.out = nrow(distrilayout))
  
  legendspace <- 2
  plot(0, xlim=c(Xglobalmin,Xglobalmax), type='n', ylim=c(min(ran)-1,max(ran)+1+legendspace),
       xlab = xdis, ylab="Relative probability density", yaxt="n")
  abline(v=0)
  
  distrifunction <- PolyViolineSwitch(violine = violine)

  transpColors <-  heat.colors(ncol(distrilayout), alpha = 0.4)
  
  for (i in 1:nbdistr)
  {
    coor <- which(distrilayout==i, arr.ind = TRUE)
    
    distrifunction(distri = distributions[[i]], color = transpColors[coor[2]], ysh = ran[coor[1]],
                   maxdensity = Yglobalmax,logy =logy)
    realscale <- round(max(density(distributions[[i]])$y)/2, 2)
    if(coor[2]==1)
    {
      abline(h=ran[i], col="gray")
      #axis(side = 2, at = c(ran[coor[1]]-0.5,ran[coor[1]],ran[coor[1]]+0.5), labels = c(realscale,0,realscale), tick = TRUE)
      
    }
  }
  
  if(!is.null(dlegend))
  {
    legend(x="topleft", legend = dlegend, fill = transpColors)
  }
}#end function()
