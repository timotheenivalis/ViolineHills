#' Violine polygon from a distribution
#'
#' Creates violine polygon from a distribution
#'
#' @param distri A vector containing the distribution to plot
#' @param color Color to fill the shape
#'
#' @return None
#'
#' @examples
#' distri <- rnorm(2000, 0,1)
#' plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
#' distripolygonVioline(distri=distri, color="red")
#'
#' @export
distripolygonVioline <- function(distri, color, ysh=0)
{
  dd <- density(distri)
  dd$y <- dd$y/max(dd$y)
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
#'
#' @return None
#'
#' @examples
#' distri <- rnorm(2000, 0,1)
#' plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
#' distripolygonHill(distri=distri, color="red")
#'
#' @export
distripolygonHill <- function(distri, color, ysh=0)
{
  dd <- density(distri)
  polygon(x=c(min(distri),
              dd$x,
              max(distri),
              min(distri)),
          y=c(0,dd$y,0,0)+ysh,
          fillOddEven = TRUE,
          col=color)
}#end function()

plotdensities <- function(distributions, distrilayout=NULL, xdis="Value")
{
  nbdistr <- length(distributions)
  if(is.null(distrilayout))
  {
    distrilayout <- matrix(1:nbdistr,nrow =nbdistr, ncol = 1)
  }
  Xglobalmin <- min(unlist(lapply(distributions, FUN = function(x)min(x, na.rm = TRUE))))
  Xglobalmax <- max(unlist(lapply(distributions, FUN = function(x)max(x, na.rm = TRUE))))
  

  ran <- seq(-(nbdistr-1), nbdistr-1, length.out = nbdistr)
  
  plot(0, xlim=c(Xglobalmin,Xglobalmax), type='n', ylim=c(min(ran)-1,max(ran)+1),
       xlab = xdis, ylab="probability density", yaxt="n")
  
  for (i in 1:nbdistr)
  {
    coor <- which(distrilayout==i, arr.ind = TRUE)
    
    if(coor[2]==1)
    {
      abline(h=ran[i], col="gray")
    }
    distripolygonVioline(distri = distributions[[i]], color = "red", ysh = ran[coor[1]])
    realscale <- round(max(density(distributions[[i]])$y)/2, 2)
    axis(side = 2, at = c(ran[coor[1]]-0.5,ran[coor[1]],ran[coor[1]]+0.5), labels = c(realscale,0,realscale), tick = TRUE)
  }
}#end function()
