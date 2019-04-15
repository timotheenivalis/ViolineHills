#' Violine polygon from a distribution
#'
#' Creates violine polygon from a distribution
#'
#' @param distri A vector containing the distribution to plot
#' @param color Color to fill the shape
#' @param ysh Shift on the y-axis
#' @param maxdensity NULL
#' @param transform Function
#' 
#' @return None
#'
#' @examples
#' distri <- rnorm(2000, 0,1)
#' plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
#' distripolygonVioline(distri=distri, color="red")
#'
#' @export
distripolygonVioline <- function(distri, color, ysh=0, maxdensity=NULL, transform=identity, adjust=1)
{
  dd <- density(distri, from=min(distri), to=max(distri), adjust=adjust)
  dd$y <- transform(dd$y)

  if(is.null(maxdensity))
  {
   dd$y <- 0.9*dd$y/max(dd$y)
  }else{
    dd$y <- 0.9*dd$y/maxdensity
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
#' @param transform Function
#'
#' @return None
#'
#' @examples
#' distri <- rnorm(2000, 0,1)
#' plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
#' distripolygonHill(distri=distri, color="red")
#'
#' @export
distripolygonHill <- function(distri, color, ysh=0, maxdensity=NULL, transform=identity, adjust = 1)
{
  dd <- density(distri, cut=3, adjust = adjust)
  dd$y <- transform(dd$y)

  if(is.null(maxdensity))
  {
    dd$y <- 0.9*dd$y/max(dd$y)
  }else{
    dd$y <- 0.9*dd$y/maxdensity
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
#' @param transform Function. Transform on the y-axis (e.g. log).
#' @param dlegend Null or legend caption
#' @param globalmaxdensity Boolean
#' @param col Colors if specified Default is "rainbow" and creates colors based on a transparent raibow palette.
#'
#' @return None
#'
#' @examples
#' distri1 <- rnorm(2000, 0,1)
#' distri2 <- rnorm(2000, 1, 1)
#' plotdensities(distributions=list(distri1,distri2))
#'
#' @export
plotdensities <- function(distributions, distrilayout=NULL, xdis="Value",
                          violine=TRUE, dlegend = NULL, globalmaxdensity=TRUE,
                          colgroups=NULL, legendncol=1, transform=identity, adjust=1,
                          col="rainbow")
{
  nbdistr <- length(distributions)
  nonna <- which(unlist(lapply(distributions, function(x){!is.na(x[1])})))
  if(is.null(distrilayout))
  {
    distrilayout <- matrix(1:nbdistr,nrow =nbdistr, ncol = 1)
  }
  #take the global bounds on x-axis
  Xrange <- max(unlist(lapply(distributions[nonna], FUN = function(x)min(x, na.rm = TRUE))))-min(unlist(lapply(distributions[nonna], FUN = function(x)min(x, na.rm = TRUE))))
  
  Xglobalmin <- min(unlist(lapply(distributions[nonna], FUN = function(x)min(x, na.rm = TRUE))))-0.1*Xrange 
  Xglobalmax <- max(unlist(lapply(distributions[nonna], FUN = function(x)max(x, na.rm = TRUE))))+0.1*Xrange
  
  #take the global bounds on y-axis
  dmodes <- unlist(lapply(distributions[nonna], FUN =function(x) max(transform(density(x)$y), na.rm = TRUE)))
  
  max(dmodes)/min(dmodes)
  
  if(globalmaxdensity)
    {
      Yglobalmax <- max(dmodes)
  }else{
    Yglobalmax <- NULL
  }
  
  #spreading the rows vertically
  ran <- seq(-(nrow(distrilayout)-1)/(2-violine), (nrow(distrilayout)-1)/(2-violine), length.out = nrow(distrilayout))
  
  legendspace <- length(dlegend)/legendncol - max(0.5,violine)
  plot(0, xlim=c(Xglobalmin,Xglobalmax), type='n',
       ylim=c(min(ran)-0.75*violine,max(ran)+1/(2-violine)+legendspace),
       xlab = xdis, ylab="", yaxt="n")
  #abline(v=0,)
  lines(x = c(0,0), y=c(-nbdistr, nbdistr+0.1))
  
  distrifunction <- PolyViolineSwitch(violine = violine)

  if(is.null(colgroups))
    {
    if(length(col)==1) if(col=="rainbow")
      {
        transpColors <-  rainbow(ncol(distrilayout), alpha = 0.4)
    }else{
      transpColors <-  col
      }
     colgroups <- matrix(data = rep(1:ncol(distrilayout), 
                                    nrow(distrilayout)), 
                         ncol = ncol(distrilayout), byrow = TRUE)
  }else{
    if(length(col)==1) if(col=="rainbow")
    {
    transpColors <- rainbow(length(unique(as.vector(colgroups))), alpha = 0.4)
    }else{
      transpColors <-  col
    }
  }
  
  for (i in 1:nbdistr)
  {
    if(i %in% nonna)
    {
      coor <- which(distrilayout==i, arr.ind = TRUE)
    if(coor[2]==1)
    {
      realscale <- round(max(density(distributions[[i]])$y)/2, 2)
      abline(h=ran[coor[1]], col="gray")
      #axis(side = 2, at = c(ran[coor[1]]-0.5,ran[coor[1]],ran[coor[1]]+0.5), labels = c(realscale,0,realscale), tick = TRUE)
      
    }
    distrifunction(distri = distributions[[i]], color = transpColors[colgroups[coor]], 
                   ysh = ran[coor[1]],
                   maxdensity = Yglobalmax, transform=transform, adjust=adjust)

    }
  }
  
  if(!is.null(dlegend))
  {
    legend(x="topleft", legend = dlegend, fill = transpColors, ncol = legendncol, bg=rgb(1,1,1,1))
  }
}#end function()
