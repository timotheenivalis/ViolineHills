#' Violine polygon from a distribution
#'
#' Creates violine polygon from a distribution
#'
#' @param distri A vector containing the distribution to plot
#' @param color Color to fill the shape
#' @param ysh Shift on the y-axis
#' @param maxdensity NULL
#' @param transform Function
#' @param yax Do we plot a y-axis?
#' @param minden Minimum mode of a density relative to the maximal density of all distributions
#' @param adjust Smoothing factor in the function density()
#' @param border The color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.
#' 
#' @return None
#'
#' @examples
#' distri <- rnorm(2000, 0,1)
#' plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
#' distripolygonVioline(distri=distri, color="red")
#'
#' @export
distripolygonVioline <- function(distri, color, ysh=0, yax=FALSE, minden=0.6,
                                 maxdensity=NULL, transform=identity, adjust=1,
                                 border=NULL)
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
          col=color, 
          border = border)
}#end function()

#' One-side polygon from a distribution
#'
#' Creates a polygon from a distribution
#'
#' @param distri A vector containing the distribution to plot
#' @param color Color to fill the shape
#' @param ysh Shift on the y-axis
#' @param maxdensity NULL
#' @param transform Function used to transform the density distribution (that is, the y-axis)
#' @param yax Do we plot a y-axis?
#' @param minden Minimum mode of a density relative to the maximal density of all distributions
#' @param adjust Smoothing factor in the function density()
#' @param border The color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.
#'
#' @return None
#'
#' @examples
#' distri <- rnorm(2000, 0,1)
#' plot(1, xlim=c(-3,3), type='n', ylim=c(-1,1))
#' distripolygonHill(distri=distri, color="red")
#'
#' @export
distripolygonHill <- function(distri, color, ysh=0, yax=FALSE, maxdensity=NULL, 
                              transform=identity, minden=0.1, adjust = 1,
                              border = NULL)
{
  dd <- density(distri, from=min(distri), to=max(distri), cut=3, adjust = adjust)
  dd$y <- transform(dd$y)
  ddval <- dd$y
  
  if(is.null(maxdensity))
  {
    dd$y <- 0.9*dd$y#/max(dd$y)
  }else{
    dd$y <- 0.9*dd$y/maxdensity#/ifelse( (max(dd$y)/maxdensity)<minden, maxdensity*minden, maxdensity)
  }
  ratiomax <- max(dd$y)
  if(ratiomax < minden) {
    dd$y <- 0.9*dd$y * (minden/ratiomax)
  }
  
  polygon(x=c(min(distri),
              dd$x,
              max(distri),
              min(distri)),
          y=unlist(c(0,dd$y,0,0))+ysh,
          fillOddEven = TRUE,
          col=color,
          border = border)
  if(yax)
  {
    rfac <- ceiling(-log(max(ddval), base=10))
    labaxis <- round(seq(from=0, to = 1.1*max(ddval), by = 10^(-rfac)), digits = rfac)
    if(length(labaxis)<3){labaxis <- round(seq(from=0, to = 1.1*max(ddval), by = 5*10^(-rfac-1)), digits = rfac+1)}
    labaxis <- labaxis[ labaxis<=ifelse(is.null(maxdensity), 1.1*max(ddval),maxdensity)]
    if(length(labaxis)>6){labaxis <- labaxis[round(seq(from=1, to =length(labaxis), length.out = 6))]}
    
    labaxistrans <- labaxis

    if(is.null(maxdensity))
    {
      labaxistrans <- 0.9*labaxistrans#/max(labaxistrans)
    }else{
      labaxistrans <- 0.9*labaxistrans/maxdensity#ifelse( (max(labaxistrans)/maxdensity)<minden, maxdensity*minden, maxdensity)
    }
    
    axis(side = 2, at = ysh + labaxistrans,line = 0,
         labels = labaxis, cex.axis=0.7, tck=-0.02)
  }
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
#' @param distributions A list, or list of lists containing the distributions to plot
#' @param violine Boolean. If TRUE draws a violine plot, otherwise, draws a one-sided distribution
#' @param transform Function. Transform on the y-axis (e.g. log).
#' @param dlegend Null or legend caption
#' @param globalmaxdensity Boolean
#' @param col Colors if specified Default is "rainbow" and creates colors based on a transparent raibow palette.
#' @param rowleg Legend to be applied in each row
#' @param sublegend sub-legend
#' @param cexsubl Magnification factor for sublegends
#' @param sublegside Side where to pring sublegend
#' @param btylegend bty argument in the function legend()
#' @param blikefill Boolean. Should the legend boxes be black (FALSE) or of the same color as the fill (TRUE)
#' @param legendncol Number of columns in the legend
#' @param adjust Smothing factor in the function density()
#' @param xshift Vector of size 2 giving the bottom and top shift to be applied to the ploting region
#' @param yshift Vector of size 2 giving the left and right shift to be applied to the ploting region
#' @param minden Minimum mode of a density relative to the maximal density of all distributions
#' @param rowtext Character vector with text to be written in each row
#' @param xlim Vector of size 2 giving the x-limits of the plot
#' @param rowtextshift Vector of size 2 giving the x and y shift to be applied to the position of text in rows
#' @param returnxpos Boolean. If TRUE the function returns the y-coordonates of each row for use in other graphic functions (such as mtext())
#' @param yax Boolean. If TRUE labels appear on the y-axis.
#' @param border The color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.
#' @param ... Other parameters to be passed to plot()
#'
#' @return None
#'
#' @examples
#' distri1 <- rnorm(2000, 0,1)
#' distri2 <- rnorm(2000, 1, 1)
#' plotdensities(distributions=list(distri1,distri2))
#'
#' @importFrom stats density
#' @importFrom graphics abline axis legend lines plot polygon text
#' @importFrom grDevices rainbow rgb
#'
#' @export
plotdensities <- function(distributions, 
                          violine=FALSE, rowleg = NULL, dlegend = NULL, globalmaxdensity=TRUE,
                          sublegend = NULL, cexsubl=1, sublegside = "left", btylegend ="o",
                          blikefill=FALSE,
                          legendncol=1, transform=identity, adjust=1,
                          xshift=c(0,0), yshift=c(0,0), minden=0.2,
                          col="rainbow", rowtext=NULL, xlim=NULL, rowtextshift=c(0,0),
                          returnxpos = FALSE, yax=TRUE, border = NULL, ...)
{
  if(typeof(distributions)!="list")
  {
    stop("distributions should be a list")
  }
  
  if(length(distributions)==1) {globalmaxdensity <- FALSE}
  
  for (i in 1:length(distributions))
  {
    if(typeof(distributions[[i]]) != "list")
    {
      distributions[[i]] <- list(distributions[[i]])
    }
  }
  
  nbdistr <- length(distributions)

  #take the global bounds on x-axis
  
  Xrange <- (xshift[2]+max(unlist(distributions), na.rm = TRUE)) - (xshift[1]+min(unlist(distributions), na.rm = TRUE))
  
  Xglobalmin <- xshift[1] + min(unlist(distributions), na.rm = TRUE)-0.1*Xrange 
  Xglobalmax <- xshift[2] + max(unlist(distributions), na.rm = TRUE) + 0.1*Xrange
  
  #take the global bounds on y-axis
  Ymax <- lapply(distributions, FUN = function(y){
    max(unlist(lapply(y,function(x){
      max(transform(density(x)$y), na.rm = TRUE)
    })))
  })
  
   #experiment with local maximum
  
  
  if(globalmaxdensity)
  {
    Yglobalmax <- max(unlist(Ymax))
  }else{
    Yglobalmax <- NULL
  }
  
  if(is.null(xlim)){
    xlim <- c(Xglobalmin,Xglobalmax)
  }
  
  #spreading the rows vertically
  nbrows <- length(distributions)#unique(distrilayout))
  ran <- seq((nbrows-1)/(2-violine), 
             -(nbrows-1)/(2-violine), 
             length.out = nbrows)
  
  legendspace <- ifelse(is.null(dlegend), 0, length(dlegend))/legendncol #- 2*max(0.35,violine)
  plot(0, xlim=xlim, type='n',
       ylim=c(min(ran)-0.75*violine + yshift[1], max(ran) + 0.9 + legendspace + yshift[2]),#+ 1/(2-violine)),
       yaxt="n",
       ...)
  axis(side = 1, at = round(seq(Xglobalmin, Xglobalmax)), labels = NA, tck = -0.02)
  lines(x = c(0,0), y=c(-nbdistr, nbdistr+0.1))
  
  distrifunction <- PolyViolineSwitch(violine = violine)
  
  if(col[1]=="rainbow")
  {
    ldl <- lapply(distributions, length)
    allcol <-  rainbow(max(unlist(ldl)), alpha = 0.4)
    transpColors <- list(length(ldl))
    poscol <- 1
    for (i in 1:length(ldl))
    {
      transpColors[[i]] <- allcol[poscol:(poscol-1+ldl[[i]])]
    }
  }else{
    transpColors <-  col
  }
  
  for (i in 1:length(distributions))
  {
    yaxd <- which.max(unlist(lapply(distributions[[i]], function(x) max(density(x)$y))))
    rowmax <- ifelse(is.null(Yglobalmax), Ymax[[i]], Yglobalmax)
    for(j in 1:length(distributions[[i]]))
    {
      if(length(border)<length(distributions[[i]]) ){
        borderv <- rep(border[1], times=length(distributions[[i]]) )
        }else{borderv <- border}
      
      distrifunction(distri = distributions[[i]][[j]], color = transpColors[[i]][[j]], 
                     ysh = ran[i], yax= yax & (j==yaxd), minden=minden,
                     maxdensity = rowmax, transform=transform, adjust=adjust,
                     border=borderv[j])
    }
    abline(h=ran[i], col="gray")
  }
  
  if(!is.null(rowtext))
  {
    text(x=Xglobalmin+0.05*(Xglobalmax-Xglobalmin)+rowtextshift[1], y=ran+rowtextshift[2], labels = rowtext)
  }
  
  if(!is.null(dlegend))
  {
    legend(x="topleft", legend = dlegend, fill = unique(unlist(transpColors)),
           ncol = legendncol, bg=rgb(1,1,1,1))
  }
  
  
  if(!is.null(sublegend))
  {
    if(length(sublegside) < length(sublegend))
    {
      sublegside <- rep(sublegside, length(sublegend))
    }
    for (i in 1:length(sublegend))
    {
      if(!is.null(sublegend[[i]]))
        if(sublegside[i]=="right")
        {legend(x=Xglobalmax, y=ran[i]+ifelse(globalmaxdensity, 0.9, max(unlist(Ymax))),legend = unlist(sublegend[[i]]),
                fill = unlist(transpColors[[i]]), cex = cexsubl, xjust = 1, yjust = 1,
                title = names(sublegend)[i], border = ifelse(blikefill,unlist(transpColors[[i]]), "black"), bty = btylegend)
        }else{
          legend(x=Xglobalmin, y=ran[i]+ifelse(globalmaxdensity, 0.9, max(unlist(Ymax))),legend = unlist(sublegend[[i]]),
                 fill = unlist(transpColors[[i]]), cex = cexsubl, xjust = 0, yjust = 1,
                 title = names(sublegend)[i], border = ifelse(blikefill,unlist(transpColors[[i]]), "black"), bty = btylegend)
        }
    }
  }
 if(returnxpos){return(ran+rowtextshift[2])}
}#end function()
