#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
distripolygonVioline <- function(distri, color)
{
  dd <- density(distri)
  polygon(x=c(min(distri),
              dd$x,
              rev(dd$x),
              min(distri)),
          y=c(0,dd$y,rev(-dd$y),0),
          fillOddEven = TRUE,
          col=color)
}#end function()

#' Illustration of crayon colors 2
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
distripolygonHill <- function(distri, color)
{
  dd <- density(distri)
  polygon(x=c(min(distri),
              dd$x,
              max(distri),
              min(distri)),
          y=c(0,dd$y,0,0),
          fillOddEven = TRUE,
          col=color)
}#end function()
