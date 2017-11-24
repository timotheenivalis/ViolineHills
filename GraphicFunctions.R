
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
