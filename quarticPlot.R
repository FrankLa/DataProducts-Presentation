quarticPlot <- function(tmp) {
  fun <- tmp$fun
  
  Xmax <- tmp$maxima[,1]
  Xmin <- tmp$minima[,1]
  Xinfl <- tmp$inflection[,1]
  
  Ymax <- tmp$maxima[,2]
  Ymin <- tmp$minima[,2]
  Yinfl <- tmp$inflection[,2]
  
  allX <- c(tmp$realRoot, Xinfl, Xmax, Xmin)
  upper <- max(allX)
  lower <- min(allX)
  range <- upper - lower
  
  Xfrom <- lower-min(1,0.15*range)
  Xto <- upper+min(1,0.15*range)
  
  if(Xto - Xfrom == 0){
    Xto <- 1
    Xfrom <- -1
  }
  
  par(mfrow = c(1,2))
  
  curve(fun, from = Xfrom, to = Xto, n = 501, ylab = 'f(x)')
  points(tmp$realRoot, rep(0,length(tmp$realRoot)), pch = 20)
  points(Xmax, Ymax, pch = 2, col = "red", cex = 1)
  points(Xmin, Ymin, pch = 6, col = "blue", cex = 1)
  points(Xinfl, Yinfl, pch = 8, col = "green", cex = 1)
  abline(h=0, v=0, lty=2)
  
  legend("bottomleft", legend = c("Root", "Maximum", "Minimum", "Inflection"), 
         bty = "n", lwd = 1, cex = 1, col = c("black", "red", "blue", "green"), 
         lty = c(NA, NA, NA, NA), pch = c(20, 2, 6, 8))
  
  x <- c(tmp$realRoot, tmp$complexRoot)
  plot(x, col = "red", pch = 20)
  abline(h=0, v=0)
  segments(Re(x), Im(x), rep(0,length(x)), rep(0,length(x)), lty = 2)
}