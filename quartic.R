# quartic.R 
# find all roots, extrema and inflection points of quartic equation
# and plot its graph
# input A0, A1, A2, A3, A4 real numbers
# f(X) = A0 + A1*X + A2*X^2 + A3*X^3 + A4*X^4 = 0 

quartic <- function(A0=0, A1=0, A2=0, A3=0, A4=1) {
  # The function 
  fun <- function(x){
    A0 + A1*x + A2*x^2 + A3*x^3 + A4*x^4
  }
  
  # 1. Find all roots, real or complex -------------------------------------
  roots <- polyroot(c(A0, A1, A2, A3, A4))
  roots <- round(roots, 6) 
  
  # separate real roots & complex roots
  realRootIndex <- which(Im(roots)==0)
  cplxRootIndex <- which(Im(roots)!=0)
  
  realRoot <- as.numeric(roots[realRootIndex])
  cplxRoot <- roots[cplxRootIndex]
  
  
  # 2. Find all extrema ----------------------------------------------------
  
  # First derivative
  B <- (1:4) * c(A1, A2, A3, A4)
  
  FP <- function(X){
    B[1] + B[2]*X + B[3]*X^2 + B[4]*X^3
  }
  
  extrema <- polyroot(B)
  extrema <- round(extrema, 6)
  
  # separate real extrema
  realExtIndex <- which(Im(extrema)==0)
  # cplxExtIndex <- which(Im(extrema)!=0)
  
  realExt <- as.numeric(extrema[realExtIndex])
  realExt <- unique(realExt)
  # cplxExt <- extrema[cplxExtIndex]
  
  
  # Second derivative
  C <- (1:3) * B[2:4]
  
  FPP <- function(X){
    C[1] + C[2]*X + C[3]*X^2
  }
  
  # Find maxima and minima 

  indicator <- sapply(realExt, function(x){
    if(FPP(x) < 0){-1}  # maximum
    else if(FPP(x) > 0){1} # minimum
    else {  
      # 2nd derivative test is inconclusive. Hence, need to calculate signs of f'(x+/-e)
      e <- 1e-3
      if(FP(x-e) > 0 && FP(x+e) < 0){-1} # maximum
      else if(FP(x-e) < 0 && FP(x+e) > 0){1} # minimum
      else {0}
    }
  })
  Xmax <- realExt[which(indicator==-1)]
  Xmin <- realExt[which(indicator==1)]

  Ymax <- fun(Xmax)
  Ymin <- fun(Xmin)
  
  maxima <- data.frame(X=Xmax,Y=Ymax)
  minima <- data.frame(X=Xmin,Y=Ymin)
  
  # 3. Find inflection points -----------------------------------------------
  rootC <- polyroot(C)
  rootC <- round(rootC, 6)
    
  # separate real points
  realRootCIndex <- which(Im(rootC)==0)
  
  realRootC <- as.numeric(rootC[realRootCIndex])
  realRootC <- unique(realRootC)
  
  # Third derivative
  D <- (1:2) * C[2:3]
  
  FPPP <- function(X){
    D[1] + D[2]*X
  }
  
  # for quartic functions, inflections are points where f''(x)=0 and f'''(x) != 0
  indicator2 <- sapply(realRootC, function(x){
    if(FPPP(x) != 0){1}  # inflection 
    else {0} # not inflection
  })
  
  Xinfl <- realRootC[which(indicator2==1)]
  Yinfl <- fun(Xinfl)
  
  inflection <- data.frame(X=Xinfl,Y=Yinfl)

  
  # Return
  results <- list(fun=fun, realRoot=realRoot, complexRoot=cplxRoot, 
                  minima=minima, maxima=maxima, inflection=inflection)
  results
}
  