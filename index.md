---
title       : Quartic Equation Solver
subtitle    : Developing Data Products Reproducible Pitch Presentation
author      : Frank La
job         : 
framework   : io2012      # {io2012, html5slides, shower, dzslides, slideous, landslide, revealjs, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : github        # {default, tomorrow, zenburn, solarized_dark, solarized_light, twitter-bootstrap, github, ...}
widgets     : [mathjax]     # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Short Intro

1. What is it?
>* A simple app that solves a quartic (4-th order) equation of the form $$A_0 + A_1 x + A_2 x^2 + A_3 x^3 + A_4 x^4 = 0.$$

2. How does it work?
>* User gets to input coefficients \\(A_0\\), \\(A_1\\), ..., \\(A_4\\)
>* The app will solve the equation, tell you the results, and even make some plots!

3. Why this app?
>* Once in a while I'd like to have fun with some maths!

--- .class #id 



## What It Does: Root Computation and More

>* Let's assume you input \\(A_0=-1\\), \\(A_1=1\\), \\(A_2=-2\\), \\(A_3=-1\\) and \\(A_4=1\\).

>* The app will display its computation, like this:
<center><div>
  <img width="64%" src="assets/img/screenshot.png" align="middle">
</div></center> 



--- .class #id

## And It Also Makes Those Plots Below


```r
A0 <- -1; A1 <- 1; A2 <- -2; A3 <- -1; A4 <- 1   # this is just an example
source('quartic.R'); source('quarticPlot.R')     # app codes are different
sol <- quartic(A0,A1,A2,A3,A4); quarticPlot(sol) 
```

![plot of chunk unnamed-chunk-1](assets/fig/unnamed-chunk-1-1.png)


--- .class #id

## See for Yourself

1. App URL: [ShinyApp link](http://frankla.shinyapps.io/quarticSolver)

2. App source code: [GitHub link](https://github.com/FrankLa/DataProducts-ShinyAppCodes.git)

3. Presentation source code: [GitHub link](https://github.com/FrankLa/DataProducts-Presentation.git)

<br></br>

>* <center><h3>
  Have fun!
</h3></center> 



