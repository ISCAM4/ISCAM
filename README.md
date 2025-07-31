ISCAM
==============
***A Companion to the Book "Investigating Statistical Concepts, Applications, and Methods"***

<!-- badges: start -->
  [![R-CMD-check](https://github.com/ISCAM4/ISCAM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ISCAM4/ISCAM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

```r
# install.packages("pak") # uncomment and run this line *once*
# options(use_bioconductor = FALSE) # do this is you get some issues running the next line when connected to the eduroam network
pak::pkg_install("ISCAM4/ISCAM") # this installs or updates the package

# if {pak} doesn't work, uncomment and run the following two commands
#install.packages("remotes")
#remotes::install_github("ISCAM4/ISCAM")

# the following loads the package. You need to do this every time you want to use ISCAM functions
library(ISCAM)
```

This package collates and presents functions for ISCAM alongside some data to be used alongside the textbook.

If you don't know how a function e.g. `EXAMPLE()` works, you can write `?EXAMPLE` in the console to get help. You can also find it on [the website](https://iscam4.github.io/ISCAM/reference/index.html). Also look to the textbook/Canvas/homework to see how the function is used.

You can load the workspace by running

```r
load(url("http://www.rossmanchance.com/iscam4/ISCAM.RData"))
```

or 

```r
load(url("https://iscam4.github.io/ISCAM/ISCAM.RData"))
```

<!-- 
TODO:
- Create vignette
- Tests
-->
