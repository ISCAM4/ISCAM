ISCAM
==============
***A Companion to the Book "Investigating Statistical Concepts, Applications, and Methods"***

<!-- badges: start -->
  [![R-CMD-check](https://github.com/ISCAM4/ISCAM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ISCAM4/ISCAM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

```r
# install.packages("pak") # uncomment this line and run it *once*

# options(use_bioconductor = FALSE) # do this is you get some issues running the next line when connected to the eduroam network
pak::pkg_install("ISCAM4/ISCAM") # this installs or updates the package

# if {pak} doesn't work, try the following two commands
#install.packages("remotes")
#remotes::install_github("ISCAM4/ISCAM")

library(ISCAM) # this loads the package. You need to do this every time you want to use ISCAM functions
```

This package collates and presents functions for ISCAM alongside some data to be used alongside the textbook.

If you don't know how a function e.g. `EXAMPLE()` works, you can write `?EXAMPLE` in the console to get help. Also look to the textbook/Canvas/homework to see how the function is used.

Old Workspace: `load(url("http://www.rossmanchance.com/iscam3/ISCAM.RData"))`

<!-- 
TODO:
- [ ] Create vignette?
- [ ] Add examples
- [ ] Tests? 
-->
