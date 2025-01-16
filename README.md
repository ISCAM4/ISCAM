ISCAM
==============
***A Companion to the Book "Investigating Statistical Concepts, Applications, and Methods"***

<!-- badges: start -->
  [![R-CMD-check](https://github.com/Vishwarrior26/ISCAM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Vishwarrior26/ISCAM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

```
# install.packages("pak") # uncomment this line and run it *once*.
pak::pkg_install("VisruthSK/ISCAM") # this installs the package
library(ISCAM) # this loads the package
```

This package collates and presents functions for ISCAM alongside some data. To be used alongside the textbook.

If you don't know how a function e.g. `EXAMPLE()` works, you can write `?EXAMPLE` in the console to get help and some examples. Also look to the textbook to see how it is used. If you see code like `iscambinomprob(14, 16, .5, FALSE)` in the text or elsewhere, use `ISCAM::binomprob(14, 16, .5, FALSE)`.

Workspace: `load(url("http://www.rossmanchance.com/iscam3/ISCAM.RData"))`

TODO:
- [ ] Write documentation
  - [ ] Add examples
  - [ ] Create vignette?
- [ ] Test?
- [ ] Write readme on Github
