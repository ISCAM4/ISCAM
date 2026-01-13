# Overlay a Normal Density Function on Histogram

`addnorm` creates a histogram of `x` and overlays a normal density
function.

## Usage

``` r
iscamaddnorm(
  x,
  main = "Histogram with normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
)
```

## Arguments

- x:

  A numeric vector representing the data to be plotted.

- main:

  Optional title for the plot

- xlab:

  Optional x-axis label for the plot

- bins:

  Optional number of bins for the histogram.

## Value

A histogram of x overlayed with an normal density function.

## Examples

``` r
set.seed(0)
x <- rnorm(100)
iscamaddnorm(x)

iscamaddnorm(x, main = "Your Active Title", xlab = "Normal Data", bins = 20)
```
