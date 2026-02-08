# Overlay a Log Normal Density Function on Histogram

`addlnorm` creates a histogram of `x` and overlays a log normal density
function.

## Usage

``` r
iscamaddlnorm(
  x,
  main = "Histogram with log-normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
)
```

## Arguments

- x:

  A numeric vector representing the data to be plotted.

- main:

  (optional) title for the plot.

- xlab:

  (optional) x-axis label for the plot.

- bins:

  (optional) number of bins for the histogram.

## Value

A histogram of x overlayed with an log normal density function.

## Examples

``` r
set.seed(0)
x <- rlnorm(100)
iscamaddlnorm(x)

iscamaddlnorm(x, main = "Your Active Title", xlab = "Log Normal Data", bins = 20)
```
