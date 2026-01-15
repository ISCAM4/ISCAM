# Overlay a t Density Function and a Normal Density Function on Histogram

Overlay a t Density Function and a Normal Density Function on Histogram

## Usage

``` r
iscamaddtnorm(
  x,
  df,
  main = "Histogram with t and normal curve",
  xlab = deparse(substitute(x)),
  bins = NULL
)
```

## Arguments

- x:

  A numeric vector representing the data to be plotted.

- df:

  A numeric value representing the degrees of freedom of `x`.

- main:

  Optional title for the plot

- xlab:

  Optional x-axis label for the plot

- bins:

  Optional number of bins for the histogram.

## Value

A histogram of x overlayed with an t density function and a normal
density function.

## Examples

``` r
set.seed(0)
x <- rt(100, 5)
iscamaddtnorm(x, 5)

iscamaddtnorm(x, 5, main = "Your Active Title", xlab = "t Data", bins = 20)
```
