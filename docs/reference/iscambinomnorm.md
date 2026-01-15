# Overlays Normal Approximation onto Binomial

`binomnorm` creates a binomial distribution of the given inputs and
overlays a normal approximation.

## Usage

``` r
iscambinomnorm(k, n, prob, direction, verbose = TRUE)
```

## Arguments

- k:

  number of successes of interest

- n:

  number of trials

- prob:

  success probability

- direction:

  "above", "below", or "two.sided"

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

A plot of the binomial distribution overlayed with the normal
approximation

## Examples

``` r
iscambinomnorm(k = 10, n = 20, prob = 0.5, direction = "two.sided")

#>  binomial: 1.176 
#>  normal approx: 1 
#>  normal approx with continuity: 1.177 
```
