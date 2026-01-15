# Hypergeometric p-value and Distribution Overlaid with Normal Distribution

Hypergeometric p-value and Distribution Overlaid with Normal
Distribution

## Usage

``` r
iscamhypernorm(k, total, succ, n, lower.tail, verbose = TRUE)
```

## Arguments

- k:

  Number of successes of interest or difference in conditional
  proportions

- total:

  Total number of observations in the study

- succ:

  Overall number of successes

- n:

  Number of observations in group A

- lower.tail:

  Boolean for finding the probability above (FALSE) or below (TRUE) the
  inputted value (inclusive)

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

Tail probabilities from the hypergeometric distribution, hypergeometric
distribution with normal distribution overlayed with the observed
statistic and more extreme shaded.

## Examples

``` r
iscamhypernorm(1, 20, 5, 10, TRUE)

#>  hypergeometric: 0.1517 
#>  normal approx: 0.06553 
#>  normal approx with continuity: 0.1571 
```
