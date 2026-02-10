# Hypergeometric p-value and Distribution

Hypergeometric p-value and Distribution

## Usage

``` r
iscamhyperprob(k, total, succ, n, lower.tail, verbose = TRUE)
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
distribution with the observed statistic and more extreme shaded.

## Examples

``` r
iscamhyperprob(1, 20, 5, 10, TRUE)

#> Probability 1 and below = 0.1517028 
```
