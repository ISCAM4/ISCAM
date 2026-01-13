# Inverse Binomial Probability

Inverse Binomial Probability

## Usage

``` r
iscaminvbinom(alpha, n, prob, lower.tail, verbose = TRUE)
```

## Arguments

- alpha:

  The probability of interest.

- n:

  The number of trials.

- prob:

  The probability of success.

- lower.tail:

  Boolean for finding the probability above (FALSE) or below (TRUE) the
  inputted value (inclusive)

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

numeric which achieves at most the stated probability

## Examples

``` r
iscaminvbinom(alpha = 0.05, n = 30, prob = 0.5, lower.tail = TRUE)

#> The observation with at most 0.05 probability at or below is 10 
#> [1] 10

iscaminvbinom(alpha = 0.05, n = 30, prob = 0.5, lower.tail = FALSE)

#> The observation with at most 0.05 probability at or above is 20 
#> [1] 20

iscaminvbinom(alpha = 0.01, n = 60, prob = 0.10, lower.tail = FALSE)

#> The observation with at most 0.01 probability at or above is 13 
#> [1] 13
```
