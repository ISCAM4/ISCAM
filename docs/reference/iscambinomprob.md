# Calculate Binomial Tail Probabilities

`binomprob` calculates the probability of the number of success of
interest using a binomial distribution and plots the distribution.

## Usage

``` r
iscambinomprob(k, n, prob, lower.tail, verbose = TRUE)
```

## Arguments

- k:

  number of successes of interest.

- n:

  number of trials.

- prob:

  success probability. Numeric between 0 & 1.

- lower.tail:

  Boolean for finding the probability above (FALSE) or below (TRUE) the
  inputted value (inclusive)

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

The probability of the binomial distribution along with a graph of the
distribution.

## Examples

``` r
iscambinomprob(k = 5, n = 20, prob = 0.4, lower.tail = TRUE)

#> Probability 5 and below = 0.125599 
#> [1] 0.125599
iscambinomprob(k = 15, n = 30, prob = 0.3, lower.tail = FALSE)

#> Probability 15 and above = 0.01693731 
#> [1] 0.01693731
iscambinomprob(k = 22, n = 25, prob = 0.9, lower.tail = TRUE)

#> Probability 22 and below = 0.4629059 
#> [1] 0.4629059
```
