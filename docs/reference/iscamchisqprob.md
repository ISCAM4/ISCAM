# Chi-Square Probability

`chisqrprob` returns the upper tail probability for the given chi-square
statistic and degrees of freedom.

## Usage

``` r
iscamchisqprob(xval, df, verbose = TRUE)
```

## Arguments

- xval:

  the value of the chi-square statistic.

- df:

  the degrees of freedom.

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

The upper tail probability for the chi-square distribution, and a plot
of the chi-square distribution with the statistic and more extreme
shaded.

## Examples

``` r
iscamchisqprob(5, 3)

#> probability: 0.1718 
```
