# Tail Probability for t-distribution

Tail Probability for t-distribution

## Usage

``` r
iscamtprob(xval, df, direction, xval2 = NULL, verbose = TRUE)
```

## Arguments

- xval:

  observed value.

- df:

  degrees of freedom.

- direction:

  direction for probability calculation, "above" or "below"; if
  "outside" or "between" are used, a second larger observation, `xval2`
  must be specified

- xval2:

  second observation value.

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

The tail probability in the specified direction using the given
arguments.

## Examples

``` r
iscamtprob(xval = -2.05, df = 10, direction = "below")

#> probability: 0.03375 
iscamtprob(xval = 1.80, df = 20, direction = "above")

#> [1] 1.8
#> [1] 5
#> probability: 0.04348 
iscamtprob(xval = -2, xval2 = 2, df = 15, direction = "between")

#> probability: 0.9361 
iscamtprob(xval = -2.5, xval2 = 2.5, df = 25, direction = "outside")

#> probability: 0.01934 
```
