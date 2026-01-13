# Inverse Normal Calculation

Inverse Normal Calculation

## Usage

``` r
iscaminvnorm(prob1, mean = 0, sd = 1, Sd = sd, direction, verbose = TRUE)
```

## Arguments

- prob1:

  probability to find normal quantile of.

- mean:

  mean of normal distribution.

- sd:

  standard deviation of normal distribution.

- Sd:

  deprecated–available for backwards compatibility.

- direction:

  direction for probability calculation: "above", "below", "outside",
  "between".

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

a plot of the normal distribution with the quantile of the specified
probability highlighted.

## Examples

``` r
iscaminvnorm(0.05, direction = "below")

#> The observation with 0.05 probability below is -1.645 
iscaminvnorm(0.90, mean = 100, sd = 15, direction = "above")

#> The observation with 0.9 probability above is 80.78 
iscaminvnorm(0.10, direction = "outside")

#> There is 0.1 probability outside -1.645 and 1.645 
iscaminvnorm(0.95, direction = "between")

#> There is 0.95 probability between -1.96 and 1.96 
```
