# Inverse T Calculation

`invt` calculates the t quantile of a specified probability.

## Usage

``` r
iscaminvt(prob, df, direction, verbose = TRUE)
```

## Arguments

- prob:

  Desired probability.

- df:

  Degrees of freedom

- direction:

  direction for probability calculation: "above", "below", "outside",
  "between".

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

The t value for the specified probability.

## Examples

``` r
iscaminvt(0.05, df = 15, direction = "below")

#> The observation with 0.05 probability below is -1.753 
iscaminvt(0.10, df = 25, direction = "above")

#> The observation with 0.1 probability above is 1.316 
iscaminvt(0.95, df = 30, direction = "between")

#> There is 0.95 probability between -2.042 and 2.042 
iscaminvt(0.05, df = 20, direction = "outside")

#> There is 0.05 probability outside -2.086 and 2.086 
```
