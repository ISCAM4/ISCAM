# One Proportion Z-Test and Interval

iscamonepropztest calculates a one-proportion z-test and/or a
corresponding confidence interval.

## Usage

``` r
iscamonepropztest(
  observed,
  n,
  hypothesized = NULL,
  alternative = "two.sided",
  conf.level = NULL,
  verbose = TRUE
)
```

## Arguments

- observed:

  The observed number of successes. If a value less than 1 is provided,
  it is assumed to be the sample proportion.

- n:

  The sample size.

- hypothesized:

  The hypothesized probability of success under the null hypothesis.
  This is an optional parameter.

- alternative:

  A character string specifying the form of the alternative hypothesis.
  Must be one of "less", "greater", or "two.sided". This is an optional
  parameter.

- conf.level:

  The confidence level(s) for a two-sided confidence interval. This is
  an optional parameter.

- verbose:

  Logical, defaults to `TRUE`. Set to `FALSE` to suppress messages

## Value

This function prints the results of the one-proportion z-test and/or the
confidence interval. It also generates plots to visualize the test and
interval.

## Examples

``` r
iscamonepropztest(observed = 35, n = 50, hypothesized = 0.5)
#> 
#> One Proportion z test
#> 
#> Data: observed successes = 35, sample size = 50, sample proportion = 0.7
#> 
#> Null hypothesis       : pi = 0.5 
#> Alternative hypothesis: pi <> 0.5 
#> z-statistic: 2.828 
#> p-value: 0.004678 


iscamonepropztest(
  observed = 0.8,
  n = 100,
  hypothesized = 0.75,
  alternative = "greater",
  conf.level = 0.95
)
#> 
#> One Proportion z test
#> 
#> Data: observed successes = 80, sample size = 100, sample proportion = 0.8
#> 
#> Null hypothesis       : pi = 0.75 
#> Alternative hypothesis: pi > 0.75 
#> z-statistic: 1.155 
#> p-value: 0.1241 

#> 95 % Confidence interval for pi: ( 0.7216014 ,  0.8783986 ) 

iscamonepropztest(observed = 60, n = 100, conf.level = 0.90)
#> 
#> One Proportion z test
#> 
#> Data: observed successes = 60, sample size = 100, sample proportion = 0.6
#> 
#> 90 % Confidence interval for pi: ( 0.519419 ,  0.680581 ) 
```
