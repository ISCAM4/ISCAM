# Some Summary Statistics

`summary` calculates the five number summary, mean, and standard
deviation of the quantitative variable `x`. An optional second,
categorical variable can be specified and values will be calculated
separately for each group. The number of digits in output can also be
specified. Skewness is sample skewness: \\g_1 :=
\frac{m_3}{m_2^{3/2}}\\, where \\m_2 := \frac{1}{n}\sum\_{i=1}^{n}(x_i -
\bar{x})^2\\ and \\m_3 := \frac{1}{n}\sum\_{i=1}^{n}(x_i - \bar{x})^3\\
are the second and third central sample moments.

## Usage

``` r
iscamsummary(x, explanatory = NULL, digits = 3)
```

## Arguments

- x:

  data to summarize.

- explanatory:

  (optional) explanatory variable to group by.

- digits:

  (optional) number of digits to round to, defaults to 3.

## Value

A table with some summary statistics of `x`.

## Examples

``` r
set.seed(0)
fake_data <- rnorm(30) # simulating some data
groups <- sample(c("group1","group2"), 30, TRUE)
iscamsummary(fake_data)
#>   Missing  n   Min     Q1 Median    Q3   Max  Mean    SD Skewness
#> 1       0 30 -1.54 -0.621 -0.031 0.487 2.405 0.022 0.914    0.441
iscamsummary(fake_data, explanatory = groups, digits = 2) # with groups
#>        Missing  n   Min    Q1 Median   Q3  Max  Mean   SD Skewness
#> group1       0 13 -1.24 -0.33  -0.06 0.44 1.26 -0.02 0.65     0.21
#> group2       0 17 -1.54 -0.89   0.13 0.76 2.40  0.05 1.09     0.38
```
