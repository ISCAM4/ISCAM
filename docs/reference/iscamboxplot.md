# A box plot

`boxplot` plots the given data in a box plot. If a second categorical
variable is given, the data is grouped by this variable.

## Usage

``` r
iscamboxplot(
  response,
  explanatory = NULL,
  main = "",
  xlab = "",
  ylab = substitute(explanatory)
)
```

## Arguments

- response:

  Vector of numeric values to plot.

- explanatory:

  (optional) second categorical variable to group by.

- main:

  (optional) title for the plot.

- xlab:

  (optional) x-axis label for the plot.

- ylab:

  (optional) y-axis label for the plot. Only displayed when
  `explanatory` is provided.

## Value

A box plot.

## Examples

``` r
iscamboxplot(
  mtcars$mpg,
  main = "mtcars Cylinders Dotplot",
  xlab = "Number of Cylinders"
)

iscamboxplot(
  mtcars$mpg,
  mtcars$am,
  main = "Automatic Cars Have Better Mileage on Average",
  xlab = "Mileage (miles per gallon)",
  ylab = "Automatic (yes coded as 1)"
)
```
