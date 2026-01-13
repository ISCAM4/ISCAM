# A dot plot

`dotplot` creates a horizontal dot plot. If a second categorical
variable is given, the data is grouped by this variable. Use `names` &
`mytitle` to specify the labels and title.

## Usage

``` r
iscamdotplot(
  response,
  explanatory = NULL,
  main = "",
  xlab = substitute(response),
  ylab = substitute(explanatory)
)
```

## Arguments

- response:

  Vector of numeric values to plot.

- explanatory:

  Optional second categorical variable to group by.

- main:

  Optional title for the plot

- xlab:

  Optional x-axis label for the plot

- ylab:

  Optional y-axis label for the plot. Only displayed when `explanatory`
  is provided.

## Value

A dot plot.

## Examples

``` r
iscamdotplot(
  mtcars$cyl,
  main = "mtcars Cylinders Dotplot",
  xlab = "Number of Cylinders"
)

iscamdotplot(
  mtcars$mpg,
  mtcars$am,
  main = "Automatic Cars Have Better Mileage on Average",
  xlab = "Mileage (miles per gallon)",
  ylab = "Automatic (yes coded as 1)"
)
```
