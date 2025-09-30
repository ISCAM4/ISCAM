set.seed(0)
fake_data <- rnorm(30)
groups <- sample(c("group1", "group2"), 30, TRUE)
raw_summary <- data.frame(
  Missing = sum(is.na(fake_data)),
  n = length(fake_data),
  Min = min(fake_data),
  Q1 = quantile(fake_data, probs = 0.25),
  Median = median(fake_data),
  Q3 = quantile(fake_data, probs = 0.75),
  Max = max(fake_data),
  Mean = mean(fake_data),
  SD = sd(fake_data),
  Skewness = sum((fake_data - mean(fake_data))^3) /
    sum((fake_data - mean(fake_data))^2)^1.5 *
    length(fake_data)^0.5,
  row.names = NULL
)


test_that("summary without explanatory works", {
  expect_equal(
    iscamsummary(fake_data),
    raw_summary |> round(3)
  )

  expect_equal(
    iscamsummary(fake_data, digits = 5),
    raw_summary |> round(5)
  )
})

test_that("summary with explanatory works", {
  group1 <- fake_data[groups == "group1"]
  group2 <- fake_data[groups == "group2"]

  expect_equal(
    iscamsummary(fake_data, groups),
    rbind(iscamsummary(group1), iscamsummary(group2)) |>
      structure(row.names = c("group1", "group2"))
  )
  groups <- sample(c("group1", "group2", "group3"), 30, TRUE)
  group1 <- fake_data[groups == "group1"]
  group2 <- fake_data[groups == "group2"]
  group3 <- fake_data[groups == "group3"]

  expect_equal(
    iscamsummary(fake_data, groups),
    rbind(iscamsummary(group1), iscamsummary(group2), iscamsummary(group3)) |>
      structure(row.names = c("group1", "group2", "group3"))
  )
})
