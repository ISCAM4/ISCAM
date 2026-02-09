test_that("every exported iscam function is callable without errors", {
  set.seed(2026)

  smoke_cases <- list(
    iscamaddexp = quote(iscamaddexp(rexp(60, rate = 0.5))),
    iscamaddlnorm = quote(iscamaddlnorm(rlnorm(60))),
    iscamaddnorm = quote(iscamaddnorm(rnorm(60))),
    iscamaddt = quote(iscamaddt(rt(60, df = 12), df = 12)),
    iscamaddtnorm = quote(iscamaddtnorm(rt(60, df = 6), df = 6)),
    iscambinomnorm = quote(iscambinomnorm(
      10,
      20,
      0.5,
      "below",
      verbose = FALSE
    )),
    iscambinompower = quote(iscambinompower(
      LOS = 0.05,
      n = 20,
      prob1 = 0.5,
      alternative = "greater",
      prob2 = 0.6,
      verbose = FALSE
    )),
    iscambinomprob = quote(iscambinomprob(5, 20, 0.4, TRUE, verbose = FALSE)),
    iscambinomtest = quote(iscambinomtest(
      observed = 12,
      n = 20,
      hypothesized = 0.5,
      alternative = "two.sided",
      conf.level = 0.95,
      verbose = FALSE
    )),
    iscamboxplot = quote(iscamboxplot(mtcars$mpg)),
    iscamchisqprob = quote(iscamchisqprob(5, 3, verbose = FALSE)),
    iscamdotplot = quote(iscamdotplot(mtcars$cyl)),
    iscamhypernorm = quote(iscamhypernorm(1, 20, 5, 10, TRUE, verbose = FALSE)),
    iscamhyperprob = quote(iscamhyperprob(1, 20, 5, 10, TRUE, verbose = FALSE)),
    iscaminvbinom = quote(iscaminvbinom(0.05, 30, 0.5, TRUE, verbose = FALSE)),
    iscaminvnorm = quote(iscaminvnorm(
      0.05,
      direction = "below",
      verbose = FALSE
    )),
    iscaminvt = quote(iscaminvt(
      0.05,
      df = 15,
      direction = "below",
      verbose = FALSE
    )),
    iscamnormpower = quote(iscamnormpower(
      LOS = 0.05,
      n = 100,
      prob1 = 0.5,
      alternative = "greater",
      prob2 = 0.6,
      verbose = FALSE
    )),
    iscamnormprob = quote(iscamnormprob(
      1.96,
      direction = "above",
      verbose = FALSE
    )),
    iscamonepropztest = quote(iscamonepropztest(
      observed = 35,
      n = 50,
      hypothesized = 0.5,
      alternative = "greater",
      conf.level = 0.95,
      verbose = FALSE
    )),
    iscamonesamplet = quote(iscamonesamplet(
      xbar = 2.5,
      sd = 1.2,
      n = 30,
      hypothesized = 2,
      alternative = "greater",
      conf.level = 0.95,
      verbose = FALSE
    )),
    iscamsummary = quote(iscamsummary(c(4, 8, 15, 16, 23, 42))),
    iscamtprob = quote(iscamtprob(
      1.8,
      df = 20,
      direction = "above",
      verbose = FALSE
    )),
    iscamtwopropztest = quote(iscamtwopropztest(
      observed1 = 35,
      n1 = 50,
      observed2 = 28,
      n2 = 45,
      hypothesized = 0,
      alternative = "greater",
      conf.level = 0.95,
      verbose = FALSE
    )),
    iscamtwosamplet = quote(iscamtwosamplet(
      x1 = 25,
      sd1 = 5,
      n1 = 40,
      x2 = 22,
      sd2 = 6,
      n2 = 45,
      hypothesized = 0,
      alternative = "greater",
      conf.level = 0,
      verbose = FALSE
    ))
  )

  exported <- getNamespaceExports("ISCAM")
  exported_iscam <- exported[grepl("^iscam", exported)]
  expect_setequal(names(smoke_cases), exported_iscam)

  for (fn in names(smoke_cases)) {
    expect_no_error(
      capture_plot_result(suppressWarnings(eval(smoke_cases[[fn]])))
    )
  }
})
