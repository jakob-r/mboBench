context("Benchmark")

test_that("Benchmark works", {
  benchmark = Benchmark$new(
    smoof.fun = makeBraninFunction(),
    termination.criterions = list(eval = TerminationEvals$new(max.evals = 10)),
    thresholds = c(50, 25, 10, 0.3979),
    initial.design.n = 10,
    tags = c("test")
  )

  des = benchmark$getInitialDesign(1)
  expect_data_frame(des)

  benchmark$evaluateDesign(2)
  des = benchmark$getInitialDesign(2)
  expect_data_frame(des)

  des = benchmark$getInitialDesignEvaluated(10)
  expect_data_frame(des)

  expect_character(benchmark$hash)

  benchmark$mlrmbo.termination.criterions
})