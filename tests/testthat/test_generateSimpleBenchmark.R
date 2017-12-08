context("generateSimpleBenchmark")

test_that("generateSimpleBenchmark works", {
  benchmark = generateSimpleBenchmark(makeBraninFunction())

  des = benchmark$getInitialDesign(1)
  expect_data_frame(des)

  benchmark$evaluateDesign(2)
  des = benchmark$getInitialDesign(2)
  expect_data_frame(des)

  des = benchmark$getInitialDesignEvaluated(10)
  expect_data_frame(des)

  benchmark$mlrmbo.termination.criterions
})