context("mbo benchmark")

test_that("mbo benchmark works", {
  skip_if_not_installed("mlrMBO")
  library(mlrMBO)
  benchmark = generateSimpleBenchmark(makeBraninFunction())
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, more.termination.conds = benchmark$mlrmbo.termination.criterions)
  run = mbo(
    fun = benchmark$smoof.fun, 
    design = benchmark$getInitialDesignEvaluated(1, calculate = FALSE),
    control = ctrl)
})