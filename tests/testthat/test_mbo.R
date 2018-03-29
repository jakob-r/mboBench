context("mbo benchmark")

test_that("mbo benchmark works", {
  skip_if_not_installed("mlrMBO")
  library(mlrMBO)
  benchmark = generateSimpleBenchmark(makeAckleyFunction(5))
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, more.termination.conds = benchmark$mlrmbo.termination.criterions)
  lrn = makeLearner("regr.lm", predict.type = "se")
  bench.res = lapply(1:2, function(i) {
    run = mbo(
      fun = benchmark$smoof.fun, 
      design = benchmark$getInitialDesignEvaluated(i, calculate = FALSE),
      control = ctrl,
      learner = lrn)
    expect_equal(run$final.state, "term.feval")
    expect_equal(getOptPathLength(run$opt.path), benchmark$termination.criterions$evals$vars$max.evals)
    BenchResult$new(
      id = NULL,
      benchmark = benchmark,
      opt.path = run$opt.path,
      repl = i
    )
  })
  res.list = bench.res
})