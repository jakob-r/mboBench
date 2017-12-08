context("BenchExecutor")

test_that("BenchExecutor and BenchResult works", {
  benchmark = generateSimpleBenchmark(makeBraninFunction())

  bench.function = function(benchmark, paramA, paramB) {
    random.design = generateRandomDesign(n = paramA+paramB, par.set = getParamSet(benchmark$smoof.fun))
    ys = evalDesign(random.design, benchmark$smoof.fun)[,1]
    op.dt = as.data.table(random.design)
    op.dt$y = ys
    res = BenchResult$new(benchmark = benchmark, op.dt = op.dt)
  }

  bench.function(benchmark, 1, 1)

  executor = BenchExecutor$new(id = "test.rs", executor.fun = bench.function, fixed.args = list(paramA = 10), benchmark = benchmark)
  res = executor$execute(paramB = 20)
  expect_class(res, "BenchResult")
  expect_true(res$values$executor$fixed.args$paramA == 10)
})