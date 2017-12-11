context("BenchExecutor")

test_that("BenchExecutor and BenchResult works", {
  benchmark = generateSimpleBenchmark(makeBraninFunction())
  benchmark2 = generateSimpleBenchmark(makeBraninFunction())
  benchmark3 = generateSimpleBenchmark(makeSwiler2014Function())
  expect_equal(benchmark$hash, benchmark2$hash)
  expect_true(benchmark$hash != benchmark3$hash)

  bench.function = function(benchmark, paramA, paramB) {
    random.design = generateRandomDesign(n = paramA+paramB, par.set = getParamSet(benchmark$smoof.fun))
    ys = evalDesign(random.design, benchmark$smoof.fun)[,1]
    op.dt = as.data.table(random.design)
    op.dt$y = ys
    BenchResult$new(benchmark = benchmark, op.dt = op.dt)
  }

  bench.function(benchmark, 1, 1)

  executor = BenchExecutor$new(id = "test.rs", executor.fun = bench.function, fixed.args = list(paramA = 10), benchmark = benchmark)
  res = executor$execute(paramB = 20)
  expect_class(res, "BenchResult")
  expect_true(res$values$executor$fixed.args$paramA == 10)

  expect_equal(benchmark$hash, res$benchmark.hash)
})