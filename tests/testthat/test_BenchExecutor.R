context("BenchExecutor")

test_that("BenchExecutor and BenchResult works", {
  benchmark = generateSimpleBenchmark(makeBraninFunction())
  benchmark2 = generateSimpleBenchmark(makeSwiler2014Function())
  benchmark3 = generateSimpleBenchmark(makeBraninFunction())
  expect_equal(benchmark$hash, benchmark3$hash)
  expect_true(benchmark$hash != benchmark2$hash)

  bench.function = function(benchmark, paramA, paramB, repl) {
    random.design = generateRandomDesign(n = paramA+paramB, par.set = getParamSet(benchmark$smoof.fun))
    ys = evalDesign(random.design, benchmark$smoof.fun)[,1]
    op.dt = as.data.table(random.design)
    op.dt$y = ys
    op.dt = tail(op.dt, benchmark$termination.criterions$evals$vars$max.evals)
    BenchResult$new(benchmark = benchmark, op.dt = op.dt, repl = repl)
  }

  res = bench.function(benchmark, 1, 1, 1)
  expect_class(res, "BenchResult")

  bench.exec = BenchExecutor$new(id = "test.rs", executor.fun = bench.function, fixed.args = list(paramA = 10))
  repls.bench1 = replicate(10, {
    bench.exec$execute(benchmark, paramB = 20)
  })
  repls.bench2 = replicate(10, {
    bench.exec$execute(benchmark2, paramB = 20)
  })
  expect_class(repls.bench1[[1]], "BenchResult")
  expect_true(repls.bench1[[1]]$algo.params$paramA == 10)

  expect_equal(benchmark$hash, repls.bench1[[1]]$benchmark.hash)

  expect_data_table(repls.bench1[[1]]$threshold.performances)
  expect_numeric(repls.bench1[[1]]$threshold.auc)
})
