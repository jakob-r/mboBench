BenchReplVis = function(res.list, benchmark) {
  assertList(res.list, "BenchResult")
  values = map(res.list, "values")
  assert_true(all(sapply(tail(values, -1), all.equal, current = values[[1]])))
  assert_true(all(sapply(map_chr(res.list, "benchmark.hash"), all.equal, current = benchmark$hash)))
  #lapply(seq_along(res.list), function(i) res.list$)
}

if (FALSE) {
  library(purrr)
  library(checkmate)
  mydev()
  benchmark = generateSimpleBenchmark(makeBraninFunction())
  benchmark2 = generateSimpleBenchmark(makeSwiler2014Function())

  bench.function = function(benchmark, paramA, paramB) {
    random.design = generateRandomDesign(n = paramA+paramB, par.set = getParamSet(benchmark$smoof.fun))
    ys = evalDesign(random.design, benchmark$smoof.fun)[,1]
    op.dt = as.data.table(random.design)
    op.dt$y = ys
    BenchResult$new(benchmark = benchmark, op.dt = op.dt)
  }
  bench.exec = BenchExecutor$new(id = "test.rs", executor.fun = bench.function, fixed.args = list(paramA = 10))
  repls.bench1 = replicate(10, {
    bench.exec$execute(benchmark, paramB = 20)
  })
  repls.bench2 = replicate(10, {
    bench.exec$execute(benchmark2, paramB = 20)
  })

  res.list = repls.bench1
}
