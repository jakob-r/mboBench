BenchReplVis = function(res.list, benchmark) {
  assertList(res.list, "BenchResult")
  values = map(res.list, "values")
  assert_true(all(sapply(map_chr(res.list, "benchmark.hash"), all.equal, current = benchmark$hash)))
  repls = map_int(res.list, "repl")
  assert_true(!anyDuplicated(repls))
  assert_true(all(sapply(map_chr(res.list, "benchmark.hash"), all.equal, current = benchmark$hash)))
  res.dt = Map(cbind.data.frame, map(res.list, "op.dt"), repl = repls)
  res.dt = rbindlist(res.dt)
  threasholds = benchmark$threasholds
  if (benchmark$minimize) {
    res.dt[, y.dob := min(y), by = .(dob, repl)]
    res.dt[, y.dob.c := cummin(y.dob), by = .(repl)]
    res.dt[, y.th := cut(y.dob.c, c(Inf, threasholds, -Inf))]
    if ("opt" %in% names(threasholds)) {
      levels(res.dt$y.th) = c("<NA>", rev(names(threasholds))[-1], "<worse>")
    } else {
      levels(res.dt$y.th) = c(rev(names(threasholds)), "<worse>")
    }
  }
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
