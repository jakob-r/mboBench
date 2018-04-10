BenchMultiVis = function(res.list, benchmark) {

}

if (FALSE) {
  library(purrr)
  library(checkmate)
  mydev()
  benchmarkA = generateSimpleBenchmark(makeBraninFunction())
  benchmarkB = generateSimpleBenchmark(makeSwiler2014Function())

  bench.function = function(benchmark, paramA, paramB, repl) {
    random.design = generateRandomDesign(n = paramA+paramB, par.set = getParamSet(benchmark$smoof.fun))
    ys = evalDesign(random.design, benchmark$smoof.fun)[,1]
    op.dt = as.data.table(random.design)
    op.dt$y = ys
    op.dt = op.dt[order(ys, decreasing = benchmark$minimize)]
    op.dt = tail(op.dt, benchmark$termination.criterions$evals$vars$max.evals)
    BenchResult$new(benchmark = benchmark, op.dt = op.dt, repl = repl)
  }
  bench.exec = BenchExecutor$new(id = "test.rs", executor.fun = bench.function, fixed.args = list(paramA = 100))
  repls.benchA1 = Map(bench.exec$execute, benchmark = list(benchmarkA), paramB = 1000, repl = as.list(1:10))
  repls.benchA2 = Map(bench.exec$execute, benchmark = list(benchmarkA), paramB = 40, repl = as.list(1:10))
  repls.benchB1 = Map(bench.exec$execute, benchmark = list(benchmarkB), paramB = 40, repl = as.list(1:10))
  repls.benchB2 = Map(bench.exec$execute, benchmark = list(benchmarkB), paramB = 1000, repl = as.list(1:10))
  res.list = c(repls.benchA1, repls.benchA2, repls.benchB1, repls.benchB2)
  BenchReplVis(res.list = c(repls.benchB1, repls.benchB2), benchmark = benchmarkB)
}
