context("BenchReplVis and BenchMultiVis")

test_that("BenchReplVis and BenchMultiVis works", {
  benchmark1 = generateSimpleBenchmark(makeBraninFunction())
  benchmark2 = generateSimpleBenchmark(makeSwiler2014Function())

  benchFun1 = function(benchmark, repl) {
    random.design = generateRandomDesign(n = benchmark$termination.criterions$evals$vars$max.evals, par.set = getParamSet(benchmark$smoof.fun))
    ys = evalDesign(random.design, benchmark$smoof.fun)[,1]
    op.dt = as.data.table(random.design)
    op.dt$y = ys
    BenchResult$new(benchmark = benchmark, op.dt = op.dt, repl = repl)
  }

  benchFun2 = function(benchmark, repl) {
    lhs.design = generateDesign(n = benchmark$termination.criterions$evals$vars$max.evals, par.set = getParamSet(benchmark$smoof.fun))
    ys = evalDesign(lhs.design, benchmark$smoof.fun)[,1]
    op.dt = as.data.table(lhs.design)
    op.dt$y = ys
    BenchResult$new(benchmark = benchmark, op.dt = op.dt, repl = repl)
  }

  executor1 = BenchExecutor$new(id = "random", benchFun1)
  executor2 = BenchExecutor$new(id = "lhs", benchFun2)

  repls.bench11 = lapply(1:10, executor1$execute, benchmark = benchmark1)
  repls.bench12 = lapply(1:10, executor1$execute, benchmark = benchmark2)
  repls.bench21 = lapply(1:10, executor2$execute, benchmark = benchmark1)
  repls.bench22 = lapply(1:10, executor2$execute, benchmark = benchmark2)
  
  brv = BenchReplVis$new(res.list = repls.bench11, benchmark = benchmark1)
  brv$plot_opt_path_progress()
  brv$plot_threshold_progress()

  bmv = BenchMultiVis$new(res.list = c(repls.bench11, repls.bench12, repls.bench21, repls.bench22), benchmarks = c(benchmark1, benchmark2))
  bmv$plot_threshold_progress()
  bmv$plot_opt_path_progress()

})
