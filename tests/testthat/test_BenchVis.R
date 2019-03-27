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

  repls.bench1 = replicate(10, {
    executor1$execute(benchmark1)
  })
  repls.bench2 = replicate(10, {
    executor2$execute(benchmark2)
  })
  brv = BenchReplVis$new(res.list = repls.bench1, benchmark = benchmark1)
  brv$plot_opt_path_progress()
  brv$plot_threshold_progress()
})
