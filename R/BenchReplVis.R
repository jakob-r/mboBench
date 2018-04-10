BenchReplVis = function(res.list, benchmark) {
  res = aggregateBenchRepls(res.list, benchmark)
  g1 = ggplot(res$op.dt, aes_string(x = "dob", y = "y.dob.c", group = "algo.name.config", color = "algo.name.config", fill = "algo.name.config"))
  g1 = g1 + geom_point(size = 0.5, alpha = 0.2)
  g1 = g1 + geom_hline(data = data.frame(value = benchmark$threasholds, threasholds = names(benchmark$threasholds)), mapping = aes(yintercept = value), alpha = 0.2)
  g1 = g1 + stat_summary(fun.y = median, geom="line")
  g1 = g1 + stat_summary(fun.ymin = partial(quantile, probs = 0.1), geom="ribbon", fun.ymax = partial(quantile, probs = 0.9), alpha = 0.1, color = NA)

  g2 = ggplot(res$threasholds.dt, aes(x = y.th, y = dob, fill = algo.name.config))
  g2 = g2 + geom_boxplot()
  g2 = g2 + geom_hline(yintercept = max(res$op.dt$dob))
  g2 = g2 + coord_cartesian(ylim=c(min(res$threasholds.dt$dob), max(res$op.dt$dob)))

  list(g1, g2)
}

if (FALSE) {
  library(purrr)
  library(checkmate)
  mydev()
  benchmarkA = generateSimpleBenchmark(makeBraninFunction())

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
  res.list = c(repls.benchA1, repls.benchA2)
  BenchReplVis(res.list, benchmarkA)
  # res.list = c(repls.bench1, repls.bench2)
}
