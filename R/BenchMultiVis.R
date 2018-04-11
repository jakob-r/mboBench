BenchMultiVis = function(res.list, benchmarks) {
  bench.tab = data.table(
    bench.hash = map_chr(benchmarks, "hash"),
    bench.id = map_chr(benchmarks, "id"),
    bench.max.evals = map_dbl(benchmarks, c("termination.criterions", "evals", "vars", "max.evals"))
  )
  res.tab = data.table(
    bench.hash = map_chr(res.list, "benchmark.hash"),
    bench.no = seq_along(res.list)
  )

  # helper fun
  benchmarkByHash = function(bench.hash) {
    benchmarks[bench.tab$bench.hash == bench.hash][[1]]
  }

  res.tab = merge(bench.tab, res.tab)
  res.all = res.tab[, {
    this.benchmark = benchmarkByHash(bench.hash[1])
    agg.res = aggregateBenchRepls(res.list = res.list[bench.no], benchmark = this.benchmark)
    lapply(agg.res, list)
    }, by = c("bench.hash")]

  # unify threasholds
  for (i in seq_len(nrow(res.all))) {
    levels(res.all$op.dt[[i]]$y.th) = c(paste0(seq(0, 100, by = 10),"%"), "<NA>")
    levels(res.all$threasholds.dt[[i]]$y.th) = c(paste0(seq(10, 100, by = 10),"%"))
  }

  # remove x columns
  for (i in seq_len(nrow(res.all))) {
    x.ids = benchmarkByHash(res.all$bench.hash[i])$x.ids
    res.all$op.dt[[i]][, (x.ids) := NULL]
    res.all$threasholds.dt[[i]][, (x.ids) := NULL]
  }

  # put bench hash in sub tables
  res.all$op.dt = Map(cbind, res.all$op.dt, bench.hash = res.all$bench.hash)
  res.all$threasholds.dt = Map(cbind, res.all$threasholds.dt, bench.hash = res.all$bench.hash)

  # merge to one dt
  op.dt = merge(bench.tab, rbindlist(res.all$op.dt, fill = TRUE))
  threasholds.dt = merge(bench.tab, rbindlist(res.all$threasholds.dt, fill = TRUE))

  # unify dob to progress
  #not needed atm
  #op.dt[, dob.progress := dob / max(dob)]
  threasholds.dt[, nev.progress := nev / bench.max.evals]

  # plot opt path progress
  g1 = ggplot(op.dt, aes_string(x = "dob", y = "y.dob.c", group = "algo.name.config", color = "algo.name.config", fill = "algo.name.config"))
  g1 = g1 + geom_point(size = 0.5, alpha = 0.2)
  g1 = g1 + stat_summary(fun.y = median, geom="line")
  g1 = g1 + stat_summary(fun.ymin = partial(quantile, probs = 0.1), geom="ribbon", fun.ymax = partial(quantile, probs = 0.9), alpha = 0.1, color = NA)
  g1 = g1 + facet_wrap(~bench.hash, scales = "free")

  # plot threashold progress
  g2 = ggplot(threasholds.dt, aes(x = y.th, y = nev.progress, fill = algo.name.config))
  g2 = g2 + geom_boxplot()
  g2 = g2 + geom_hline(yintercept = 1)
  g2 = g2 + coord_cartesian(ylim=c(min(threasholds.dt$nev.progress), 1))

  list(g1, g2)
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
  benchmarks = list(benchmarkB, benchmarkA)
  BenchReplVis(res.list = c(repls.benchB1, repls.benchB2), benchmark = benchmarkB)
  res = aggregateBenchRepls(res.list = c(repls.benchB1, repls.benchB2), benchmark = benchmarkB)
  res$op.dt[y == min(y)]
  benchmarkB$threasholds
}
