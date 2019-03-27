#' @title Visualize and Evaluate multiple Benchmarks
#'
#' @description
#' Visualize and Evaluate multiple Benchmarks
#'
#' @param res.list [\code{list}]\cr
#'   List of BenchmarkResult Objects
#' @param benchmarks [\code{list}]\cr
#'   List of Benchmarks.
#'   Will be matched with res.list entries.
#' @return list of ggplots and tables
#' @export  
BenchMultiVis = function(res.list, benchmarks) {
  assertList(res.list, types = "BenchResult", any.missing = FALSE)
  assertList(benchmarks, types = "Benchmark", any.missing = FALSE)
  bench.tab = data.table(
    bench.hash = map_chr(benchmarks, "hash"),
    bench.id = map_chr(benchmarks, "id"),
    bench.max.evals = map_dbl(benchmarks, c("termination.criterions", "evals", "vars", "max.evals"))
  )
  #make sure bench ids are unique!
  bench.tab[, bench.id := if (.N>1) paste0(bench.id, seq_len(.N)) else bench.id, by = .(bench.hash)]
  res.tab = data.table(
    bench.hash = map_chr(res.list, "benchmark.hash"),
    bench.no = seq_along(res.list)
  )

  # helper fun
  benchmarkById = function(bench.id) {
    benchmarks[bench.tab$bench.id == bench.id][[1]]
  }

  res.tab = merge(bench.tab, res.tab)
  res.all = res.tab[, {
    this.benchmark = benchmarkById(bench.id[1])
    agg.res = aggregateBenchRepls(res.list = res.list[bench.no], benchmark = this.benchmark)
    lapply(agg.res, list)
    }, by = c("bench.id")]

  # unify thresholds
  for (i in seq_len(nrow(res.all))) {
    new_levels = paste0(seq(0, 100, by = 10),"%")
    if ("<NA>" == tail(levels(res.all$op.dt[[i]]$y.th), 1)) {
      new_levels_2 = c(new_levels, "<NA>")
    } else {
      new_levels_2 = new_levels
    }
    res.all$op.dt[[i]][, y.th := lvls_revalue(res.all$op.dt[[i]]$y.th, new_levels_2)]
    res.all$thresholds.dt[[i]][, y.th := lvls_revalue(res.all$thresholds.dt[[i]]$y.th, tail(new_levels, -1))]
  }

  # remove x columns
  for (i in seq_len(nrow(res.all))) {
    x.ids = benchmarkById(res.all$bench.id[i])$x.ids
    res.all$op.dt[[i]][, (x.ids) := NULL]
    res.all$thresholds.dt[[i]][, (x.ids) := NULL]
  }

  # put bench id in sub tables
  if (seq_len(nrow(res.all)) == 1) { #FIXME: This is a workaraound for a data.table bug
    op = copy(res.all$op.dt[[1]])
    op[, bench.id := res.all$bench.id[1]]
    res.all$op.dt[[1]] = list(op)
    t.dt = copy(res.all$thresholds.dt[[1]])
    t.dt[, bench.id := res.all$bench.id[1]]
    res.all$thresholds.dt[[1]] = list(t.dt)
  } else {
    for (i in seq_len(nrow(res.all))) {
      res.all$op.dt = Map(cbind, res.all$op.dt, bench.id = res.all$bench.id)
      res.all$thresholds.dt = Map(cbind, res.all$thresholds.dt, bench.id = res.all$bench.id)
    } 
  }
  
  
  # merge to one dt
  op.dt = merge(bench.tab, rbindlist(res.all$op.dt, fill = TRUE))
  thresholds.dt = merge(bench.tab, rbindlist(res.all$thresholds.dt, fill = TRUE))

  # unify dob to progress
  #not needed atm
  #op.dt[, dob.progress := dob / max(dob)]
  thresholds.dt[, nev.progress := nev / bench.max.evals]

  # plot opt path progress
  g1 = ggplot(op.dt, aes_string(x = "dob", y = "y.dob.c", group = "algo.name.config", color = "algo.name.config", fill = "algo.name.config"))
  g1 = g1 + geom_point(size = 0.5, alpha = 0.2)
  g1 = g1 + stat_summary(fun.y = median, geom="line")
  g1 = g1 + stat_summary(fun.ymin = partial(quantile, probs = 0.1), geom="ribbon", fun.ymax = partial(quantile, probs = 0.9), alpha = 0.1, color = NA)
  g1 = g1 + facet_wrap(~bench.id, scales = "free")

  # plot threshold progress
  g2 = ggplot(thresholds.dt, aes(x = y.th, y = nev.progress, fill = algo.name.config))
  g2 = g2 + geom_boxplot()
  g2 = g2 + geom_hline(yintercept = 1)
  g2 = g2 + coord_cartesian(ylim=c(min(thresholds.dt$nev.progress), 1))

  ## Tables

  thresholds.dt[, rank.prog := rank(nev.progress), by = .(bench.id, repl, y.th)]
  # ranked performance, aggregates over replications
  t1 = thresholds.dt[, mean(rank.prog), by = .(bench.id, algo.name.config, y.th)]
  # aggregates over all thresholds
  t2 = thresholds.dt[, mean(rank.prog), by = .(bench.id, algo.name.config)]
  # aggregates over benchmarks
  t3 = thresholds.dt[, mean(rank.prog), by = .(algo.name.config, y.th)]
  #dcast(t3, algo.name.config~y.th, value.var = "V1")

  list(plot.progress = g1, plot.threshold = g2, table.run = t1, table.all.thr.ave = t2, table.all.benchs = t3, raw.dt = thresholds.dt)
}

if (FALSE) {
  library(purrr)
  library(checkmate)
  mydev()
  benchmarkA = generateSimpleBenchmark(makeBraninFunction())
  #benchmarkB = generateSimpleBenchmark(makeSwiler2014Function())
  #funB = makeRosenbrockFunction(7)
  funB = makeSingleObjectiveFunction(name = "test1", fn = function(x) x[[1]]^2 + sin(x[[2]]), par.set = makeNumericParamSet("x", 2, -3, 3))
  benchmarkB = generateSimpleBenchmark(funB)

  bench.cmaes = function(benchmark, repl, design, sigma = 1.5, lambda = 40) {
    fun = smoof::addLoggingWrapper(benchmark$smoof.fun, logg.x = TRUE, logg.y = TRUE)
    des = benchmark$getInitialDesignEvaluated(repl)
    start.point = unlist(des[des$y == benchmark$minmax(des$y), benchmark$x.ids, drop = TRUE])
    res = cmaesr::cmaes(
      objective.fun = fun,
      start.point = start.point,
      control = list(
        log.population = TRUE,
        sigma = sigma,
        lambda = lambda,
        stop.ons =list(
          cmaesr::stopOnMaxEvals(benchmark$termination.criterions$evals$vars$max.evals)#,
          #cmaesr::stopOnOptValue(benchmark$termination.criterions$termination.value$vars$best.y.value, tol = benchmark$termination.criterions$termination.value$vars$tol)
        )
      )
    )
    logged.values = getLoggedValues(fun)
    op.dt = cbind(logged.values$pars, y = logged.values$obj.vals)
    op.dt = head(op.dt, benchmark$termination.criterions$evals$vars$max.evals)
    BenchResult$new(benchmark = benchmark, op.dt = as.data.table(op.dt), repl = repl)
  }
  bench.optim = function(benchmark, repl) {
    fun = smoof::addLoggingWrapper(benchmark$smoof.fun, logg.x = TRUE, logg.y = TRUE)
    des = benchmark$getInitialDesignEvaluated(repl)
    start.point = unlist(des[des$y == benchmark$minmax(des$y), benchmark$x.ids, drop = TRUE])
    res = optim(par = start.point, fn = fun, control = list(maxit = benchmark$termination.criterions$evals$vars$max.evals))
    logged.values = getLoggedValues(fun)
    op.dt = cbind(logged.values$pars, y = logged.values$obj.vals)
    op.dt = head(op.dt, benchmark$termination.criterions$evals$vars$max.evals)
    BenchResult$new(benchmark = benchmark, op.dt = as.data.table(op.dt), repl = repl)
  }
  bench.e.cmaes = BenchExecutor$new(id = "cmaesr", executor.fun = bench.cmaes, fixed.args = list(sigma = 1.5))
  bench.e.optim = BenchExecutor$new(id = "optim", executor.fun = bench.optim)
  repls.benchA1 = Map(bench.e.cmaes$execute, benchmark = list(benchmarkA), lambda = 20, repl = as.list(1:10))
  repls.benchA2 = Map(bench.e.optim$execute, benchmark = list(benchmarkA), repl = as.list(1:10))
  repls.benchB1 = Map(bench.e.cmaes$execute, benchmark = list(benchmarkB), lambda = 20, repl = as.list(1:10))
  repls.benchB2 = Map(bench.e.optim$execute, benchmark = list(benchmarkB), repl = as.list(1:10))
  res.list = c(repls.benchA1, repls.benchA2, repls.benchB1, repls.benchB2)
  benchmarks = list(benchmarkB, benchmarkA)
  BenchReplVis(res.list = c(repls.benchB1, repls.benchB2), benchmark = benchmarkB)
  BenchReplVis(res.list = c(repls.benchA1, repls.benchA2), benchmark = benchmarkA)
  res.list = c(repls.benchA1, repls.benchA2, repls.benchB1, repls.benchB2)
  BenchMultiVis(res.list, benchmarks = benchmarks)
}
