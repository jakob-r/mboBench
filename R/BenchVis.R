BenchReplVis = function(res.list, benchmark) {
  assertList(res.list, "BenchResult")
  assert_true(all(sapply(map_chr(res.list, "benchmark.hash"), all.equal, current = benchmark$hash)))
  repls = map_int(res.list, "repl")

  # check if replications make sense
  assert_true(length(unique(table(repls))) == 1)
  assert_true(mean(repls == 1) != 1)

  # generate list of repls with opt paths
  res.dt = Map(cbind.data.frame, map(res.list, "op.dt"), repl = repls)

  # add algo.name to each opt path
  if (!is.null(res.list[[1]]$algo.name)) {
    res.dt = Map(cbind, res.dt, algo.name = map_chr(res.list, "algo.name"))
  } else {
    res.dt = Map(cbind, res.dt, algo.name = "")
  }

  # add algo param to each opt path
  if (!is.null(res.list[[1]]$algo.params)) {
    algo.param = lapply(res.list, function(x) setNames(as.data.frame(x$algo.params), paste0("algo.param.", names(x$algo.params))))
    algo.name.post = map_chr(res.list, function(x) paste0(names(x$algo.params), "=", x$algo.params, collapse = " "))
    res.dt = Map(cbind, res.dt, algo.param, algo.name.post = algo.name.post)
  }

  # combine opt paths to one data.table
  res.dt = rbindlist(res.dt, fill = TRUE)

  # add DOB if missing
  if (is.null(res.dt$dob)) {
    res.dt[, dob := seq_len(.N), by = c("repl", "algo.name", "algo.name.post")]
  }

  res.dt[, algo.name.config := paste(algo.name, algo.name.post)]

  threasholds = benchmark$threasholds
  if (benchmark$minimize) {
    res.dt[, y.dob := min(y), by = c("algo.name.config", "repl", "dob")]
    setkeyv(res.dt, c("algo.name.config", "repl", "dob"))
    res.dt[, y.dob.c := cummin(y.dob), by = c("algo.name.config", "repl")]
    res.dt[, y.th := cut(y.dob.c, c(Inf, threasholds, -Inf))]
    if ("opt" %in% names(threasholds)) {
      levels(res.dt$y.th) = c("<NA>", rev(names(threasholds))[-1], "<worse>")
    } else {
      levels(res.dt$y.th) = c(rev(names(threasholds)), "<worse>")
    }
    res.dt[, y.th := ordered(y.th, levels = rev(levels(y.th)))]
  } else {
    res.dt[, y.dob := max(y), by = c("algo.name.config", "repl",  "dob")]
    res.dt[, y.dob.c := cummax(y.dob), by = c("algo.name.config", "repl")]
    stop("Not implemented!")
  }

  g1 = ggplot(res.dt, aes_string(x = "dob", y = "y.dob.c", group = "algo.name.config", color = "algo.name.config", fill = "algo.name.config"))
  g1 = g1 + geom_point(size = 0.5, alpha = 0.2)
  g1 = g1 + geom_hline(data = data.frame(value = benchmark$threasholds, threasholds = names(benchmark$threasholds)), mapping = aes(yintercept = value), alpha = 0.2)
  g1 = g1 + stat_summary(fun.y = median, geom="line")
  g1 = g1 + stat_summary(fun.ymin = partial(quantile, probs = 0.1), geom="ribbon", fun.ymax = partial(quantile, probs = 0.9), alpha = 0.1, color = NA)
  g1

  getUsableThr = function(x) {
    ordered(x, levels = setdiff(levels(x), c("<NA>", "<worse>")))
  }

  level.dt = CJ(algo.name.config = unique(res.dt$algo.name.config), repl = unique(res.dt$repl), y.th.i = seq_along(levels(getUsableThr(res.dt$y.th))))
  res.dt[, y.th.i := as.integer(getUsableThr(y.th))]
  setkeyv(res.dt, c("algo.name.config", "repl", "y.th.i"))
  setkeyv(level.dt, c("algo.name.config", "repl", "y.th.i"))
  res.dt2 = res.dt[level.dt, roll = -Inf]

  res.dt2[, y.th := ordered(y.th.i, labels = levels(getUsableThr(res.dt2$y.th)))]
  res.dt2[is.na(dob), dob := as.integer(ceiling(max(res.dt$dob) * 2))]
  res.dt2 = res.dt2[, .SD[dob == max(dob),], by = c("algo.name.config", "repl", "y.th")]
  g2 = ggplot(res.dt2, aes(x = y.th, y = dob, fill = algo.name.config))
  g2 = g2 + geom_boxplot()
  g2 = g2 + geom_hline(yintercept = max(res.dt$dob))
  g2 = g2 + coord_cartesian(ylim=c(min(res.dt2$dob), max(res.dt$dob)))
  g2
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
  repls.benchB1 = Map(bench.exec$execute, benchmark = list(benchmarkB), paramB = 30, repl = as.list(1:10))
  res.list = c(repls.benchA1, repls.benchA2)
  BenchReplVis(res.list, benchmarkA)
  # res.list = c(repls.bench1, repls.bench2)
}
