BenchReplVis = function(res.list, benchmark) {
  assertList(res.list, "BenchResult")
  assert_true(all(sapply(map_chr(res.list, "benchmark.hash"), all.equal, current = benchmark$hash)))
  repls = map_int(res.list, "repl")
  assert_true(!anyDuplicated(repls))

  # generate list of repls with opt paths
  res.dt = Map(cbind.data.frame, map(res.list, "op.dt"), repl = repls)

  # add algo.name to each opt path
  if (!is.null(res.list[[1]]$algo.name)) {
    Map(cbind, res.dt, algo.name = map_chr(res.list, "algo.name"))
  }

  # add algo param to each opt path
  if (!is.null(res.list[[1]]$algo.params)) {
    algo.param = lapply(res.list, function(x) setNames(as.data.frame(x$algo.params), paste0("algo.param.", names(x$algo.params))))
    Map(cbind, res.dt, algo.param)
  }

  # combine opt paths to one data.table
  res.dt = rbindlist(res.dt, fill = TRUE)

  # add DOB if missing
  if (is.null(res.dt$dob)) {
    res.dt[, dob := seq_len(.N), by = "repl"]
  }

  threasholds = benchmark$threasholds
  if (benchmark$minimize) {
    res.dt[, y.dob := min(y), by = .(dob, repl)]
    setkeyv(res.dt, c("repl", "dob"))
    res.dt[, y.dob.c := cummin(y.dob), by = .(repl)]
    res.dt[, y.th := cut(y.dob.c, c(Inf, threasholds, -Inf))]
    if ("opt" %in% names(threasholds)) {
      levels(res.dt$y.th) = c("<NA>", rev(names(threasholds))[-1], "<worse>")
    } else {
      levels(res.dt$y.th) = c(rev(names(threasholds)), "<worse>")
    }
    res.dt[, y.th := ordered(y.th, levels = rev(levels(y.th)))]
  } else {
    res.dt[, y.dob := max(y), by = .(dob, repl)]
    res.dt[, y.dob.c := cummax(y.dob), by = .(repl)]
    stop("Not implemented!")
  }

  g1 = ggplot(res.dt, aes(x = dob, y = y.dob.c, group = repl))
  g1 = g1 + geom_line()
  g1 = g1 + geom_hline(data = data.frame(value = benchmark$threasholds, threasholds = names(benchmark$threasholds)), mapping = aes(yintercept = value), alpha = 0.2)
  g1

  level.dt = CJ(repl = unique(res.dt$repl), y.th.i = seq_along(levels(res.dt$y.th)))
  res.dt[, y.th.i := as.integer(y.th)]
  setkeyv(res.dt, c("repl", "y.th.i"))
  setkeyv(level.dt, c("repl", "y.th.i"))
  res.dt = res.dt[level.dt, roll = -Inf]

  res.dt2 = copy(res.dt)
  res.dt2[, y.th := ordered(y.th.i, labels = levels(res.dt2$y.th))]
  res.dt2[repl == 3]
  res.dt2[is.na(y), dob := as.integer(ceiling(max(res.dt2$dob) * 1.1))]
  res.dt2 = res.dt2[, .SD[dob == max(dob),], by = c("repl", "y.th")]
  g2 = ggplot(res.dt2, aes(x = y.th, y = dob))
  g2 = g2 + geom_boxplot()
  g2 + coord_flip()
}

if (FALSE) {
  library(purrr)
  library(checkmate)
  mydev()
  benchmark = generateSimpleBenchmark(makeBraninFunction())
  benchmark2 = generateSimpleBenchmark(makeSwiler2014Function())

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
  repls.bench1 = Map(bench.exec$execute, benchmark = list(benchmark), paramB = 20, repl = as.list(1:10))
  repls.bench2 = Map(bench.exec$execute, benchmark = list(benchmark2), paramB = 30, repl = as.list(1:10))
  res.list = c(repls.bench1)
  # res.list = c(repls.bench1, repls.bench2)
}
