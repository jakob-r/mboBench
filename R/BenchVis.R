aggregateBenchRepls = function(res.list, benchmark) {
  assertList(res.list, "BenchResult")
  assert_true(all(sapply(map_chr(res.list, "benchmark.hash"), all.equal, current = benchmark$hash)))
  repls = map_int(res.list, "repl")

  # check if replications make sense
  assert_true(length(unique(table(repls))) == 1)
  assert_true(mean(repls == 1) != 1)

  # generate list of repls with opt paths
  res.dt = Map(cbind.data.frame, map(res.list, "op.dt"), repl = repls)

  res.dt = Map(function(x, y) {cbind(x, algo.name = y$algo.name %??% NA)}, res.dt, res.list)

  res.dt = Map(function(x, y) {
    if (length(y$algo.params)>0) {
      z = setNames(as.data.frame(y$algo.params), paste0("algo.param.", names(y$algo.params)))
      z$algo.name.post = paste0(names(y$algo.params), "=", y$algo.params, collapse = " ")
      cbind(x, z)
    } else {
      cbind(x, algo.name.post = "")
    }
  }, res.dt, res.list)

  # combine opt paths to one data.table
  res.dt = rbindlist(res.dt, fill = TRUE)


  # add DOB if missing
  dob = NULL #haha hack
  res.dt[is.null(dob), dob := seq_len(.N), by = c("repl", "algo.name", "algo.name.post")]

  # add nev (noumber of evaluation counter) if missing
  nev = NULL #again hack
  res.dt[is.null(nev), nev := seq_len(.N), by = c("repl", "algo.name", "algo.name.post")]

  res.dt[, algo.name.config := paste(algo.name, algo.name.post)]

  thresholds = benchmark$thresholds
  if (benchmark$minimize) {
    res.dt[, y.dob := min(y), by = c("algo.name.config", "repl", "dob")]
    setkeyv(res.dt, c("algo.name.config", "repl", "dob"))
    res.dt[, y.dob.c := cummin(y.dob), by = c("algo.name.config", "repl")]
    res.dt[, y.th := cut(y.dob.c, c(Inf, thresholds, -Inf))]
    if ("opt" %in% names(thresholds)) {
      res.dt$y.th = lvls_revalue(res.dt$y.th, c("<NA>", rev(names(thresholds))[-1], "<worse>"))
    } else {
      res.dt$y.th = lvls_revalue(res.dt$y.th, c(rev(names(thresholds)), "<worse>"))
    }
    res.dt[, y.th := ordered(y.th, levels = rev(levels(y.th)))]
  } else {
    res.dt[, y.dob := max(y), by = c("algo.name.config", "repl",  "dob")]
    res.dt[, y.dob.c := cummax(y.dob), by = c("algo.name.config", "repl")]
    res.dt[, y.th := cut(y.dob.c, c(-Inf, thresholds, Inf))]
    if ("opt" %in% names(thresholds)) {
      res.dt$y.th = lvls_revalue(res.dt$y.th, c("<worse>", names(thresholds)[-1], "<NA>"))
    } else {
      res.dt$y.th = lvls_revalue(res.dt$y.th, c("<worse>", names(thresholds)))
    }
    res.dt[, y.th := ordered(y.th, levels = levels(y.th))]
  }

  # Build data.frame when each threshold was passed

  res.dt2 = copy(res.dt)
  getUsableThr = function(x) {
    ordered(x, levels = setdiff(levels(x), c("<NA>", "<worse>")))
  }

  # crossproduct of all data points we need
  level.dt = CJ(algo.name.config = unique(res.dt2$algo.name.config), repl = unique(res.dt2$repl), y.th.i = seq_along(levels(getUsableThr(res.dt2$y.th))))
  res.dt2[, y.th.i := as.integer(getUsableThr(y.th))]
  setkeyv(res.dt2, c("algo.name.config", "repl", "y.th.i"))
  setkeyv(level.dt, c("algo.name.config", "repl", "y.th.i"))
  res.dt2 = res.dt2[level.dt, roll = -Inf]
  res.dt2[, y.th := ordered(y.th.i, labels = levels(getUsableThr(res.dt2$y.th)))]

  # punish budget for incomplete runs
  res.dt2[is.na(dob), dob := as.integer(ceiling(max(res.dt$dob) * 2))]
  res.dt2[is.na(nev), nev := as.integer(ceiling(max(res.dt$nev) * 2))]

  res.dt2 = res.dt2[, .SD[dob == max(dob),], by = c("algo.name.config", "repl", "y.th", "algo.name", "algo.name.post", stringi::stri_subset(str = colnames(res.dt2), regex = "algo\\.param"))]

  list(op.dt = res.dt, thresholds.dt = res.dt2)
}
