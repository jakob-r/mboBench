aggregateBenchRepls = function(res.list, benchmark) {
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

  # add nev (noumber of evaluation counter) if missing
  if (is.null(res.dt$nev)) {
    res.dt[, nev := seq_len(.N), by = c("repl", "algo.name", "algo.name.post")]
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

  # Build data.frame when each threashold was passed

  res.dt2 = copy(res.dt)
  getUsableThr = function(x) {
    ordered(x, levels = setdiff(levels(x), c("<NA>", "<worse>")))
  }

  level.dt = CJ(algo.name.config = unique(res.dt2$algo.name.config), repl = unique(res.dt2$repl), y.th.i = seq_along(levels(getUsableThr(res.dt2$y.th))))
  res.dt2[, y.th.i := as.integer(getUsableThr(y.th))]
  setkeyv(res.dt2, c("algo.name.config", "repl", "y.th.i"))
  setkeyv(level.dt, c("algo.name.config", "repl", "y.th.i"))
  res.dt2 = res.dt2[level.dt, roll = -Inf]
  res.dt2[, y.th := ordered(y.th.i, labels = levels(getUsableThr(res.dt2$y.th)))]

  # punish budget for incomplete runs
  res.dt2[is.na(dob), dob := as.integer(ceiling(max(res.dt$dob) * 2))]
  res.dt2[is.na(nev), nev := as.integer(ceiling(max(res.dt$nev) * 2))]

  res.dt2 = res.dt2[, .SD[dob == max(dob),], by = c("algo.name.config", "repl", "y.th")]

  list(op.dt = res.dt, threasholds.dt = res.dt2)
}
