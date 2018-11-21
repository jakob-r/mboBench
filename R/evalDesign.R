# design: data.frame of x points, eventual present y will be erased and ignored
# smoof.fun: smoof.fun to evaluate on x poitns
# y.names: names of y values for filtering only

# out
# y values as vector with exec.times as vector attribute

evalDesign = function(design, smoof.fun) {
  assertDataFrame(design)
  assertClass(smoof.fun, "smoof_function")

  par.set = getParamSet(smoof.fun)
  sub.design = design[, getParamIds(par.set, repeated = TRUE, with.nr = TRUE), drop = FALSE]

  xs = dfRowsToList(sub.design, par.set, enforce.col.types = TRUE) #FIXME changes NA_numeric to NA_logical! (1)

  if (hasTrafo(par.set)) {
    xs.trafo = lapply(xs, trafoValue, par = par.set)
  } else {
    xs.trafo = xs
  }
  ys = foreach(x = xs.trafo) %do% {
    st = proc.time()
    x = x[!sapply(x, is.na)] # Should avoid problems with (1)
    y = smoof.fun(x)
    st = proc.time() - st
    list(y = y, time = st[3])
  }
  y = map(ys, "y")
  y = do.call(rbind, y) # y now should always be a matrix [nrow(design) x d]
  colnames(y) = getYNames(ncol(y))
  exec.times = unname(unlist(map(ys, "time")))
  setAttribute(y, "exec.times", exec.times)
}