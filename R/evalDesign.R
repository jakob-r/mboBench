# design: data.frame of x points, eventual present y will be erased and ignored
# smoof.fun: smoof.fun to evaluate on x poitns
# y.names: names of y values for filtering only

# out
# y values as vector with exec.times as vector attribute

evalDesign = function(design, smoof.fun) {
  assertDataFrame(design)
  assertClass(smoof.fun, "smoof_function")

  par.set = getParamSet(smoof.fun)

  xs = dfRowsToList(design[, getParamIds(par.set, repeated = TRUE, with.nr = TRUE)], par.set)

  evalX = function(xs) {
    if (hasTrafo(par.set)) {
      xs.trafo = lapply(xs, trafoValue, par = par.set)
    } else {
      xs.trafo = xs
    }
    parallelMap(
      function(x) {
        st = proc.time()
        y = smoof.fun(x)
        st = proc.time() - st
        list(y = y, time = st[3])
      }, x = xs.trafo, level = "mboBench.evalX"
    )
  }
  ys = evalX(xs)
  y = map(ys, "y")
  y = do.call(rbind, y) # y now should always be a matrix [nrow(design) x d]
  colnames(y) = getYNames(ncol(y))
  exec.times = unname(unlist(map(ys, "time")))
  setAttribute(y, "exec.times", exec.times)
}