generateThreasholds = function(smoof.fun, initial.design.n, ys) {

  d = getNumberOfParameters(smoof.fun)
  
  if (shouldBeMinimized(smoof.fun)) {
    minomax = min
    val.to = 0
  } else {
    minomax = max
    val.to = 1
  }

  # get the best y value that is reached in the initial design in average
  design.best.y = mean(aggregate(ys~(seq_along(ys)-1) %/% initial.design.n, FUN = minomax)[,2])
  # what quantile does this represent?
  design.best.quantile = mean(ys<=design.best.y)
  # build threasholds from the worst (reaching what should be reached in the initial design) to the best reached y value in the given ys vector.
  thresholds = quantile(ys, seq(from = design.best.quantile, to = val.to, length.out = 10))

  if (length(unique(thresholds)) < 2) {
    stop("Only 1 unique threshold value. This does not make sense.")
  }
  # it can happen that threasholds are duplicated
  # however we are fixed to 10
  # so if we have a duplicated value it means that the bin has to be increased
  # so we increase it by 1/100 of the smallest bin
  inds.dup = duplicated(thresholds)
  if (any(inds.dup)) {
    dup.count = cumsum(inds.dup)
    dup.count = dup.count - cummax(dup.count * !inds.dup) #counts the duplicated values 0 for first appeareance, 1 for first duplicate etc.
    incr.step = dup.count * min(diff(thresholds[!inds.dup])) / 100
    if (shouldBeMinimized(smoof.fun)) {
      thresholds = thresholds - incr.step  
    } else {
      thresholds = thresholds + incr.step
    }  
  }
  
  

  best.y.value = getGlobalOptimum(smoof.fun)$value
  if (!is.null(best.y.value)) {
    thresholds = c(thresholds, opt = getGlobalOptimum(smoof.fun)$value)
    termination.criterions = list(
      termination.value = TerminationValue$new(best.y.value = best.y.value, minimization = shouldBeMinimized(smoof.fun), tol = 1e-10)
      )
  } else {
    termination.criterions = list()
  }

  termination.evals = TerminationEvals$new(max.evals = 16 * d)

  # Calculate skewness
  x = ys
  if (!shouldBeMinimized(smoof.fun)) {
    x = -x
  }
  n = length(x)
  x = x - mean(x)
  # a skew < 0 means a hard minimization problem, because just a few values are close to the minimum
  skew = 1/n * sum(x ^ 3) / (1/(n-1) * sum(x ^ 2)) ^ (3/2)

  list(thresholds = thresholds, termination.evals = termination.evals, termination.criterions = termination.criterions, skew = skew)
}

if (FALSE) {
  initial.design.n = 10
  ys = runif(1000, 0 , 100)
}
