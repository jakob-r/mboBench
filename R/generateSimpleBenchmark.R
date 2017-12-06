#' @export
generateSimpleBenchmark = function(smoof.fun) {
  old.seed = .Random.seed
  set.seed(1)
  on.exit({ .Random.seed <<- old.seed })

  if (!isSingleobjective(smoof.fun)) {
    stop ("Anything else then single objective not supported yet.")
  }
  d = getNumberOfParameters(smoof.fun)
  random.design = generateRandomDesign(n = 500*d, par.set = getParamSet(smoof.fun))
  ys = evalDesign(random.design, smoof.fun)[,1]
  assertNumeric(ys, any.missing = FALSE, len = nrow(random.design))
  threasholds = quantile(ys, seq(0, 1, by = 0.1))
  if (shouldBeMinimized(smoof.fun)) {
    threasholds = rev(threasholds)
  }
  best.y.value = getGlobalOptimum(smoof.fun)$value 
  if (!is.null(best.y.value)) {
    threasholds = c(threasholds, opt = getGlobalOptimum(smoof.fun)$value)
    termination.criterions = list(
      termination.value = TerminationValue$new(best.y.value = best.y.value, minimization = shouldBeMinimized(smoof.fun), tol = 1e-10)
      )
  } else {
    termination.criterions = list()
  }

  initial.design.n = 4 * d

  # calculate how many evaluations we should allow
  max.evals = 20 * sqrt(d)
  x = ys
  if (!shouldBeMinimized(smoof.fun)) {
    x = -x
  }
  n = length(x)
  x = x - mean(x)
  # a skew < 0 means a hard minimization problem, because just a few values are close to the minimum
  skew = 1/n * sum(x ^ 3) / (1/(n-1) * sum(x ^ 2)) ^ (3/2)
  #skew.double = 2 * sqrt(2) / 5 # skewness of a triangle a = 0, b = 1 , c = 1
  guess.max.evals = max.evals * 2 ^ (2 * tanh(-skew))
  max.evals = round(initial.design.n + max(min(guess.max.evals, 10 * max.evals), max.evals / 10))
  termination.evals = TerminationEvals$new(max.evals = max.evals)

  tags = c("simple.benchmark", getTags(smoof.fun))

  id = getID(smoof.fun) %??% stri_replace_all_regex(str = getName(smoof.fun), pattern = "[^a-zA-Z1-9]", replacement = "_")

  Benchmark$new(
    id = paste0("simple.", id),
    smoof.fun = smoof.fun,
    termination.criterions = c(list(evals = termination.evals), termination.criterions),
    threasholds = threasholds,
    initial.design.n = initial.design.n,
    tags = tags,
    values = list(skew = skew))
}

if (FALSE) {
  smoof.fun = makeSphereFunction(2)
  bench = generateSimpleBenchmark(smoof.fun)
  bench$termination.criterions[[1]]$vars
  library(mlrMBO)
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, more.termination.conds = bench$mlrmbo.termination.criterions)
  run = mbo(fun = bench$smoof.fun, design = bench$getInitialDesign(1), control = ctrl)
  run$y
  bench$threasholds
}