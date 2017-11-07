generateSimpleBenchmark = function(smoof.fun) {
  old.seed = .Random.seed
  set.seed(1)
  on.exit({ .Random.seed <<- old.seed })

  d = getNumberOfParameters(smoof.fun)
  random.design = generateRandomDesign(n = 500*d, par.set = getParamSet(smoof.fun))
  random.design$y = apply(random.design, 1, smoof.fun)
  threasholds = quantile(random.design$y, seq(0, 1, by = 0.1))
  if (shouldBeMinimized(smoof.fun)) {
    threasholds = rev(threasholds)
  }
  if (!is.null(getGlobalOptimum(smoof.fun)$value)) {
    threasholds = c(threasholds, opt = getGlobalOptimum(smoof.fun)$value)
  }

  initial.design.n = 4 * d

  # calculate how many evaluations we should allow
  max.evals = 20 * sqrt(d)
  x = random.design$y
  if (!shouldBeMinimized(smoof.fun)) {
    x = -x
  }
  n = length(x)
  x = x - mean(x)
  # a skew < 0 means a hard minimization problem, because just a few values are close to the minimum
  skew = 1/n * sum(x ^ 3) / (1/(n-1) * sum(x ^ 2)) ^ (3/2)
  #skew.double = 2 * sqrt(2) / 5 # skewness of a triangle a = 0, b = 1 , c = 1
  max.evals = round(max.evals * 2 ^ (2 * tanh(-skew)))
  max.evals = max.evals + initial.design.n
  termination.evals = TerminationEvals$new(max.evals = max.evals)

  tags = c("simple.benchmark", getTags(smoof.fun))

  Benchmark$new(
    id = paste0("simple.", getID(smoof.fun)),
    smoof.fun = smoof.fun,
    termination.criterions = list(termination.evals),
    threasholds = threasholds,
    initial.design.n = initial.design.n,
    tags = tags,
    values = list(random.design = random.design, skew = skew))
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