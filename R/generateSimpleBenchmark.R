#' @title Generates a simple Benchmark
#'
#' @description
#' Generates a simple Benchmark from simple inexpensive single objective functions.
#'
#' @param smoof.fun [\code{smoof_function}]\cr
#'   Single objective smoof function.
#' @return \code{\link{Benchmark}}
#' @export
generateSimpleBenchmark = function(smoof.fun) {
  old.seed = getRandomSeed()
  set.seed(1)
  on.exit({ .Random.seed <<- old.seed })

  if (!isSingleobjective(smoof.fun)) {
    stop ("Anything else then single objective not supported yet.")
  }
  d = getNumberOfParameters(smoof.fun)
  initial.design.n = 4 * d

  random.design = generateRandomDesign(n = 500*d, par.set = getParamSet(smoof.fun))
  ys = evalDesign(random.design, smoof.fun)[,1]
  assertNumeric(ys, any.missing = FALSE, len = nrow(random.design))
  if (shouldBeMinimized(smoof.fun)) {
    design.min = mean(aggregate(ys~floor(seq_along(ys) %% initial.design.n), FUN = min)[,2])
    design.min = mean(ys<=design.min)
    threasholds = quantile(ys, seq(from = design.min, to = 0, length.out = 10))
  } else {
    design.max = mean(aggregate(ys~floor(seq_along(ys) %% initial.design.n), FUN = max)[,2])
    design.max = mean(ys>=design.max)
    threasholds = quantile(ys, seq(from = design.max, to = 1, length.out = 10))
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

  id = getID(smoof.fun)
  if (is.na(id)) {
    id = stri_replace_all_regex(str = smoof::getName(smoof.fun), pattern = "[^a-zA-Z1-9]", replacement = "_")
  }
  
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