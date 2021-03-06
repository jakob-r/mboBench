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
  
  res = generateThreasholds(smoof.fun, initial.design.n, ys)
  thresholds = res$thresholds
  termination.evals = res$termination.evals
  termination.criterions = res$termination.criterions
  skew = res$skew

  tags = c("simple.benchmark", getTags(smoof.fun))

  id = getID(smoof.fun)
  if (is.na(id)) {
    id = stri_replace_all_regex(str = smoof::getName(smoof.fun), pattern = "[^a-zA-Z1-9]", replacement = "_")
  }

  Benchmark$new(
    id = paste0("simple.", id),
    smoof.fun = smoof.fun,
    termination.criterions = c(list(evals = termination.evals), termination.criterions),
    thresholds = thresholds,
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
  bench$thresholds
}