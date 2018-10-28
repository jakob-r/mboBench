#' @title Generates a simple Benchmark
#'
#' @description
#' Generates a simple Benchmark from simple inexpensive single objective functions.
#'
#' @param smoof.fun [\code{smoof_function}]\cr
#'   Single objective smoof function.
#' @param design [\code{data.frame}]\cr
#'   The preevaluated design of the function.
#'   The results have to be in the column "y"
#' @return \code{\link{Benchmark}}
#' @export
generateSimpleBenchmarkPreEvaluated = function(smoof.fun, design) {
  old.seed = getRandomSeed()
  set.seed(1)
  on.exit({ .Random.seed <<- old.seed })

  if (!isSingleobjective(smoof.fun)) {
    stop ("Anything else then single objective not supported yet.")
  }
  d = getNumberOfParameters(smoof.fun)
  initial.design.n = 4 * d
  ps = getParamSet(smoof.fun)
  par.ids = getParamIds(ps, repeated = TRUE, with.nr = TRUE)
  assertDataFrame(design)
  assertSubset(par.ids, colnames(design))
  random.design = design[, par.ids, drop = FALSE]
  ys = design$y
  assertNumeric(ys, any.missing = FALSE, len = nrow(random.design))
  
  res = generateThreasholds(smoof.fun, initial.design.n, ys)
  thresholds = res$thresholds
  termination.evals = res$termination.evals
  termination.criterions = res$termination.criterions
  skew = res$skew

  tags = c("preevaluated.benchmark", getTags(smoof.fun))

  id = getID(smoof.fun)
  if (is.na(id)) {
    id = stri_replace_all_regex(str = smoof::getName(smoof.fun), pattern = "[^a-zA-Z0-9]", replacement = "_")
  }

  Benchmark$new(
    id = paste0("preevaluated.", id),
    smoof.fun = smoof.fun,
    termination.criterions = c(list(evals = termination.evals), termination.criterions),
    thresholds = thresholds,
    initial.design.n = initial.design.n,
    tags = tags,
    values = list(skew = skew))
}

if (FALSE) {
  library("omlTuneBenchR")
  r = startOmlTuneServer()
  learner_id = "classif.svm"
  task_id = 3
  smoof.fun = makeOmlBenchFunction(learner_id, task_id, include.extras = TRUE)
  design = getAllRuns(learner.name = learner_id, task.id = task_id)
  design = dplyr::rename(design, y = "auc")
  benchmark = generateSimpleBenchmark(smoof.fun, design)
}
