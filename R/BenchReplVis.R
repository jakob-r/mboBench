#' @title Visualization for a single repeated Benchmar
#'
#' @description
#' Visualization for a single repeated Benchmar
#' @format \code{\link{R6Class}} object 
#' @export
#' 
##' @param res.list [\code{list}]\cr
##'   Lift of BenchResult Objects
##' @param benchmark [\code{Benchmark}]\cr
##'   The Benchmark object for the benchmarks in the \code{res.list}.
##' @return list of ggplot objects
BenchReplVis = R6Class("BenchReplVis",

  public = list(
    # public member
    res = NULL,

    # constructor
    initialize = function(res.list, benchmark) {
      assertList(res.list, types = "BenchResult", any.missing = FALSE)
      assertR6(benchmark, classes = "Benchmark")
      self$res = aggregateBenchRepls(res.list, benchmark)
    },

    # public methods
    plot_opt_path_progress = function() {
      g1 = ggplot(res$op.dt, aes_string(x = "dob", y = "y.dob.c", group = "algo.name.config", color = "algo.name.config", fill = "algo.name.config"))
      g1 = g1 + geom_point(size = 0.5, alpha = 0.2)
      g1 = g1 + geom_hline(data = data.frame(value = benchmark$thresholds, thresholds = names(benchmark$thresholds)), mapping = aes(yintercept = value), alpha = 0.2)
      g1 = g1 + stat_summary(fun.y = median, geom="line")
      g1 = g1 + stat_summary(fun.ymin = partial(quantile, probs = 0.1), geom="ribbon", fun.ymax = partial(quantile, probs = 0.9), alpha = 0.1, color = NA)
      y.range = range(c(benchmark$thresholds, benchmark$minmax(res$op.dt$y))) #add min y if minimization or max y if maximization to range
      g1 = g1 + coord_cartesian(ylim = y.range)
      g1
    },
    plot_threshold_progress = function() {
      g2 = ggplot(res$thresholds.dt, aes(x = y.th, y = dob, fill = algo.name.config))
      g2 = g2 + geom_boxplot()
      g2 = g2 + geom_hline(yintercept = max(res$op.dt$dob))
      g2 = g2 + coord_cartesian(ylim=c(min(res$thresholds.dt$dob), max(res$op.dt$dob)))
      g2  
    }
    )
  )


if (FALSE) {
  library(purrr)
  library(checkmate)
  mydev()
  benchmarkA = generateSimpleBenchmark(makeBraninFunction())

  bench.function = function(benchmark, paramA, paramB, repl) {
    random.design = generateRandomDesign(n = paramA+paramB, par.set = getParamSet(benchmark$smoof.fun))
    ys = evalDesign(random.design, benchmark$smoof.fun)[,1]
    op.dt = as.data.table(random.design)
    op.dt$y = ys
    op.dt = op.dt[order(ys, decreasing = benchmark$minimize)]
    op.dt = tail(op.dt, benchmark$termination.criterions$evals$vars$max.evals)
    BenchResult$new(benchmark = benchmark, op.dt = op.dt, repl = repl)
  }
  bench.exec = BenchExecutor$new(id = "test.rs", executor.fun = bench.function, fixed.args = list(paramA = 100))
  repls.benchA1 = Map(bench.exec$execute, benchmark = list(benchmarkA), paramB = 1000, repl = as.list(1:10))
  repls.benchA2 = Map(bench.exec$execute, benchmark = list(benchmarkA), paramB = 40, repl = as.list(1:10))
  res.list = c(repls.benchA1, repls.benchA2)
  brv = BenchReplVis$new(res.list, benchmarkA)
  brv$plot_opt_path_progress()
  brv$plot_threshold_progress()
  # res.list = c(repls.bench1, repls.bench2)
}
