#' @title BenchResult Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' BenchResult Class
#'
#' @export
BenchResult = R6Class(
  classname = "BenchResult",
  public = list(

    # member variables
    id = NULL,
    benchmark.hash = NULL,
    op.dt = NULL,
    values = NULL,
    repl = NULL,
    algo.name = NULL,
    algo.params = NULL,
    benchmark.thresholds = NULL,
    benchmark.minimize = NULL,
    benchmark.max.evals = NULL,

    # constructor
    initialize = function(id = NULL, benchmark, algo.name = id, algo.params = NULL, op.dt = NULL, opt.path = NULL, values = NULL, repl = 1) {

      assertClass(benchmark, "Benchmark")
      assertCharacter(id, any.missing = FALSE, null.ok = TRUE)
      assertInt(repl)
      assertString(algo.name, null.ok = TRUE)
      assertList(algo.params, c("numeric", "integer", "character", "logical", "complex"), null.ok = TRUE)

      assertClass(opt.path, "OptPath", null.ok = TRUE)
      assertTRUE(xor(is.null(op.dt), is.null(opt.path)))

      if (!is.null(opt.path)) {
        op.dt = as.data.table(opt.path)
      }

      assertDataTable(op.dt, null.ok = TRUE)

      # check that we don't have too much observations
      max.n = benchmark$termination.criterions$evals$vars$max.evals
      if (!is.null(max.n) && max.n < nrow(op.dt)) {
        stop(sprintf("Result has more observations then allowed by termination.criterion. %i instead of %i", nrow(op.dt), max.n))
      }

      # check that colum names fit
      x.ids = benchmark$x.ids
      y.ids = benchmark$y.ids
      assertSubset(x.ids, colnames(op.dt))
      assertSubset(y.ids, colnames(op.dt))

      # check that the inital design fits if dob = 0 present
      if ("dob" %in% colnames(op.dt) && sum(op.dt$dob == 0) > 1) {
        if (benchmark$expensive) {
          init.design = benchmark$getInitialDesign(repl)
        } else {
          init.design = benchmark$getInitialDesignEvaluated(repl)
        }
        assertTRUE(identical(op.dt[seq_along(init.design[,1]), colnames(init.design), with = FALSE], as.data.table(init.design)))
      }

      self$id = id %??% benchmark$id
      self$values = assertList(values, null.ok = TRUE)
      self$benchmark.hash = benchmark$hash
      self$op.dt = op.dt
      self$repl = repl
      self$algo.name = algo.name
      self$algo.params = algo.params
      self$benchmark.thresholds = benchmark$thresholds
      self$benchmark.minimize = benchmark$minimize
      self$benchmark.max.evals = benchmark$termination.criterions$evals$vars$max.evals
    }
  ),

  active = list(
    threshold.performances = function() {
      if (is.null(private$p.threshold.performances)) {
        private$p.threshold.performances = benchresultThresholdPerformances(self)
      }
      return(private$p.threshold.performances)
    },
    stepfun = function() {
      if (is.null(private$p.stepfun)) {
        private$p.stepfun = benchresultStepfun(self)
      }
      return(private$p.stepfun)
    },
    threshold.auc = function() {
      if (is.null(private$p.threshold.auc)) {
        private$p.threshold.auc = benchresultThresholdAuc(self)
      }
      return(private$p.threshold.auc)
    }
  ),

  private = list(
    p.threshold.performances = NULL,
    p.stepfun = NULL,
    p.threshold.auc = NULL
)

benchresultThresholdPerformances = function(self) {
  if (!self$benchmark.minimize) {
    stop("not implemented for maximization, yet")
  }
  
  op.dt = copy(self$op.dt)
  nev = NULL #again hack
  op.dt[is.null(nev), nev := seq_len(.N)]
  dob = NULL
  op.dt[is.null(dob), dob := seq_len(.N)]
  
  # build all wanted thresholds
  dt.th = data.table(y = self$benchmark.thresholds, threshold = factor(names(self$benchmark.thresholds), levels = names(self$benchmark.thresholds)))
  # for each threshold find the evals(lines) that below this one and before the next one
  op.dt = dt.th[op.dt, , roll = -Inf, on = "y"]
  # for each threshold take the first eval (according to dob)
  op.dt = op.dt[, .SD[dob == min(dob), ][1,] ,by = .(threshold)]
  # generate lines for each threshold
  op.dt = op.dt[dt.th[, .(threshold), ], on = "threshold"]
  # sort lower threshold first
  setorderv(op.dt, "threshold", order=-1L, na.last=FALSE)
  # calc when threshold was hit according to dob
  op.dt[, nev.progress := nev / self$benchmark.max.evals]
  op.dt[, nev.progress := cummin(ifelse(is.na(nev.progress), Inf, nev.progress))]
  setorderv(op.dt, "threshold", order=1L, na.last=FALSE)
  return(op.dt[, .(threshold, nev.progress)])
}

benchresultStepfun = function(self) {
  y = rev(as.numeric(self$threshold.performances$threshold))
  y = (y-1) / max(y)
  x = self$threshold.performances$nev.progress
  y = y[is.finite(x)]
  x = x[is.finite(x)]
  x = c(0,x)
  y = c(1,y, min(tail(y,1),1)) # min incase y is empty (all x inf)
  stepfun(x, y)
}

benchresultThresholdAuc = function(self) {
  integrate(self$stepfun,0,1)$value
}