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
      sefl$benchmark.max.evals = benchmark$termination.criterions$evals$vars$max.evals
    }
  ),

  active = list(
    threshold.performances = function() {
      if (is.null(private$p.threshold.performances)) {
        if (!self$benchmark.minimize) {
          stop("not implemented for maximization, yet")
        }
        
        op.dt = self$op.dt
        nev = NULL #again hack
        op.dt[is.null(nev), nev := seq_len(.N)]
        
        # build all wanted thresholds
        dt.th = data.table(y = self$benchmark.thresholds, threshold = names(self$benchmark.thresholds))
        # for each threshold find the evals(lines) that below this one and before the next one
        op.dt = dt.th[op.dt, , roll = -Inf, on = "y"]
        # for each threshold take the first eval (according to dob)
        op.dt = op.dt[, .SD[dob == min(dob), ][1,] ,by = .(threshold)]
        # generate lines for each threshold
        op.dt = op.dt[dt.th[, .(threshold), ], on = "threshold"]
        # sort lower threshold first
        setkeyv(op.dt, "threshold")
        # calc when threshold was hit according to dob
        op.dt[, dob := cummin(ifelse(is.na(dob), Inf, dob))]
        op.dt[, nev.progress := nev / self$benchmark.max.evals]
        private$p.threshold.performances = op.dt[, .(threshold, nev.progress)]
      }
      return(private$threshold.performances)
    },
    auc = function() {
      stop("please implement me")
    }
  ),

  private = list(
    p.threshold.performances = NULL)
)