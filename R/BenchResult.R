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
    executed = FALSE,
    algo.name = NULL,
    algo.params = NULL,

    # constructor
    initialize = function(id = NULL, benchmark, algo.name = id, algo.params = NULL, op.dt = NULL, opt.path = NULL, values = NULL, repl = 1) {

      assertClass(benchmark, "Benchmark")
      assertCharacter(id, any.missing = FALSE, null.ok = TRUE)
      assertInt(repl)
      assertString(algo.name, null.ok = TRUE)
      assertList(algo.params, null.ok = TRUE)

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
    }
  )
)