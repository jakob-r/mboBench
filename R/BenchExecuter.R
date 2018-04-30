#' @title BenchExecutor Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' BenchExecutor Class
#'
#' @export
BenchExecutor = R6Class(
  classname = "BenchExecutor",

  public = list(
    # member variables
    id = NULL,
    fixed.args = NULL,
    executor.fun = NULL,
    resources.cpus.multiplicator = NULL,
    resources.memory.multiplicator = NULL,
    resources.walltime.multiplicator = NULL,

    # constructor
    initialize = function(id = NULL, executor.fun, fixed.args = list(), resources.cpus.multiplicator = 1, resources.memory.multiplicator = 1, resources.walltime.multiplicator = 1) {

      self$id = assertString(id, null.ok = TRUE)
      self$fixed.args = assertList(fixed.args, names = "named")
      self$executor.fun = assertFunction(executor.fun, args = "benchmark")
      self$resources.cpus.multiplicator = assertNumber(resources.cpus.multiplicator, lower = 1)
      self$resources.memory.multiplicator = assertNumber(resources.memory.multiplicator, lower = 1)
      self$resources.walltime.multiplicator = assertNumber(resources.walltime.multiplicator, lower = 1)

    },

    execute = function(benchmark, repl = 1, ...) {
      dots = list(...)
      assertList(dots, names = "named")
      assertTRUE(length(intersect(names(dots), names(self$fixed.args))) == 0)
      args = c(self$fixed.args, dots)
      res = do.call(self$executor.fun, c(list(benchmark = benchmark, repl = repl), args))
      assertClass(res, "BenchResult")
      res$algo.params = c(res$algo.params, args)
      res$algo.name = paste0(self$id, res$algo.name %??% character())
      return(res)
    }
  )
)