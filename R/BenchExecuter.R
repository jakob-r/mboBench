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
    benchmark = NULL,
    executor.fun = NULL,
    resources.cpus.multiplicator = NULL,
    resources.memory.multiplicator = NULL,
    resources.walltime.multiplicator = NULL,

    # constructor
    initialize = function(id = NULL, benchmark, executor.fun, fixed.args = list(), resources.cpus.multiplicator = 1, resources.memory.multiplicator = 1, resources.walltime.multiplicator = 1) {

      self$id = assertString(id) 
      self$fixed.args = assertList(fixed.args, names = "named")
      self$benchmark = assertClass(benchmark, "Benchmark")
      self$executor.fun = assertFunction(executor.fun, args = "benchmark")
      self$resources.cpus.multiplicator = assertNumber(resources.cpus.multiplicator, lower = 1)
      self$resources.memory.multiplicator = assertNumber(resources.memory.multiplicator, lower = 1)
      self$resources.walltime.multiplicator = assertNumber(resources.walltime.multiplicator, lower = 1)

    },

    execute = function(...) {
      dots = list(...)
      assertList(dots, names = "named")
      assertTRUE(length(intersect(names(dots), names(self$fixed.args))) == 0)
      args = c(list(benchmark = self$benchmark), self$fixed.args, dots)
      res = do.call(self$executor.fun, args)
      assertClass(res, "BenchResult")
      res$setExecutorValues(fixed.args = self$fixed.args, args = args, values = list(executor.fun.hash = sha1(self$executor.fun)))
      return(res)
    }
  )
)