#' @title BenchmarkExpensive Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' BenchmarkExpensive Class
#' 
#' @family Benchmark
#' 
#' @export
BenchmarkExpensive = R6Class(
  classname = "BenchmarkExpensive",
  inherit = Benchmark,

  public = list(
    # member variables
    resources.walltime = NULL,
    resources.memory = NULL,
    resources.cpus = NULL,

    # constructor
    initialize = function(id = NULL, smoof.fun, termination.criterions, expensive = FALSE, threasholds, initial.designs = NULL, initial.design.n = NULL, tags = character(0L), values = NULL, resources.cpus = 1, resources.memory = 256, resources.walltime = 1) {

      self$resources.cpus = assertInt(resources.cpus, lower = 1)
      self$resources.memory = assertInt(resources.memory, lower = 256)
      self$resources.walltime = assertInt(resources.walltime, lower = 1)

    }
  )
)