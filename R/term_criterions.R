TerminationIteration = R6Class(
  "TerminationIteration",
  inherit = TerminationCriterion,
  public = list(
    initialize = function(max.iteration) {
      super$initialize(
        fun = function(iteration, ...) c(list(...), iteration = iteration, term = iteration > self$value$max.iteration, origin = class(self)[1]),
        vars = list(max.iteration = assertInt(max.iteration, lower = 1))
      )
    }
  )
)

TerminationEvals = R6Class(
  "TerminationEvals",
  inherit = TerminationCriterion,
  public = list(
    initialize = function(max.evals) {
      super$initialize(
        fun = function(evals) c(list(...), evals = evals, term = evals > self$value$max.evals, origin = class(self)[1]),
        vars = list(max.evals = assertInt(max.evals, lower = 1))
      )
    }
  )
)