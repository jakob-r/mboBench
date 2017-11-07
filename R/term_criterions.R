TerminationIteration = R6Class(
  "TerminationIteration",
  inherit = TerminationCriterion,
  public = list(
    initialize = function(max.iteration) {
      super$initialize(
        id = "max.iteration",
        fun = function(iteration, ...) c(list(...), iteration = iteration, term = iteration > self$vars$max.iteration, origin = class(self)[1]),
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
        id = "max.evals",
        fun = function(evals, ...) {
          c(list(...), evals = evals, term = evals > self$vars$max.evals, origin = class(self)[1])
        },
        vars = list(max.evals = assertInt(max.evals, lower = 1))
      )
    }
  )
)