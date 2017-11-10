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

TerminationValue = R6Class(
  "TerminationValue",
  inherit = TerminationCriterion,
  public = list(
    initialize = function(best.y.value, minimization = TRUE, tol = 0) {
      super$initialize(
        id = "best.y.value",
        fun = function(best.y.value, ...) {
          if (self$vars$minimization) {
            term = best.y.value + tol >= self$vars$best.y.value
          } else {
            term = best.y.value - tol <= self$vars$best.y.value
          }
          c(list(...), best.y.value = best.y.value, term = term, origin = class(self)[1])
        },
        vars = list(
          best.y.value = assertNumeric(max.evals, na.ok = FALSE),
          minimization = assertFlag(minimization),
          tol = assertNumeric(tol, na.ok = FALSE)
        )
      )
    }
  )
)