#' @title TerminationIteration Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' TerminationIteration Class
#' 
#' @family TerminationCriterion
#' 
#' @export
TerminationIteration = R6Class(
  "TerminationIteration",
  inherit = TerminationCriterion,
  public = list(
    initialize = function(max.iteration) {
      super$initialize(
        id = "max.iteration",
        fun = function(iteration, ...) {
          c(list(...), 
            iteration = iteration, 
            term = iteration > self$vars$max.iteration, 
            progress = iteration / self$vars$max.iteration,
            origin = class(self)[1])
        },
        vars = list(max.iteration = assertInt(max.iteration, lower = 1))
      )
    }
  )
)

#' @title TerminationEvals Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' TerminationEvals Class
#' 
#' @family TerminationCriterion
#' 
#' @export
TerminationEvals = R6Class(
  "TerminationEvals",
  inherit = TerminationCriterion,
  public = list(
    initialize = function(max.evals) {
      super$initialize(
        id = "max.evals",
        fun = function(evals, ...) {
          c(list(...), 
            evals = evals, 
            term = evals > self$vars$max.evals, 
            progress = evals / self$vars$max.evals,
            origin = class(self)[1])
        },
        vars = list(max.evals = assertInt(max.evals, lower = 1))
      )
    }
  )
)


#' @title TerminationValue Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' TerminationValue Class
#' 
#' @family TerminationCriterion
#' 
#' @export
TerminationValue = R6Class(
  "TerminationValue",
  inherit = TerminationCriterion,
  public = list(
    initialize = function(best.y.value, minimization = TRUE, tol = 0) {
      super$initialize(
        id = "best.y.value",
        fun = function(current.y.value, start.y.value, ...) {
          if (self$vars$minimization) {
            term = current.y.value + tol <= self$vars$best.y.value 
          } else {
            term = current.y.value - tol >= self$vars$best.y.value
          }
          progress = abs(start.y.value - current.y.value)/abs(start.y.value - self$vars$best.y.value)
          browser()
          c(list(...), current.y.value = current.y.value, term = term, progress = progress, origin = class(self)[1])
        },
        vars = list(
          best.y.value = assertNumber(best.y.value, na.ok = FALSE),
          minimization = assertFlag(minimization),
          tol = assertNumber(tol, na.ok = FALSE)
        )
      )
    }
  )
)