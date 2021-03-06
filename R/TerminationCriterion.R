#' @title TerminationCriterion Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' TerminationCriterion Class
#'
#' @family TerminationCriterion
#'
#' @export
TerminationCriterion = R6Class(
  "TerminationCriterion",
  public = list(

    # member variables
    vars = NULL,
    fun = NULL,
    id = NULL,

    # constructor
    initialize = function(id, fun, vars = list()) {
      self$id = assertCharacter(id)
      self$fun = assertFunction(fun, args = "...")
      assertSubset(self$formals, c("evals", "iteration", "time.used", "exec.time.used", "current.y.value", "start.y.value", "..."))
      self$vars = assertList(vars)
    },

    # public methods
    mlrmbo.term = function(opt.state) {
      opt.path = mlrMBO:::getOptStateOptPath(opt.state)
      args = list(
        evals = getOptPathLength(opt.path),
        iteration = mlrMBO:::getOptStateLoop(opt.state),
        time.used = as.numeric(mlrMBO:::getOptStateTimeUsed(opt.state), units = "secs"),
        exec.time.used = sum(getOptPathExecTimes(opt.path), na.rm = TRUE),
        current.y.value = getOptPathY(opt.path)[getOptPathBestIndex(opt.path)],
        start.y.value = getOptPathY(opt.path)[getOptPathBestIndex(opt.path, dob = 0)]
      )
      res = do.call(self$fun, args)
      list(term = res$term, code = res$code, progress = res$progress, message = res$message)
    }
  ),

  active = list(
    formals = function() names(formals(self$fun))
  )
)