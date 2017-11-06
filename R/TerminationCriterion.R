TerminationCriterion = R6Class(
  "TerminationCriterion",
  public = list(

    # member variables
    vars = NULL,
    fun = NULL,
    id = NULL,

    # constructor
    initialize = function(fun, vars = list()) {
      self$id = assertCharacter(id)
      self$fun = assertFunction(fun, args = "...")
      assertSubset(self$formals, c("evals", "iteration", "time.used", "exec.time.used", "best.y.value"))
      self$vars = assertList(vars)
    },

    # public methods
    mlrmbo.term = function(opt.state) {
      opt.path = mlrMBO:::getOptStateOptPath(opt.state)
      args = list(
        evals = getOptPathLength(opt.path),
        iteration = mlrMBO:::getOptStateLoop(opt.state),
        time.used = as.numeric(mlrMBO:::getOptStateTimeUsed(opt.state), units = "secs"),
        exec.time.used = sum(mlrMBO:::getOptPathExecTimes(opt.path), na.rm = TRUE),
        best.y.value = getOptPathEl(opt.path, getOptPathBestIndex((opt.path)))$y
      )
      res = do.call(self$fun, args)
      list(term = res$term, message = "term.custom")
    }
  ),

  active = list(
    formals = names(formals(self$fun))
  )
)