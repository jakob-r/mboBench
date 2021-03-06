#' @title Benchmark Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' Benchmark Class
#'
#' @family Benchmark
#'
#' @export
Benchmark = R6Class(
  classname = "Benchmark",
  public = list(

    # member variables
    id = NULL,
    smoof.fun = NULL,
    termination.criterions = NULL,
    expensive = NULL,
    thresholds = NULL,
    tags = NULL,
    values = NULL, # stores additional values in a list

    # constructor
    initialize = function(id = NULL, smoof.fun, termination.criterions, expensive = FALSE, thresholds, initial.designs = NULL, initial.design.n = NULL, tags = character(0L), values = NULL) {

      assertFunction(smoof.fun)
      self$smoof.fun = assertClass(smoof.fun, "smoof_function")

      assertCharacter(id, any.missing = FALSE, null.ok = TRUE)
      self$id = id %??% getID(smoof.fun)

      self$termination.criterions = assertList(termination.criterions, types = "TerminationCriterion")

      self$expensive = assertFlag(expensive)

      self$thresholds = assertNumeric(thresholds, any.missing = FALSE, unique = TRUE)

      assertTRUE(xor(is.null(initial.designs), is.null(initial.design.n)))
      private$initial.designs = assertList(initial.designs, "data.frame", null.ok = TRUE)

      if (!is.null(initial.designs)) {
        private$initial.designs.provided = TRUE
        init.design.lenghts = unique(sapply(initial.designs, nrow))
        if (length(unique(init.design.lenghts)) != 1) {
          stopf("The initial designs all have to be of the same length and not: %s", convertToShortString(init.design.lenghts, clip.len = 64))
        } else {
          initial.design.n = init.design.lenghts[1]
        }
      }
      private$initial.design.n = assertInt(initial.design.n, null.ok = TRUE, lower = 0)

      self$tags = assertCharacter(tags, any.missing = FALSE, null.ok = TRUE)

      self$values = assertList(values, null.ok = TRUE, names = "named")
    },

    # public methods
    getInitialDesign = function(i) {
      if (!is.null(private$initial.designs[i][[1]])) {
        return(private$initial.designs[[i]])
      } else if (private$initial.designs.provided) {
        stopf("An initial.design for index %i is not provided.", i)
      } else {
        old.seed = getRandomSeed()
        set.seed(i)
        on.exit({ .Random.seed <<- old.seed })
        res = generateDesign(n = private$initial.design.n, par.set = getParamSet(self$smoof.fun), fun = lhs::maximinLHS)
        private$initial.designs[[i]] = res
        return(res)
      }
    },

    # just an idea. we can also use smoof wrappers. anyway iy has to be a smoof function again.
    fun.logged = function(...) {
      x = as.list(...)
      y = self$smoof.fun(...)
      private$opt.path$add(x = x, y = y)
      return(y)
    },

    getInitialDesignEvaluated = function(i) {
      if (isNoisy(self$smoof.fun)) {
        stop("Not supported for noisy functions.")
      }
      self$evaluateDesign(i)
      self$getInitialDesign(i)
    },

    evaluateDesign = function(i) {
      assertInt(i ,lower = 1L)
      design = self$getInitialDesign(i)
      if (all(self$y.ids %in% colnames(design))) {
        invisible(design)
      } else {
        ys = evalDesign(design, self$smoof.fun)
        design = cbind(design, ys)
        private$initial.designs[[i]] = design
        invisible(design)
      }
    }

  ),

  active = list(
    minimize = function() shouldBeMinimized(self$smoof.fun),
    dim = function() getNumberOfParameters(self$smoof.fun),
    par.set = function() getParamSet(self$smoof.fun),
    mlrmbo.termination.criterions = function() map(self$termination.criterions, "mlrmbo.term"),
    x.ids = function() getXNames(self$smoof.fun),
    y.ids = function() getYNames(getNumberOfObjectives(self$smoof.fun)),
    resources.walltime = function() 1,
    resources.memory = function() 256,
    resources.cpus = function() 1,
    hash = function() pseudodigest(self),
    minmax = function() if(self$minimize) min else max
  ),

  private = list(
    opt.path = NULL,
    initial.design.n = NULL,
    initial.designs = list(),
    initial.designs.provided = FALSE
  )
)