Benchmark = R6Class(
  classname = "Benchmark",
  public = list(

    # member variables
    id = NULL,
    smoof.fun = NULL,
    termination.criterions = NULL,
    expensive = NULL,
    threasholds = NULL,
    initial.design.n = NULL,
    tags = NULL,
    values = NULL,

    # constructor
    initialize = function(id = NULL, smoof.fun, termination.criterions, expensive = FALSE, threasholds, initial.designs = NULL, initial.design.n = NULL, tags = character(0L), values = NULL) {

      assertFunction(smoof.fun)
      self$smoof.fun = assertClass(smoof.fun, "smoof_function")

      assertCharacter(id, any.missing = FALSE, null.ok = TRUE)
      self$id = id %??% getID(smoof.fun)

      self$termination.criterions = assertList(termination.criterions, types = "TerminationCriterion")

      self$expensive = assertFlag(expensive)

      self$threasholds = assertNumeric(threasholds, any.missing = FALSE)

      assert_true(xor(is.null(initial.designs), is.null(initial.design.n)))
      private$initial.designs = assertList(initial.designs, "data.frame", null.ok = TRUE)
      if (!is.null(initial.designs)) {
        init.design.lenghts = unique(sapply(initial.designs, nrow))
        if (length(unique(init.design.lenghts)) != 1) {
          stopf("The initial designs all have to be of the same length and not: %s", convertToShortString(init.design.lenghts, clip.len = 64))
        }
      }
      self$initial.design.n = assertInt(initial.design.n, null.ok = TRUE, lower = 0)

      self$tags = assertCharacter(tags, any.missing = FALSE, null.ok = TRUE)

      self$values = assertList(values, null.ok = TRUE, names = "named")
    },

    # public methods
    getInitialDesign = function(i) {
      if (!is.null(private$initial.designs)) {
        if (i <= length(private$initial.designs)) {
          return(private.initial.designs[[i]])
        } else {
          stopf("An initial.design for index %i is not provided.", i)
        }
      } else {
        old.seed = .Random.seed
        set.seed(i)
        on.exit({ .Random.seed <<- old.seed })
        res = private$design.generator()
        return(res)
      }
    }
  ),

  active = list(
    minimize = function() shouldBeMinimized(self$smoof.fun),
    dim = function() getNumberOfParameters(self$smoof.fun),
    par.set = function() getParamSet(self$smoof.fun),
    mlrmbo.termination.criterions = function() lapply(self$termination.criterions, function(z) z$mlrmbo.term)
  ),

  private = list(
    initial.designs = NULL,
    design.generator = function() {
      generateDesign(n = self$initial.design.n, par.set = getParamSet(self$smoof.fun), fun = lhs::maximinLHS)
    }
  )
)