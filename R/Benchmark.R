Benchmark = R6Class(
  classname = "Benchmark",
  public = list(

    # member variables
    smoof.fun = NULL,
    termination.criterions = NULL,
    expensive = NULL,
    threasholds = NULL,
    initial.design.n = NULL,
    tags = NULL,

    # constructor
    initialize = function(smoof.fun, termination.criterions, expensive = FALSE, threasholds, initial.designs = NULL, initial.design.n = NULL, tags = character(0L)) {

      assertFunction(smoof.fun)
      slef$smoof.fun = assertClass(smoof.fun, "smoof_function")

      self$termination.criterions = assertList(termination.criterions, types = "TerminationCriterion")

      self$expensive = assertFlag(expensive)

      self$threasholds = assertNumeric(threasholds, any.missing = FALSE)

      private$initial.designs = assertList(initial.designs, "data.frame", null.ok = TRUE)
      init.design.lenghts = unique(sapply(initial.designs, nrow))
      if (length(unique(init.design.lenghts)) != 1) {
        stopf("The initial designs all have to be of the same length and not: %s", convertToShortString(init.design.lenghts, clip.len = 64))
      }

      self$initial.design.n = assertInt(initial.design.n, null.ok = TRUE, lower = 0)

      self$tags = assertCharacter(tags, any.missing = FALSE)
    },

    # public methods
    public = list(
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

    private = list(
      initial.designs = NULL,
      design.generator = function() {
        generateDesign(n = self$initial.design.n, par.set = getParamSet(self$smoof.fun), fun = lhs::maximinLHS())
      }
    )
  )
)