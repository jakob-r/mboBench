#' @title BenchResult Class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' BenchResult Class
#' 
#' @export
BenchResult = R6Class(
  classname = "BenchResult",
  public = list(

    # member variables
    id = NULL,
    benchmark.hash = NULL,
    op.dt = NULL,
    values = NULL,
    repl = NULL,
    executed = FALSE,
    algo.name = NULL,
    algo.params = NULL,

    # constructor
    initialize = function(id = NULL, benchmark, algo.name = NULL, algo.params = NULL, op.dt = NULL, opt.path = NULL, values = NULL, repl = NULL) {

      assertClass(benchmark, "Benchmark")
      assertCharacter(id, any.missing = FALSE, null.ok = TRUE)
      assertInt(repl, null.ok = TRUE)
      assertString(algo.name, null.ok = TRUE)
      assertList(algo.params, null.ok = TRUE)
      
      assertDataTable(op.dt, null.ok = TRUE)
      assertClass(opt.path, "OptPath", null.ok = TRUE)
      assertTRUE(xor(is.null(op.dt), is.null(opt.path)))

      if (!is.null(opt.path)) {
        op.dt = as.data.frame(as.data.table(opt.path))
      }

      par.set = getParamSet(benchmark$smoof.fun)
      x.ids = benchmark$x.ids
      y.ids = benchmark$y.ids

      assertSubset(x.ids, colnames(op.dt))
      assertSubset(y.ids, colnames(op.dt))

      self$id = id %??% benchmark$id
      self$values = assertList(values, null.ok = TRUE)
      self$benchmark.hash = benchmark$hash
      self$op.dt = op.dt
      self$repl = repl %??% 1
      self$algo.name = algo.name
      self$algo.params = algo.params
    },

    # public methods
    setExecutorValues = function(fixed.args, args, values) {
      self$executed = TRUE
      assertList(values, names = "named")
      self$values$executor = list(fixed.args = fixed.args, args = args, values = values)
    }
  )
)