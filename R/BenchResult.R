#' @title BenchResult Class
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
    benchmark = NULL,
    op.dt = NULL,
    values = NULL,
    repl = NULL,

    # constructor
    initialize = function(id = NULL, benchmark, op.dt = NULL, opt.path = NULL, values = NULL, repl = NULL) {

      assertClass(benchmark, "Benchmark")
      assertCharacter(id, any.missing = FALSE, null.ok = TRUE)
      assertInt(repl, null.ok = TRUE)
      
      assertDataTable(op.dt, null.ok = TRUE)
      assertClass(opt.path, null.ok = TRUE)
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
      self$benchmark = benchmark
      self$op.dt = op.dt
      self$repl = repl %??% 1
    }

    # public methods
  )
)