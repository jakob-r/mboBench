#' @export
getYNames = function(d) {
  if (d == 1){
    return(y)
  } else {
    return(paste("y", seq_len(d), sep = "_"))
  }
}

#' @export
getXNames = function(x) {
  UseMethod("getXNames")
}

#' @export
getXNames.ParamSet = function(x) {
  getParamIds(x, repeated = TRUE, with.nr = TRUE)
}

#' @export
getXNames.smoof_function = function(x) {
  getXNames(getParamSet(x))
}

#' @export
getXNames.Benchmark = function(x) {
  x$x.ids
}

