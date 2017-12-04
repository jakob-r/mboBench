getYNames = function(d) {
  if (d == 1){
    return(y)
  } else {
    return(paste("y", seq_len(d), sep = "_"))
  }
}

getXNames = function(x) {
  UseMethod("getXNames")
}

getXNames.ParamSet = function(x) {
  getParamIds(x, repeated = TRUE, with.nr = TRUE)
}

getXNames.smoof_function = function(x) {
  getXNames(getParamSet(x))
}

getXNames.Benchmark = function(x) {
  x$x.ids
}

