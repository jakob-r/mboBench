getYNames = function(d) {
  if (d == 1){
    return("y")
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

getRandomSeed = function() {
  if (!exists(".Random.seed")) runif(1)
  .Random.seed
}

# calculates a digest on a R6 object which is not so easy because of all these environments.
pseudodigest = function(obj, digest.fun.name = "sha1") {
  digest.fun = getFunction(digest.fun.name)
  allowed = as.character(methods(digest.fun.name))
  allowed = stri_replace_first_fixed(allowed, pattern = paste0(digest.fun.name, "."), replacement = "")
  digestable = list(
    id = obj$id,
    y.ids = obj$y.ids,
    x.ids = obj$x.ids,
    par.set = unlist(obj$par.set),
    dim = obj$dim,
    minimize = obj$minimize,
    values = obj$values,
    tags = obj$tags,
    threasholds = obj$threasholds,
    expensive = obj$expensive,
    termination.criterions = map(obj$termination.criterions, "vars"),
    smoof.fun = deparse(obj$smoof.fun)
  )
  digest.fun(digestable)
}