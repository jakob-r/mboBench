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
  deenv = function(x, level = 1) {
    if (any(class(x) %in% setdiff(allowed, "list"))) {
      res = digest.fun(x)
    } else if (level <= 3) {
      res = lapply(as.list(x), function(z) deenv(z, level + 1)) 
    } else {
      res = ""
    }
    return(res)
  }
  digest.fun(deenv(obj))
}