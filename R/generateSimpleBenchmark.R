generateSimpleBenchmark = function(smoof.fun) {
  old.seed = .Random.seed
  set.seed(1)
  on.exit({ .Random.seed <<- old.seed })

  d = getNumberOfParameters(smoof.fun)
  random.design = generateRandomDesign(n = 500*d, par.set = getParamSet(smoof.fun))
  random.design$y = apply(random.design, 1, smoof.fun)
  threasholds = quantile(random.design$y, seq(0, 1, by = 0.1))
  if (shouldBeMinimized(smoof.fun)) {
    threasholds = rev(threasholds)
  }

  initial.design.n = 4 * d

  # calculate how many evaluations we should allow
  max.evals = 10 * d
  x = random.design$y
  n = length(x)
  x = x - mean(x)
  skew = 1/n * sum(x ^ 3) / (1/(n-1) * sum(x ^ 2)) ^ (3/2)
  if (shouldBeMinimized(smoof.fun)) {
    max.evals = 
  } else {
    max.evals = round(max.evals * skew)
  }
  max.evals = max.evals = initial.design.n
  

  if (!is.null(getGlobalOptimum(smoof.fun)$value)) {
    threasholds = c(threasholds, opt = getGlobalOptimum(smoof.fun)$value)
  }
  
  tags = c("simple.benchmark", getTags(smoof.fun))
  
  Benchmark$new(
    smoof.fun = smoof.fun, 
    termination.criterions = ,
    threasholds = threasholds,
    initial.design.n = initial.design.n,
    tags = tags)
}

if (FALSE) {
  smoof.fun = makeBraninFunction()
  generateSimpleBenchmark(smoof.fun)
}