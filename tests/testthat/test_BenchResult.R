context("BenchResult")

test_that("BenchResult works", {
  benchmark = generateSimpleBenchmark(makeSphereFunction(1))

  n = length(benchmark$thresholds)
  n.max = benchmark$termination.criterions$evals$vars$max.evals

  results = list(
    ideal.steps = benchmark$thresholds, # best
    subideal.steps = benchmark$thresholds + 1e-05, # a bit worse
    worse = max(benchmark$thresholds) + benchmark$thresholds + 1e-05, # all above threshold
    half = {
      too.good = benchmark$thresholds <= median(benchmark$thresholds)
      c(benchmark$thresholds[!too.good], rep(max(benchmark$thresholds + 0.1), sum(too.good)))    
    }, # half of the way
    direct.hit1 = rep(min(benchmark$thresholds), n), #all best perf
    direct.hit2 = rev(benchmark$thresholds), #best at begining then down
    direct.half.hit = rep(median(benchmark$thresholds), n) #median always
  )

  results = lapply(results, function(y) data.table(y = y, dob = seq_along(y), x = sqrt(y)))
  results = Map(partial(BenchResult$new, benchmark = benchmark), algo.name = names(results), op.dt = results)
  
  expect_equal(results$ideal.steps$threshold.performances$nev.progress, (1:n) / n.max)
  expect_equal(results$subideal.steps$threshold.performances$nev.progress, c(2:n, Inf) / n.max )
  expect_equal(results$worse$threshold.performances$nev.progress, rep(Inf, n))
  expect_equal(results$half$threshold.performances$nev.progress, c(1:5, rep(Inf, n-5)) / n.max)
  expect_equal(results$direct.hit1$threshold.performances$nev.progress, rep(1, n) / n.max)
  expect_equal(results$direct.hit2$threshold.performances$nev.progress, rep(1, n) / n.max)
  expect_equal(results$direct.half.hit$threshold.performances$nev.progress, c(rep(1, 6), rep(Inf, n-6)) / n.max)

  expect_true(results$ideal.steps$threshold.auc < results$subideal.steps$threshold.auc)
  expect_equal(results$worse$threshold.auc, 1)
  expect_numeric(results$half$threshold.auc, lower = 0.35, upper = 0.65)
  expect_equal(results$direct.hit1$threshold.auc, results$direct.hit2$threshold.auc)
  expect_numeric(results$direct.half.hit$threshold.auc, lower = 0.45, upper = 0.55)
  class(results$half$stepfun)
  for (i in seq_along(results)) {
    expect_numeric(results[[i]]$threshold.auc, 0, 1)
    expect_class(results[[i]]$stepfun, "stepfun")
    # plot(results[[i]]$stepfun, ylim = c(0,1), xlim = c(0,1))
  }
  
})
