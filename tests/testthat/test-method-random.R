test_that('Check function dzipois', {
  skip_if_not_installed('VGAM')
  pop <- c(1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L)

  expect_equal( # nolint: expect_identical_linter.
    dzipois(x = pop, lambda = 2.4, pstr0 = 0.2, log = TRUE),
    VGAM::dzipois(x = pop, lambda = 2.4, pstr0 = 0.2, log = TRUE)
  )
})

test_that('Check calcul 1', {

  set.seed(123L)

  po <- 0.39
  lambda <- 4.09
  n_s <- 756L

  pop <- rbinom(10000L, 1L, 1L - po)
  pop[which(pop > 0L)] <- stats::rpois(sum(pop), lambda)

  smp <- sample(1:10000, 200L)

  r <- calculateSampleZeroInflatedDistribution(
    smp_pop = pop[smp],
    n = 10000L,
    error_confidence = 95L
  )

  expect_in(r$pop, 25300:25320)
  expect_equal(r$pop_i, c(`2.5%` = 21850L, `97.5%` = 28750L)) # nolint: expect_identical_linter.
  expect_equal(r$lambda, 4.077476)
  expect_equal(r$prob, 0.379516)

})

test_that('Check calcul 2', {

  set.seed(123L)

  pop <- c(1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L)

  r <- calculateSampleZeroInflatedDistribution(
    smp_pop = pop,
    n = 10000L,
    error_confidence = 95L
  )

  expect_equal(r$pop, 1808L) # nolint: expect_identical_linter.
  expect_equal(r$prob, -161.43844) # nolint: expect_identical_linter.
  expect_equal(r$pop_i, c(`2.5%` = 0L, `97.5%` = 4545.4545))
  expect_equal(r$lambda, 0.001121)
})
