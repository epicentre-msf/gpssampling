test_that('Check calcul', {

  pop <- c(1L, 1L, 0L, 0L, 0L, 0L, 0L, 2L, 0L, 2L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L)

  r <- calculateQuadrat(qdr_pop = pop, qdr_size = 5L, 10000L)

  expect_equal( # nolint: expect_identical_linter.
    r,
    list(
      pop_m2_avg = 0.02,
      pop_m2_sd = 0.02753,
      pop_m2_se = 0.006156,
      pop_m2_ci = 0.012884,
      pop = 200L,
      pop_i = 129L,
      spd = 0.947368,
      tvalue = 2.093024
    )
  )

  r <- calculateQuadrat(qdr_pop = c(10L, NA), qdr_size = 25L, area = 10000L)

  expect_equal( # nolint: expect_identical_linter.
    r,
    list(
      pop_m2_avg = 0.008,
      pop_m2_sd = 0.011314,
      pop_m2_se = 0.008,
      pop_m2_ci = 0.10165,
      pop = 80L,
      pop_i = 1016L,
      spd = 10L,
      tvalue = 12.706205
    )
  )

  r <- calculateQuadrat(
    qdr_pop = c(11L, 15L, 45L, 25L, 68L, 40L, 37L, 24L, 34L, 29L, 23L, 39L, 53L, 29L, 41L, 27L, 7L, 18L, 21L, 15L, 18L, 16L, 24L, 8L, 7L, 12L, 23L, 29L, 25L, 26L),
    qdr_size = 25L, area = 182541L
  )

  expect_equal( # nolint: expect_identical_linter.
    r,
    list(
      pop_m2_avg = 0.04208,
      pop_m2_sd = 0.022322,
      pop_m2_se = 0.004075,
      pop_m2_ci = 0.008335,
      pop = 7681L,
      pop_i = 1521L,
      spd = 7.40042,
      tvalue = 2.04523
    )
  )

})
