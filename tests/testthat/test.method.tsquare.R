test_that('Check calcul', {

  pts <- tibble::tribble(
    ~d1, ~d2, ~p,
    NA, NA, 0L
  )

  r <- calculateTSquare(
    d1 = pts$d1,
    d2 = pts$d2,
    p = pts$p,
    area = 10000L * 10000L
  )

  expect_true(anyNA(r))

  pts <- tibble::tribble(
    ~d1, ~d2, ~p,
    42L, 11L, 7L,
    23L, 10L, 3L,
    35L, 14L, 5L,
    32L, 18L, 8L,
    14L, 21L, 6L,
    39L, 30L, 4L,
    30L, 27L, 3L,
    62L, 36L, 7L,
    24L, 38L, 6L,
    45L, 15L, 7L,
    17L, 48L, 9L,
    30L, 20L, 2L,
    41L, 61L, 4L,
    45L, 27L, 9L,
    51L, 75L, 8L,
    23L, 31L, 10L,
    52L, 86L, 3L,
    34L, 39L, 7L,
    31L, 100L, 6L,
    35L, 90L, 8L
  )

  r <- calculateTSquare(
    d1 = pts$d1,
    d2 = pts$d2,
    p = pts$p,
    area = 10000L * 10000L
  )

  expect_equal( # nolint: expect_identical_linter.
    r,
    list(
      t = 0.631752,
      z = 2.041095,
      z_p = 0.020621,
      mc = NA_real_,
      mc_p = NA_real_,
      spd = 'not random (aggregated)',
      pop = 153038L,
      pop_i = NA_real_
    )
  )
})


test_that('TSQuare (Report R.Grais Rapid Population Estimation) ', {

  pts <- tibble::tribble(
    ~d1, ~d2, ~p1, ~p2,
    14L, 17L, 8L, 3L,
    6.5, 12L, 13L, 16L,
    16L, 16.2, 12L, 4L,
    4.7, 9.65, 8L, 2L,
    0L, 7.55, 2L, 3L,
    0L, 10.4, 4L, 5L,
    0L, 10.7, 4L, 4L,
    0L, 6.15, 16L, 7L,
    0L, 23.9, 4L, 6L,
    4.9, 18.9, 12L, 4L,
    0L, 14.75, 7L, 2L,
    0L, 14.3, 2L, 7L,
    0L, 18L, 10L, 6L,
    0L, 16.95, 4L, 5L,
    12.9, 7.53, 4L, 6L,
    0L, 16.77, 6L, 11L,
    17.33, 13L, 5L, 0L,
    0L, 16.25, 4L, 3L,
    8.96, 12.6, 6L, 1L,
    0L, 12.175, 5L, 4L,
    0L, 23.25, 7L, 8L,
    0L, 11.9, 12L, 6L,
    0L, 24L, 15L, 0L,
    0L, 15.75, 1L, 2L,
    0L, 25.72, 8L, 5L,
    0L, 20.82, 8L, 7L,
    0L, 16.97, 3L, 3L,
    0L, 4.39, 1L, 7L,
    2.64, 5.09, 6L, 2L,
    0L, 20.35, 4L, 5L,
    3.5, 8.5, 4L, 6L,
    0L, 4.12, 3L, 3L,
    0L, 9.08, 4L, 3L,
    0L, 5.87, 3L, 2L,
    3.9, 12.36, 8L, 9L,
    0L, 14.3, 5L, 1L,
    10.95, 15.5, 5L, 4L,
    19.35, 20.4, 6L, 11L,
    0L, 15.25, 8L, 6L,
    6L, 5.85, 2L, 2L,
    0L, 4.81, 4L, 0L,
    0L, 14.77, 3L, 4L,
    0L, 9.4, 4L, 6L,
    0L, 16L, 5L, 12L,
    0L, 6L, 5L, 9L,
    0L, 3.5, 4L, 3L,
    0L, 14.65, 3L, 9L,
    0L, 5.62, 10L, 7L,
    6.37, 13.27, 9L, 4L,
    0L, 23.45, 1L, 10L,
    5.66, 22L, 12L, 4L,
    0L, 13.17, 9L, 15L,
    0L, 13.58, 6L, 6L,
    0L, 17L, 4L, 5L,
    10.36, 15L, 4L, 1L,
    5.95, 18.32, 9L, 11L,
    5.56, 16.16, 4L, 9L,
    10.45, 22.6, 6L, 7L,
    15.27, 14.1, 8L, 4L,
    14.97, 14.85, 6L, 2L
  )

  r <- calculateTSquare(
    d1 = pts$d1,
    d2 = pts$d2,
    p1 = pts$p1,
    p2 = pts$p2,
    area = 182541L
  )

  expect_equal( # nolint: expect_identical_linter.
    r,
    list(
      t = 0.162181,
      z = -9.064629,
      z_p = 0L,
      mc = NA_real_,
      mc_p = NA_real_,
      spd = 'not random (regular)',
      pop = 9755L,
      pop_i = NA_real_
    )
  )
})

test_that('TSQuare', {
  skip_if_not_installed('spatstat.random')

  size <- 10000L

  n <- 100L

  tests <- tibble::tibble(
    type = rep(c('RS_SMP', 'AGG', 'RGL'), each = 5L),
    size = rep(1:5 * 1000L, 3L),
    hh = NA,
    pop = NA,
    t = NA,
    z = NA,
    z_p = NA,
    mc = NA,
    mc_p = NA,
    spd = NA,
    est_pop = NA,
    est_pop_i = NA
  )

  for (t in seq_len(nrow(tests))) {

    area_sfg <- sf::st_polygon(list(matrix(c(0L, 0L, size, 0L, size, size, 0L, size, 0L, 0L), ncol = 2L, byrow = 2L)))
    area <- sf::st_area(area_sfg) # 10000

    hh_sfg <- switch(tests$type[t],
      RS_SMP = spatstat.random::rpoispp(tests$size[t]),
      AGG = spatstat.random::rMatClust(tests$size[t] / 10L, 0.05, 10L),
      RGL = spatstat.random::rMaternII(tests$size[t], 0.01)
    )
    hh_sfg <-
      sf::st_multipoint(
        matrix(
          cbind(
            hh_sfg$x * size,
            hh_sfg$y * size
          ),
          ncol = 2L, byrow = TRUE
        )
      )
    hh_sfc <- sf::st_sfc(hh_sfg) |> sf::st_cast('POINT')
    hh_sf <- sf::st_sf(pop = sample(1:5, length(hh_sfc), replace = TRUE), geometry = hh_sfc)

    sample_sfg <- sf::st_sample(area_sfg, size = n)
    sample_sf <- sf::st_sf(i = 1:n, geometry = sf::st_sfc(sample_sfg))
    sample_sf$xi <- NA
    sample_sf$yi <- NA
    sample_sf$pi1 <- NA
    sample_sf$pi2 <- NA

    for (i in seq_len(n)) {

      hh <- hh_sf

      p0 <- sample_sfg[i, ]
      p0cc <- sf::st_coordinates(p0)

      d1 <- sf::st_distance(p0, hh)
      d1min <- min(d1)
      pi1 <- which(d1 == d1min)
      p1 <- hh[pi1, ]
      p1cc <- sf::st_coordinates(p1)

      hh <- hh[-pi1, ]

      j <- 0L

      repeat({

        d2 <- sf::st_distance(p1, hh)
        d2min <- min(d2)
        pi2 <- which(d2 == d2min)
        p2 <- hh[pi2, ]
        p2cc <- sf::st_coordinates(p2)

        a <- angle(
          p1cc[1L], p1cc[2L],
          p0cc[1L], p0cc[2L],
          p2cc[1L], p2cc[2L]
        )

        if (a > 90L | j > 5L) {
          break
        }

        hh <- hh[-pi2, ]

        j <- j + 1L

      })

      sample_sf$xi[i] <- d1min
      sample_sf$yi[i] <- d2min
      sample_sf$pi1[i] <- p1$pop
      sample_sf$pi2[i] <- p2$pop

      # plot(area_sfg)
      # plot(hh_sfg, add = TRUE, col = 'black', cex = 0.1)
      # plot(p0, add = TRUE, col = 'red')
      # plot(p1, add = TRUE, col = 'blue')
      # plot(p2, add = TRUE, col = 'green')

    }

    r <- calculateTSquare(
      d1 = sample_sf$xi,
      d2 = sample_sf$yi,
      p = (sample_sf$pi1 + sample_sf$pi2) / 2L,
      area = area
    )

    tests$hh[t] <- nrow(hh_sf)
    tests$pop[t] <- sum(hh_sf$pop)
    tests$t[t] <- r$t
    tests$z[t] <- r$z
    tests$z_p[t] <- r$z_p
    tests$mc[t] <- r$mc
    tests$mc_p[t] <- r$mc_p
    tests$spd[t] <- r$spd
    tests$est_pop[t] <- r$pop
    tests$est_pop_i[t] <- r$pop_i

    # cat(sprintf("%s %s %s %s\n", i, a, d1min, d2min))
    cat(sprintf('Simulation %s\n', t))

  }

  # Verify all simulations produced valid results
  expect_true(all(!is.na(tests$t)))
  expect_true(all(!is.na(tests$z)))
  expect_true(all(tests$spd %in% c(
    'not random (aggregated)', 'not random (regular)',
    'random (homogeneous)', 'random (non-homogeneous)'
  )))
  expect_true(all(tests$est_pop > 0L))
})
