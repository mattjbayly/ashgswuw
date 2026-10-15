bg <- ashgswuw:::build_geometry
gas <- ashgswuw:::geometry_at_stage

test_that("bankfull area equals width x mean depth for a range of b", {
  for (b in c(0.05, 0.2, 0.5, 0.8, 0.95)) {
    g <- bg(10, 0.6, b = b)
    s <- gas(g, 0)
    expect_equal(s$flow_area_m2, 10 * 0.6, tolerance = 1e-12)
    expect_equal(s$wetted_width_m, 10)
    expect_equal(s$mean_depth_m, 0.6)
  }
})

test_that("default b and dmax follow the paper", {
  g <- bg(14.36, 0.705)
  expect_equal(g$b, 14.36 / 0.705 / 100)
  expect_equal(g$dmax_m, 0.705 / (1 - g$b))
  expect_identical(g$b_source, "estimated")
  expect_identical(bg(10, 0.5, b = 0.3)$b_source, "user")
})

test_that("bankfull perimeter equals the two segment lengths plus the vertical bank", {
  Wb <- 10; db <- 0.6; b <- 0.3; dmax <- db / (1 - b)
  g <- bg(Wb, db, b = b)
  expected <- sqrt((b * Wb)^2 + db^2) + sqrt(((1 - b) * Wb)^2 + (dmax - db)^2) + dmax
  expect_equal(gas(g, 0)$wetted_perimeter_m, expected)
})

test_that("drawdown reduces width, area and perimeter monotonically to zero", {
  g <- bg(10, 0.6, b = 0.3)
  dd <- seq(0, g$dmax_m, length.out = 50)
  s <- lapply(dd, gas, geom = g)
  W <- vapply(s, `[[`, 1, "wetted_width_m")
  A <- vapply(s, `[[`, 1, "flow_area_m2")
  P <- vapply(s, `[[`, 1, "wetted_perimeter_m")
  expect_true(all(diff(W) <= 1e-12))
  expect_true(all(diff(A) <= 1e-12))
  expect_true(all(diff(P) <= 1e-12))
  expect_equal(tail(W, 1), 0)
  expect_equal(tail(A, 1), 0)
  expect_equal(tail(P, 1), 0)
})

test_that("a stage below the first vertex wets only the deeper segment", {
  g <- bg(10, 0.6, b = 0.3)   # dmax = 0.857; first vertex at depth 0.6
  s <- gas(g, 0.7)            # water surface 0.1 m above dmax, below depth-0.6 vertex
  expect_lt(s$wetted_width_m, (1 - 0.3) * 10)
  expect_gt(s$wetted_width_m, 0)
})

test_that("b out of range is reported", {
  expect_identical(bg(120, 1)$status, "b_out_of_range")   # Wb/db = 120 -> b = 1.2
  expect_identical(bg(10, 1, b = 0)$status, "b_out_of_range")
  expect_identical(bg(10, 1, b = 1)$status, "b_out_of_range")
  expect_identical(bg(10, 1, b = -0.2)$status, "b_out_of_range")
  expect_identical(bg(10, 1, b = NA_real_)$status, "invalid")
})
