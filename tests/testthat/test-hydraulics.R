bg <- ashgswuw:::build_geometry
has <- ashgswuw:::hydraulics_at_stage
solve_stage <- ashgswuw:::solve_stage

test_that("Harris Creek bankfull check values are reproduced", {
  # Independent check values from the action plan (section 12).
  g <- bg(14.36, 0.705)
  expect_equal(g$b, 0.2036879433, tolerance = 1e-9)
  expect_equal(g$dmax_m, 0.8853313146, tolerance = 1e-9)
  bf <- has(g, 0, 0.011, 0.135)
  expect_equal(bf$flow_area_m2, 10.1238, tolerance = 1e-9)
  expect_equal(bf$wetted_perimeter_m, 15.3305164934, tolerance = 1e-9)
  expect_equal(bf$mean_velocity_ms, 1.8585644835, tolerance = 1e-9)
  expect_equal(bf$discharge_m3s, 18.8157351182, tolerance = 1e-9)
})

test_that("resistance law uses the dimensionless ratio in both places", {
  res <- ashgswuw:::ferguson_resistance
  r <- 0.5; d <- 0.1
  expect_equal(res(r, d), 6.5 * 2.5 * (r / d) / sqrt(6.5^2 + 2.5^2 * (r / d)^(5 / 3)))
})

test_that("mixture weight and lognormal conversion follow the paper and action plan", {
  expect_equal(ashgswuw:::mixture_weight(1), plogis(-4.72))
  expect_equal(ashgswuw:::.sdlog_depth, 0.8848919, tolerance = 1e-7)
  expect_equal(ashgswuw:::.sdlog_velocity, 0.9392309, tolerance = 1e-7)
  # Lognormal component has arithmetic mean 1.
  sdl <- ashgswuw:::.sdlog_depth
  expect_equal(exp(-sdl^2 / 2 + sdl^2 / 2), 1)
})

test_that("stage solver hits the target discharge and respects the domain", {
  g <- bg(14.36, 0.705)
  for (Q in c(0.05, 0.75, 1, 5, 18)) {
    s <- solve_stage(g, Q, 0.011, 0.135)
    expect_identical(s$status, "ok")
    expect_equal(s$at_q$discharge_m3s, Q, tolerance = 1e-8)
    expect_equal(s$at_q$flow_area_m2 * s$at_q$mean_velocity_ms, Q, tolerance = 1e-8)
    expect_lt(abs(s$at_q$discharge_residual_m3s), 1e-6)
  }
  bf <- has(g, 0, 0.011, 0.135)
  expect_identical(solve_stage(g, bf$discharge_m3s, 0.011, 0.135)$at_q$drawdown_m, 0)
  expect_identical(solve_stage(g, bf$discharge_m3s + 0.01, 0.011, 0.135)$status, "above_bankfull")
})

test_that("Q = 1 reproduces the planning prototype", {
  g <- bg(14.36, 0.705)
  h <- solve_stage(g, 1, 0.011, 0.135)$at_q
  expect_equal(h$wetted_width_m, 11.8786, tolerance = 1e-4)
  expect_equal(h$mean_depth_m, 0.1917, tolerance = 1e-3)
  expect_equal(h$mean_velocity_ms, 0.4391, tolerance = 1e-3)
  expect_equal(h$froude, 0.3202, tolerance = 1e-3)
})

test_that("bin probabilities sum to one and report excluded mass", {
  p <- ashgswuw:::bin_probabilities(50, 5, 0.3, ashgswuw:::.sdlog_depth)
  expect_equal(sum(p$prob), 1)
  expect_length(p$prob, 50)
  expect_true(all(p$prob >= 0))
  expect_gt(p$excluded_below, 0)
  expect_lt(p$excluded_below + p$excluded_above, 0.05)
  expect_equal(p$rel_upper[50], 5)
})
