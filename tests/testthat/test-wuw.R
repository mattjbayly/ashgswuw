test_that("a complete result is returned with the documented structure", {
  res <- harris_estimate(1, reach_length_m = 150, joint = TRUE)
  expect_s3_class(res, "wuw_result")
  expect_named(res, c("wuw_m", "wetted_width_m", "wuw_fraction", "wua_m2", "inputs",
                      "geometry", "hydraulics", "depth_bins", "velocity_bins", "joint",
                      "diagnostics", "qaqc", "qaqc_pass", "assumptions", "model_version"))
  expect_equal(nrow(res$depth_bins), 50)
  expect_equal(dim(res$joint), c(50, 50))
  expect_equal(sum(res$joint), res$wuw_m)
  expect_equal(sum(res$depth_bins$prob), 1)
  expect_equal(sum(res$velocity_bins$prob), 1)
  expect_true(res$wuw_m > 0 && res$wuw_m < res$wetted_width_m)
  expect_true(res$wuw_fraction > 0 && res$wuw_fraction < 1)
  expect_equal(res$wua_m2, 150 * res$wuw_m)
  expect_equal(res$geometry$bankfull_discharge_m3s, 18.8157351182, tolerance = 1e-9)
  expect_equal(res$hydraulics$smix, 0.1846, tolerance = 1e-3)
  expect_length(res$assumptions, 13)
  expect_true(any(grepl("15 m3/s or less", res$assumptions)))
  expect_match(res$model_version, "ASHGS direct-geometry")
  expect_output(print(res), "WUW = ")
})

test_that("physical bin bounds scale with mean depth and velocity", {
  res <- harris_estimate(1)
  expect_equal(res$depth_bins$upper_m, res$depth_bins$rel_upper * res$hydraulics$mean_depth_m)
  expect_equal(res$velocity_bins$centre_ms,
               res$velocity_bins$rel_centre * res$hydraulics$mean_velocity_ms)
})

test_that("constant suitability gives the expected weighted width", {
  base <- function(dh, vh) suppressMessages(wuw_estimate(
    14.36, 0.705, 0.011, 135, 1, depth_hsi = dh, velocity_hsi = vh))
  ones <- base(const_hsi(1), const_hsi(1))
  expect_equal(ones$wuw_m, ones$wetted_width_m)
  expect_equal(ones$wuw_fraction, 1)
  zeros <- base(const_hsi(0), const_hsi(0))
  expect_equal(zeros$wuw_m, 0)
  half <- base(const_hsi(0.5), const_hsi(0.8))
  expect_equal(half$wuw_m, 0.4 * half$wetted_width_m)
})

test_that("WUA scales with reach length and WUW does not", {
  a <- harris_estimate(1, reach_length_m = 100)
  b <- harris_estimate(1, reach_length_m = 200)
  expect_equal(b$wua_m2, 2 * a$wua_m2)
  expect_equal(a$wuw_m, b$wuw_m)
  expect_true(is.na(harris_estimate(1)$wua_m2))
})

test_that("results converge with finer bins", {
  r50 <- harris_estimate(1)
  r400 <- harris_estimate(1, n_bins = 400)
  expect_equal(r400$wuw_m, r50$wuw_m, tolerance = 0.01)
})

test_that("user-supplied b is honoured", {
  res <- harris_estimate(1, b = 0.4)
  expect_equal(res$geometry$b, 0.4)
  expect_identical(res$geometry$b_source, "user")
  expect_equal(res$geometry$bankfull_area_m2, 14.36 * 0.705)
})

test_that("NULL with message for out-of-range inputs", {
  expect_message(expect_null(harris_estimate(0)), "positive")
  expect_message(expect_null(harris_estimate(-1)), "positive")
  expect_message(expect_null(harris_estimate(25)), "exceeds the modelled bankfull")
  expect_message(expect_null(harris_estimate(1, b = 1.2)), "outside the valid range")
  expect_message(expect_null(harris_estimate(1, n_bins = 1)), "n_bins")
  expect_message(expect_null(harris_estimate(1, reach_length_m = 0)), "reach_length_m")
  expect_message(expect_null(suppressWarnings(wuw_estimate(
    120, 1, 0.011, 135, 1, depth_toy, velocity_toy))), "b = 1.2000")
  expect_message(expect_null(wuw_estimate(
    14.36, 0.705, 1.1, 135, 1, depth_toy, velocity_toy)), "must be < 1")
  expect_message(expect_null(wuw_estimate(
    14.36, 0.705, 0.011, NA, 1, depth_toy, velocity_toy)), "d84_mm")
  expect_message(expect_null(wuw_estimate(
    14.36, 0.705, 0.011, 135, 1, data.frame(depth = 0.3, hsi = 1), velocity_toy)),
    "depth_hsi")
})

test_that("a gradient that looks like percent warns", {
  w <- testthat::capture_warnings(harris_estimate(1, gradient_m_per_m = 0.5))
  expect_true(any(grepl("rise/run", w)))
})

test_that("wuw_curve returns one labelled row per discharge", {
  Q <- c(0.25, 1, 5, 25, 0)
  cur <- suppressWarnings(suppressMessages(wuw_curve(Q, 14.36, 0.705, 0.011, 135, depth_toy, velocity_toy,
                                                     reach_length_m = 150, keep_results = TRUE)))
  expect_s3_class(cur, "data.frame")
  expect_equal(nrow(cur), length(Q))
  expect_identical(cur$status, c("ok", "ok", "ok", "above_bankfull", "invalid_discharge"))
  expect_true(all(is.na(cur$wuw_m[4:5])))
  single <- harris_estimate(1, reach_length_m = 150)
  expect_equal(cur$wuw_m[2], single$wuw_m)
  expect_equal(cur$wua_m2[2], single$wua_m2)
  expect_equal(cur$wetted_width_m[2], single$wetted_width_m)
  expect_equal(attr(cur, "bankfull_discharge_m3s"), single$geometry$bankfull_discharge_m3s)
  expect_length(attr(cur, "results"), 5)
  expect_null(attr(cur, "results")[[4]])
  expect_s3_class(attr(cur, "results")[[1]], "wuw_result")
})

test_that("wuw_curve returns NULL for invalid reach inputs", {
  expect_message(expect_null(wuw_curve(1, -1, 0.705, 0.011, 135, depth_toy, velocity_toy)))
  expect_message(expect_null(wuw_curve(numeric(0), 14.36, 0.705, 0.011, 135, depth_toy, velocity_toy)))
})
