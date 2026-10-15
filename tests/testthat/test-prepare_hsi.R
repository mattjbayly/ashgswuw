test_that("named columns are found and sorted ascending", {
  x <- data.frame(HSI = c(0.4, 1, 0), Depth = c(0.9, 0.3, 0))
  out <- prepare_hsi(x, "depth")
  expect_s3_class(out, "hsi_curve")
  expect_identical(names(out), c("depth", "hsi"))
  expect_equal(out$depth, c(0, 0.3, 0.9))
  expect_equal(out$hsi, c(0, 1, 0.4))
  expect_identical(attr(out, "variable"), "depth")
})

test_that("positional columns are used with a message when names are absent", {
  x <- data.frame(a = c(0, 0.5), b = c(0, 1))
  expect_message(out <- prepare_hsi(x, "velocity"), "column 1")
  expect_equal(out$velocity, c(0, 0.5))
})

test_that("NA rows are dropped and hsi clamped", {
  x <- data.frame(depth = c(0, NA, 0.5, 1), hsi = c(0, 1, 1.2, -0.1))
  expect_message(out <- prepare_hsi(x, "depth"), "dropped 1 row")
  expect_equal(nrow(out), 3)
  expect_equal(out$hsi, c(0, 1, 0))
})

test_that("duplicates collapse to mean", {
  x <- data.frame(depth = c(0, 0.5, 0.5, 1), hsi = c(0, 0.2, 0.8, 1))
  out <- suppressMessages(prepare_hsi(x, "depth"))
  expect_equal(out$depth, c(0, 0.5, 1))
  expect_equal(out$hsi, c(0, 0.5, 1))
})

test_that("NULL with message on unusable input", {
  expect_message(expect_null(prepare_hsi(data.frame(depth = 0.3, hsi = 1), "depth")), "two complete rows")
  expect_message(expect_null(prepare_hsi(data.frame(depth = c(-0.1, 0.3), hsi = c(0, 1)), "depth")), "non-negative")
  expect_message(expect_null(prepare_hsi(list(1, 2), "depth")), "data frame")
  expect_message(expect_null(prepare_hsi(data.frame(depth = c(NA, NA), hsi = c(1, 1)), "depth")))
})

test_that("an hsi_curve passes through, mismatched variable fails", {
  cur <- prepare_hsi(depth_toy, "depth")
  expect_identical(prepare_hsi(cur, "depth"), cur)
  expect_message(expect_null(prepare_hsi(cur, "velocity")), "not `velocity`")
})

test_that("interpolation is flat beyond the documented range", {
  cur <- prepare_hsi(data.frame(depth = c(0.2, 0.4), hsi = c(0.5, 1)), "depth")
  expect_equal(ashgswuw:::hsi_at(cur, c(0, 0.3, 5)), c(0.5, 0.75, 1))
})
