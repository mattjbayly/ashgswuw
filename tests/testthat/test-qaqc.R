qnames <- c("qaqc_mad_ok", "qaqc_b_ok", "qaqc_wd_ratio_ok", "qaqc_width_ok",
            "qaqc_depth_ok", "qaqc_d84_ok", "qaqc_gradient_ok",
            "qaqc_q_le_mad_ok", "qaqc_q_ge_5pct_mad_ok", "qaqc_submergence_ok",
            "qaqc_mean_depth_ok")

test_that("qaqc_limits lists every check with a limit and explanation", {
  lim <- qaqc_limits()
  expect_identical(lim$check, qnames)
  expect_true(all(nzchar(lim$limit)))
  expect_true(all(nchar(lim$explanation) > 40))
})

test_that("Harris Creek at 1 m3/s passes every check; MAD checks NA without MAD", {
  res <- harris_estimate(1)
  expect_s3_class(res$qaqc, "data.frame")
  expect_identical(res$qaqc$check, qnames)
  expect_identical(names(res$qaqc_pass), qnames)
  expect_true(all(is.na(res$qaqc_pass[c("qaqc_mad_ok", "qaqc_q_le_mad_ok", "qaqc_q_ge_5pct_mad_ok")])))
  expect_true(all(res$qaqc_pass[!is.na(res$qaqc_pass)]))
  expect_output(print(res), "all 8 evaluable checks passed")
})

test_that("MAD is advisory: it drives checks and warnings but never NULL", {
  # Harris Creek MAD ~2.9 m3/s (Gronsdahl et al. 2021).
  res <- harris_estimate(1, mad_m3s = 2.94)
  expect_true(res$qaqc_pass[["qaqc_mad_ok"]])
  expect_true(res$qaqc_pass[["qaqc_q_le_mad_ok"]])
  expect_true(res$qaqc_pass[["qaqc_q_ge_5pct_mad_ok"]])
  expect_equal(res$inputs$mad_m3s, 2.94)

  expect_warning(hi <- harris_estimate(1, mad_m3s = 20), "qaqc_mad_ok")
  expect_false(hi$qaqc_pass[["qaqc_mad_ok"]])
  expect_warning(over <- harris_estimate(5, mad_m3s = 2.94), "qaqc_q_le_mad_ok")
  expect_false(over$qaqc_pass[["qaqc_q_le_mad_ok"]])
  expect_warning(low <- harris_estimate(0.1, mad_m3s = 2.94), "qaqc_q_ge_5pct_mad_ok")
  expect_false(low$qaqc_pass[["qaqc_q_ge_5pct_mad_ok"]])
  expect_equal(low$wuw_m, suppressWarnings(harris_estimate(0.1))$wuw_m)   # calculation unchanged

  expect_message(bad <- harris_estimate(1, mad_m3s = -3), "mad_m3s` ignored")
  expect_s3_class(bad, "wuw_result")
  expect_true(is.na(bad$qaqc_pass[["qaqc_mad_ok"]]))
})

test_that("shape, width-to-depth and envelope checks fail where expected", {
  expect_warning(r <- harris_estimate(1, b = 0.75), "qaqc_b_ok")
  expect_false(r$qaqc_pass[["qaqc_b_ok"]])

  # Wb/db = 80 -> b = 0.8: fails b and wd-ratio but still runs.
  expect_warning(r <- suppressMessages(wuw_estimate(40, 0.5, 0.011, 135, 1, depth_toy, velocity_toy)),
                 "qaqc_wd_ratio_ok")
  expect_false(r$qaqc_pass[["qaqc_wd_ratio_ok"]])
  expect_false(r$qaqc_pass[["qaqc_b_ok"]])

  # Envelope: small steep stream with fine bed.
  expect_warning(r <- wuw_estimate(1.2, 0.15, 0.06, 40, 0.05, depth_toy, velocity_toy),
                 "qaqc_width_ok")
  expect_false(r$qaqc_pass[["qaqc_width_ok"]])
  expect_false(r$qaqc_pass[["qaqc_depth_ok"]])
  expect_false(r$qaqc_pass[["qaqc_d84_ok"]])
  expect_false(r$qaqc_pass[["qaqc_gradient_ok"]])
  expect_output(print(r), "FAILED")
})

test_that("submergence and shallow-depth checks respond to low flow", {
  # Harris Creek at a tiny flow: R/D84 < 1 and mean depth < 0.05 m.
  expect_warning(r <- harris_estimate(0.02), "qaqc_submergence_ok")
  expect_false(r$qaqc_pass[["qaqc_submergence_ok"]])
  expect_false(r$qaqc_pass[["qaqc_mean_depth_ok"]])
  expect_lt(r$qaqc$value[r$qaqc$check == "qaqc_submergence_ok"], 1)
  ok <- harris_estimate(5)
  expect_true(ok$qaqc_pass[["qaqc_submergence_ok"]])
})

test_that("wuw_curve carries qaqc_* columns and one consolidated warning", {
  expect_warning(
    cur <- suppressMessages(wuw_curve(c(0.02, 1, 5, 25), 14.36, 0.705, 0.011, 135,
                                      depth_toy, velocity_toy, mad_m3s = 2.94)),
    "QA/QC: .*qaqc_submergence_ok")
  expect_true(all(qnames %in% names(cur)))
  expect_true("qaqc_all_pass" %in% names(cur))
  expect_identical(cur$qaqc_all_pass, c(FALSE, TRUE, FALSE, NA))
  expect_false(cur$qaqc_q_le_mad_ok[3])
  expect_true(cur$qaqc_mad_ok[2])
  expect_true(is.na(cur$qaqc_mad_ok[4]))   # above-bankfull row: nothing evaluated
  cur2 <- suppressWarnings(suppressMessages(wuw_curve(c(1, 5), 14.36, 0.705, 0.011, 135,
                                                      depth_toy, velocity_toy)))
  expect_true(all(is.na(cur2$qaqc_mad_ok)))
  expect_true(all(cur2$qaqc_all_pass))
})
