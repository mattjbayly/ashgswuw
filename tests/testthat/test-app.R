test_that("pasted text with header, trailing commas and blank lines parses", {
  txt <- "velocity,hsi,\n0,0,\n0.03,1,\n\n0.06,1,\n0.12,0.7,\n0.18,0.4\n"
  cur <- parse_hsi_text(txt, "velocity")
  expect_s3_class(cur, "hsi_curve")
  expect_equal(cur$velocity, c(0, 0.03, 0.06, 0.12, 0.18))
  expect_equal(cur$hsi, c(0, 1, 1, 0.7, 0.4))
})

test_that("tab and semicolon delimited text without header parses", {
  expect_equal(parse_hsi_text("0\t0\n0.5\t1\n1\t0.2", "depth")$hsi, c(0, 1, 0.2))
  expect_equal(parse_hsi_text("0;0\n0.5;1", "depth")$depth, c(0, 0.5))
  expect_equal(parse_hsi_text(c("0  0", "0.5  1"), "depth")$depth, c(0, 0.5))
})

test_that("header column order is respected", {
  cur <- parse_hsi_text("HSI,Depth\n0,0\n1,0.3\n0.5,0.9", "depth")
  expect_equal(cur$depth, c(0, 0.3, 0.9))
  expect_equal(cur$hsi, c(0, 1, 0.5))
})

test_that("unusable text returns NULL with a message", {
  expect_message(expect_null(parse_hsi_text("", "depth")))
  expect_message(expect_null(parse_hsi_text("depth,hsi\n0.3,1", "depth")))
  expect_message(expect_null(parse_hsi_text("0\n1\n2", "depth")), "two fields")
  expect_message(expect_null(parse_hsi_text(1:3, "depth")), "character")
})

test_that("the bundled app directory and file exist", {
  dir <- ashgswuw:::wuw_app_dir()
  expect_true(nzchar(dir))
  expect_true(file.exists(file.path(dir, "app.R")))
})

test_that("the app source parses and runs through a full server cycle", {
  skip_if_not_installed("shiny")
  dir <- ashgswuw:::wuw_app_dir()
  expr <- parse(file.path(dir, "app.R"))
  expect_true(length(expr) > 0)

  # Build the app object in a clean environment and exercise the server
  # with the default inputs plus a run click.
  env <- new.env(parent = globalenv())
  app <- NULL
  for (e in expr) app <- eval(e, env)
  expect_s3_class(app, "shiny.appobj")

  shiny::testServer(app, {
    session$setInputs(wb = 14.36, db = 0.705, slope = 0.011, d84 = 135, q = 1, length = 150,
                      mad = NA, use_b = FALSE, b = 0.2, qmin = 0.1, qmax = 20, nq = 10,
                      n_bins = 50, rel_max = 5,
                      depth_txt = "depth,hsi\n0,0\n0.3,1\n1,0.2",
                      vel_txt = "velocity,hsi\n0,0\n0.1,1\n1,0", run = 1)
    m <- model()
    expect_true(m$ok)
    expect_s3_class(m$res, "wuw_result")
    expect_equal(nrow(m$curve), 10)
    expect_equal(m$res$wua_m2, 150 * m$res$wuw_m)

    # Out-of-domain discharge gives a NULL result with a note, not an error.
    session$setInputs(q = 50, run = 2)
    m2 <- model()
    expect_false(m2$ok)
    expect_true(any(grepl("exceeds the modelled bankfull", m2$notes)))

    # Bad HSI text is reported.
    session$setInputs(q = 1, depth_txt = "0.3,1", run = 3)
    m3 <- model()
    expect_false(m3$ok)
    expect_true(any(grepl("Depth HSI", m3$notes)))

    # Above-bankfull discharges in the curve range are explained in plain words.
    session$setInputs(depth_txt = "depth,hsi\n0,0\n0.3,1\n1,0.2", qmax = 40, run = 4)
    m4 <- model()
    expect_true(m4$ok)
    expect_true(any(grepl("above the modelled bankfull discharge", m4$notes)))
    expect_false(any(grepl("see `status`", m4$notes)))

    # MAD is optional, advisory, and feeds the QA/QC table.
    session$setInputs(mad = 20, run = 5)
    m5 <- model()
    expect_true(m5$ok)
    expect_false(m5$res$qaqc_pass[["qaqc_mad_ok"]])
    expect_false(any(grepl("^Warning: QA/QC", m5$notes)))   # shown in its own panel
    html <- as.character(output$status_box$html)
    expect_true(grepl("qaqc_mad_ok", html))
    expect_true(grepl("alert-warning", html))
    tbl <- as.character(output$qaqc_table)
    expect_true(grepl('<span class="qa-fail">FAIL</span>', tbl, fixed = TRUE))
    expect_true(grepl('<span class="qa-pass">PASS</span>', tbl, fixed = TRUE))
    session$setInputs(mad = NA, run = 6)
    m6 <- model()
    expect_true(is.na(m6$res$qaqc_pass[["qaqc_mad_ok"]]))
    expect_true(grepl("alert-success", as.character(output$status_box$html)))
  })
})
