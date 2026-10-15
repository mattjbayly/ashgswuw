# ashgswuw interactive explorer
#
# Launched by ashgswuw::run_wuw_app(). All calculations go through the
# package functions; this file only collects inputs and draws results.

library(shiny)
library(ashgswuw)

# ---- palette (validated categorical slots + sequential blue ramp) ----------
COL <- list(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a",
            grey = "#52514e", light = "#f0efec", water = "#9ec5f4")
SEQ_BLUE <- c("#fcfcfb", "#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

example_text <- function(file) {
  paste(readLines(system.file("extdata", file, package = "ashgswuw")), collapse = "\n")
}

# Run f(), collecting messages and warnings instead of printing them.
capture_conditions <- function(f) {
  msgs <- character(0); warns <- character(0)
  value <- withCallingHandlers(
    f(),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(value = value, messages = trimws(msgs), warnings = trimws(warns))
}

# Larger, consistent base-graphics styling for every plot.
base_par <- function(cex = 1.3) {
  par(mar = c(4.6, 5, 3, 1.2), mgp = c(3, 0.8, 0), las = 1,
      cex.axis = cex * 0.9, cex.lab = cex, cex.main = cex, font.main = 1,
      col.axis = COL$grey, col.lab = COL$grey, col.main = "#0b0b0b", fg = COL$grey, bty = "l")
}

fmt <- function(x, d = 3) ifelse(is.na(x), "NA", formatC(x, digits = d, format = "f"))

# Plain-language status descriptions for wuw_curve() rows.
status_label <- c(above_bankfull = "above the modelled bankfull discharge",
                  invalid_discharge = "not a positive discharge",
                  solver_failed = "too small for the stage solver")

# ---- input tooltips ----------------------------------------------------------
# Each sidebar input label carries a question-mark icon with a hover tooltip.
TIPS <- list(
  wb = "Bankfull width (m): width of the water surface when the channel is full to the top of its banks, just before water spills onto the floodplain. Reach average.",
  db = "Mean bankfull depth (m): bankfull flow area divided by bankfull width. This is the AVERAGE depth across the section, not the maximum depth and not bank height.",
  slope = "Reach gradient as rise over run (m/m). Enter 0.011 for 1.1 %. Used as the energy slope in the resistance law. Values above 0.2 trigger a warning because they usually mean a percentage was typed.",
  d84 = "D84 (mm): the bed-surface grain size than which 84 % of the surface particles are finer (the coarse end of the surface grain-size distribution). It is the roughness length scale in the Ferguson (2007) flow-resistance law. Converted to metres internally.",
  q = "Discharge of interest (m3/s): the single flow at which the summary, cross-section and distributions are reported. Must be greater than zero and no more than the modelled bankfull discharge.",
  length = "Reach length (m), optional: multiplies WUW to give weighted usable area (WUA, m2). Leave blank for WUW only.",
  use_b = "Shape parameter b describes the cumulative depth distribution: 0 = rectangular, 1 = extremely skewed. By default it is estimated as (bankfull width / mean bankfull depth) / 100. Tick to enter your own value, e.g. from a surveyed section (b = 1 - mean depth / maximum depth).",
  b = "Shape parameter b, 0 < b < 1. Values of 0.1 to 0.7 are typical of fish-bearing BC channels.",
  qmin = "Lowest discharge (m3/s) in the flow-response curves. Must be greater than zero.",
  qmax = "Highest discharge (m3/s) in the flow-response curves. Flows above the modelled bankfull discharge are reported but left out of the curves.",
  nq = "Number of discharges, spaced evenly on a log scale between Q min and Q max.",
  n_bins = "Number of equal bins used to integrate the relative depth and relative velocity distributions (50 in the paper). More bins give a smoother, slightly more exact result.",
  rel_max = "Upper limit of the binned range, as a multiple of the mean depth or mean velocity (5 in the paper). Probability beyond it is dropped and reported.",
  depth_txt = "Depth suitability curve: depth in metres and HSI from 0 (unsuitable) to 1 (optimal), one pair per line. Linear interpolation between points; held flat beyond the first and last points.",
  vel_txt = "Velocity suitability curve: velocity in m/s and HSI from 0 to 1, one pair per line. Velocity means reach-average (water-column) velocity, so use curves defined the same way.",
  mad = "Long-term mean annual discharge (m3/s) from a gauge or regional estimate. OPTIONAL and used only for QA/QC: the method was evaluated on rivers with MAD of about 15 m3/s or less, and its depth/velocity distributions were fitted at 5 to 100 % of MAD. MAD never changes the calculation."
)

tip_label <- function(id, text) {
  tags$span(text, tags$span(class = "tip-icon", `data-toggle` = "tooltip", `data-placement` = "right",
                            title = TIPS[[id]], icon("circle-question")))
}
num <- function(id, label, ...) numericInput(id, tip_label(id, label), ...)

# ---- UI ----------------------------------------------------------------------
ui <- fluidPage(
  useBusyIndicators(),
  tags$head(
    tags$style(HTML("
      .container-fluid { max-width: 1500px; }
      .note { color:#52514e; font-size:0.92em; max-width: 78ch; line-height: 1.5; }
      .intro { max-width: 95ch; }
      textarea { font-family: monospace; font-size: 0.9em; }
      .tab-content { padding-top: 22px; }
      .tab-content h4:first-child { margin-top: 0; }
      .def-table { max-width: 1050px; }
      .def-table td:nth-child(1) { font-weight: 600; white-space: nowrap; width: 24ch; }
      .def-table td:nth-child(2) { font-family: monospace; white-space: nowrap; text-align: right; width: 14ch; }
      .def-table td:nth-child(3) { color:#52514e; max-width: 60ch; line-height: 1.45; }
      .run-box { padding: 10px 12px; margin-bottom: 14px; background:#f0efec; border-radius: 6px; }
      .run-box .btn { font-size: 1.1em; }
      h4 { margin-top: 1.4em; margin-bottom: 0.7em; }
      .checklist { max-width: 85ch; }
      .checklist .checkbox { margin-bottom: 12px; line-height: 1.45; }
      .checklist label { font-weight: normal; }
      .tip-icon { color:#2a78d6; margin-left: 5px; cursor: help; font-size: 0.9em; }
      .tooltip-inner { max-width: 320px; text-align: left; font-size: 0.9em; line-height: 1.4; }
      .plot-caption { margin-top: 10px; }
      .qaqc-box { padding: 10px 12px; margin-bottom: 14px; background:#fff6e5; border: 1px solid #f3d9a4; border-radius: 6px; }
      .qaqc-box .note { margin-bottom: 0; }
      .qaqc-alert ul { margin-top: 8px; }
      .qaqc-alert li { margin-bottom: 6px; max-width: 95ch; }
      .qaqc-table td:nth-child(2) { font-weight: 600; white-space: nowrap; }
      .qa-pass, .qa-fail, .qa-na { display: inline-block; padding: 2px 10px; border-radius: 10px; font-size: 0.9em; }
      .qa-pass { background:#dff3e6; color:#0e6b3a; border: 1px solid #9fd8b5; }
      .qa-fail { background:#fde3e0; color:#a6271a; border: 1px solid #f2a99f; }
      .qa-na   { background:#eeedea; color:#6b6a66; border: 1px solid #d6d4cf; font-weight: normal; }
      .qaqc-table td:nth-child(3), .qaqc-table td:nth-child(4) { font-family: monospace; white-space: nowrap; }
      .qaqc-table td:nth-child(5) { color:#52514e; max-width: 55ch; line-height: 1.4; }
    ")),
    tags$script(HTML("
      $(function () {
        $(document).on('shiny:connected', function () { $('[data-toggle=\"tooltip\"]').tooltip({container: 'body'}); });
        $(document).on('shiny:value', function () { $('[data-toggle=\"tooltip\"]').tooltip({container: 'body'}); });
      });
    "))
  ),
  titlePanel("ashgswuw: weighted usable width explorer"),
  p(class = "note intro",
    "R adaptation of selected functions from the At-a-Station Hydraulic Geometry Simulator (ASHGS), ",
    "original MATLAB by Dr. Brett Eaton, UBC (", a("source", href = "https://blogs.ubc.ca/beaton/working-code/", target = "_blank"),
    "); method by McParland, Eaton and Rosenfeld (2016). The original authors have not reviewed this tool. ",
    "Screening-level physical habitat index only; not an estimate of fish abundance. ",
    "Hover over the ", icon("circle-question", style = "color:#2a78d6;"), " icons for input definitions."),
  sidebarLayout(
    sidebarPanel(width = 4,
      div(class = "run-box",
          actionButton("run", "Run model", class = "btn-primary", icon = icon("play")),
          span(style = "margin-left: 12px;", class = "note", textOutput("last_run", inline = TRUE))),
      h4("Reach inputs"),
      fluidRow(
        column(6, num("wb", "Bankfull width (m)", 14.36, min = 0.01, step = 0.1)),
        column(6, num("db", "Mean bankfull depth (m)", 0.705, min = 0.001, step = 0.01))),
      fluidRow(
        column(6, num("slope", "Gradient (m/m)", 0.011, min = 0.00001, step = 0.001)),
        column(6, num("d84", "D84 (mm)", 135, min = 0.1, step = 1))),
      fluidRow(
        column(6, num("q", "Discharge (m3/s)", 1, min = 0.0001, step = 0.1)),
        column(6, num("length", "Reach length (m)", NA, min = 0, step = 10))),
      checkboxInput("use_b", tip_label("use_b", "Override shape parameter b"), FALSE),
      conditionalPanel("input.use_b",
        num("b", "b (0 < b < 1)", 0.2, min = 0.001, max = 0.999, step = 0.01)),
      h4("Flow-response curves"),
      fluidRow(
        column(4, num("qmin", "Q min", 0.1, min = 0.0001, step = 0.1)),
        column(4, num("qmax", "Q max", 20, min = 0.001, step = 1)),
        column(4, num("nq", "Points", 40, min = 3, max = 400, step = 1))),
      h4("Distribution bins"),
      fluidRow(
        column(6, num("n_bins", "Number of bins", 50, min = 2, max = 1000, step = 1)),
        column(6, num("rel_max", "Relative range max", 5, min = 0.5, step = 0.5))),
      h4("Habitat suitability curves"),
      p(class = "note",
        "Paste two-column tables (comma, tab or semicolon separated). ",
        "A header row with 'depth'/'velocity' and 'hsi' is optional."),
      textAreaInput("depth_txt", tip_label("depth_txt", "Depth HSI (m, hsi)"),
                    example_text("hsi_example_depth.csv"), rows = 8),
      textAreaInput("vel_txt", tip_label("vel_txt", "Velocity HSI (m/s, hsi)"),
                    example_text("hsi_example_velocity.csv"), rows = 8),
      h4("QA/QC only"),
      div(class = "qaqc-box",
        num("mad", tags$span(tags$strong("(OPTIONAL)"), " Mean annual discharge, MAD (m3/s)"),
            NA, min = 0, step = 0.1),
        p(class = "note", strong("Not used in the calculation. "),
          "MAD is compared with the range in which the method was evaluated ",
          "(about 15 m3/s or less) and with the discharge of interest (5 to 100 % of MAD) ",
          "to flag results that fall outside the tested domain. Leave blank to skip those checks.")),
      p(class = "note", textOutput("version", inline = TRUE))
    ),
    mainPanel(width = 8,
      uiOutput("status_box"),
      tabsetPanel(id = "tabs",
        tabPanel("Summary",
          h4("Results at the discharge of interest"),
          tableOutput("stat_table"),
          h4("Geometry and bankfull hydraulics"),
          tableOutput("geom_table"),
          h4("QA/QC checks against the tested domain"),
          p(class = "note", "Advisory only: a failed check does not change the result. ",
            "Thresholds follow Gronsdahl et al. (2021) and Ferguson (2007). ",
            "Decide for yourself whether to proceed when a check fails."),
          tableOutput("qaqc_table")),
        tabPanel("Cross-section",
          checkboxInput("mirror", "Show as a mirrored (symmetric) channel, for intuition only", FALSE),
          plotOutput("xs_plot", height = 480),
          div(class = "note plot-caption",
            p(strong("Why does the section look one-sided?")),
            p("ASHGS does not model a real channel shape. It models the ", em("cumulative depth distribution"),
              " across the reach: the fraction of the wetted width that is shallower than a given depth ",
              "(McParland et al. 2016, Figures 1 and 3)."),
            p("Drawing that distribution as a bed profile sorts the depths from shallowest (left) to deepest ",
              "(right), which is why it looks like half a channel. Width, flow area, mean depth and wetted ",
              "perimeter are the same whichever way it is drawn."),
            p("The mirrored view folds the same distribution about the centreline so the deepest point sits ",
              "mid-channel. It is for intuition only and is not used in any calculation."))),
        tabPanel("Flow response",
          p(class = "note", "Top row: habitat response. Lower rows: wetted width and the reach-average ",
            "hydraulics that drive it. Dashed line = modelled bankfull discharge; dotted line = discharge of interest."),
          fluidRow(
            column(6, plotOutput("p_wuw", height = 320)),
            column(6, plotOutput("p_frac", height = 320))),
          fluidRow(
            column(6, plotOutput("p_width", height = 320)),
            column(6, plotOutput("p_depth", height = 320))),
          fluidRow(
            column(6, plotOutput("p_vel", height = 320)),
            column(6, plotOutput("p_fr", height = 320))),
          br(),
          downloadButton("dl_curve", "Download flow-response table (CSV)")),
        tabPanel("Suitability and distributions",
          h4("Habitat suitability curves (as parsed)"),
          plotOutput("hsi_plot", height = 360),
          h4("Predicted frequency distributions at the discharge of interest"),
          plotOutput("dist_plot", height = 680),
          h4("Joint contribution"),
          plotOutput("joint_plot", height = 460),
          br(),
          downloadButton("dl_bins", "Download depth and velocity bin tables (CSV)")),
        tabPanel("Assumptions checklist",
          p(class = "note", "Tick each assumption after confirming it is acceptable for your reach. ",
            "This list is for review only; it does not change the calculation."),
          uiOutput("checklist"),
          p(strong(textOutput("checked_count", inline = TRUE))))
      )
    )
  )
)

# ---- server ------------------------------------------------------------------
server <- function(input, output, session) {

  output$version <- renderText(model_version())

  model <- eventReactive(input$run, {
    b <- if (isTRUE(input$use_b)) input$b else NULL
    len <- if (is.na(input$length) || input$length <= 0) NULL else input$length
    mad <- if (is.null(input$mad) || is.na(input$mad) || input$mad <= 0) NULL else input$mad

    dh <- capture_conditions(function() parse_hsi_text(input$depth_txt, "depth"))
    vh <- capture_conditions(function() parse_hsi_text(input$vel_txt, "velocity"))
    notes <- c(if (length(dh$messages)) paste("Depth HSI:", dh$messages),
               if (length(vh$messages)) paste("Velocity HSI:", vh$messages))
    if (is.null(dh$value) || is.null(vh$value)) {
      return(list(ok = FALSE, notes = notes, depth_hsi = dh$value, velocity_hsi = vh$value,
                  time = Sys.time()))
    }

    est <- capture_conditions(function() wuw_estimate(
      input$wb, input$db, input$slope, input$d84, input$q, dh$value, vh$value,
      reach_length_m = len, n_bins = input$n_bins, rel_max = input$rel_max,
      b = b, joint = TRUE, mad_m3s = mad))
    # QA/QC warnings are shown in their own panel, not as notes.
    other_warn <- est$warnings[!grepl("^QA/QC", est$warnings)]
    notes <- c(notes, est$messages, if (length(other_warn)) paste("Warning:", other_warn))

    curve <- NULL
    if (is.finite(input$qmin) && is.finite(input$qmax) && input$qmax > input$qmin && input$qmin > 0) {
      qs <- exp(seq(log(input$qmin), log(input$qmax), length.out = max(3, input$nq)))
      cv <- capture_conditions(function() wuw_curve(
        qs, input$wb, input$db, input$slope, input$d84, dh$value, vh$value,
        reach_length_m = len, n_bins = input$n_bins, rel_max = input$rel_max, b = b,
        mad_m3s = mad))
      curve <- cv$value
      if (!is.null(curve)) {
        qc <- grep("^qaqc_", names(curve), value = TRUE)
        qc <- setdiff(qc, "qaqc_all_pass")
        nfail <- vapply(qc, function(cc) sum(!curve[[cc]], na.rm = TRUE), integer(1))
        if (any(nfail > 0)) {
          nfail <- nfail[nfail > 0]
          notes <- c(notes, sprintf(
            "Flow-response curves: QA/QC checks fail for some discharges: %s. The qaqc_* columns in the downloaded table flag each row.",
            paste(sprintf("%s (%d of %d)", names(nfail), nfail, sum(curve$status == "ok")), collapse = "; ")))
        }
      }
      # Replace the package's generic message with an explicit explanation.
      if (!is.null(curve) && any(curve$status != "ok")) {
        for (st in unique(curve$status[curve$status != "ok"])) {
          qq <- curve$discharge_m3s[curve$status == st]
          notes <- c(notes, sprintf(
            "Flow-response curves: %d of %d discharges (%.3g to %.3g m3/s) are %s%s and are left out of the curves.",
            length(qq), nrow(curve), min(qq), max(qq), status_label[[st]],
            if (st == "above_bankfull") sprintf(" (%.3g m3/s)", attr(curve, "bankfull_discharge_m3s")) else ""))
        }
      } else if (is.null(curve)) {
        notes <- c(notes, cv$messages)
      }
    } else {
      notes <- c(notes, "Flow-response curves skipped: Q min must be > 0 and less than Q max.")
    }

    list(ok = !is.null(est$value), res = est$value, curve = curve, notes = unique(notes),
         depth_hsi = dh$value, velocity_hsi = vh$value, time = Sys.time())
  }, ignoreNULL = FALSE)

  output$last_run <- renderText({
    m <- model()
    paste("Results updated", format(m$time, "%H:%M:%S"))
  })

  # ---- status -----------------------------------------------------------------
  output$status_box <- renderUI({
    m <- model()
    if (!m$ok) {
      return(div(class = "alert alert-danger",
                 strong("No result returned (NULL). "), tags$ul(lapply(m$notes, tags$li))))
    }
    q <- m$res$qaqc
    failed <- q[!is.na(q$pass) & !q$pass, ]
    n_eval <- sum(!is.na(q$pass))
    qa_box <- if (nrow(failed)) {
      div(class = "alert alert-warning qaqc-alert",
          strong(sprintf("QA/QC: %d of %d evaluable checks failed at the discharge of interest. ",
                         nrow(failed), n_eval)),
          "The result is still shown; the method was not evaluated under these conditions, so decide whether to proceed.",
          tags$ul(lapply(seq_len(nrow(failed)), function(i) tags$li(
            strong(failed$check[i]), sprintf(" (value %s; limit %s): ", fmt(failed$value[i], 3), failed$limit[i]),
            failed$explanation[i]))))
    } else {
      div(class = "alert alert-success",
          strong(sprintf("QA/QC: all %d evaluable checks passed. ", n_eval)),
          if (is.null(m$res$inputs$mad_m3s)) "MAD not supplied, so the three MAD-based checks were not evaluated." else NULL)
    }
    notes_box <- if (length(m$notes)) {
      div(class = "alert alert-info", strong("Notes "), tags$ul(lapply(m$notes, tags$li)))
    } else NULL
    tagList(qa_box, notes_box)
  })

  output$qaqc_table <- renderTable({
    m <- model(); req(m$ok); q <- m$res$qaqc
    data.frame(
      Check = q$check,
      Result = ifelse(is.na(q$pass), '<span class="qa-na">not evaluated</span>',
                      ifelse(q$pass, '<span class="qa-pass">PASS</span>', '<span class="qa-fail">FAIL</span>')),
      Value = fmt(q$value, 3),
      Limit = q$limit,
      Explanation = q$explanation,
      stringsAsFactors = FALSE)
  }, striped = TRUE, class = "def-table qaqc-table", sanitize.text.function = identity)

  # ---- summary tables ---------------------------------------------------------
  output$stat_table <- renderTable({
    m <- model(); req(m$ok); r <- m$res; h <- r$hydraulics; dg <- r$diagnostics
    data.frame(
      Parameter = c("Discharge, Q", "Weighted usable width, WUW", "Total wetted width, W",
                    "Usable fraction, WUW / W", "Weighted usable area, WUA",
                    "Drawdown", "Flow area, A", "Mean depth, d", "Wetted perimeter, P",
                    "Hydraulic radius, R", "Mean velocity, v", "Froude number, Fr",
                    "Mixing weight, Smix", "Excluded probability (depth)",
                    "Excluded probability (velocity)"),
      Value = c(paste(fmt(r$inputs$discharge_m3s, 4), "m3/s"), paste(fmt(r$wuw_m), "m"),
                paste(fmt(r$wetted_width_m), "m"), fmt(r$wuw_fraction),
                if (is.na(r$wua_m2)) "NA" else paste(fmt(r$wua_m2, 1), "m2"),
                paste(fmt(h$drawdown_m), "m"), paste(fmt(h$flow_area_m2), "m2"),
                paste(fmt(h$mean_depth_m), "m"), paste(fmt(h$wetted_perimeter_m), "m"),
                paste(fmt(h$hydraulic_radius_m), "m"), paste(fmt(h$mean_velocity_ms), "m/s"),
                fmt(h$froude), fmt(h$smix),
                fmt(dg$depth_prob_excluded_below_zero + dg$depth_prob_excluded_above_rel_max, 4),
                fmt(dg$velocity_prob_excluded_below_zero + dg$velocity_prob_excluded_above_rel_max, 4)),
      Description = c(
        "The target discharge entered in the sidebar.",
        "Wetted width multiplied by the average combined depth x velocity suitability. Equivalent width of fully suitable habitat per metre of channel.",
        "Width of the water surface at Q on the equivalent section.",
        "Share of the wetted width that is usable (0 to 1). WUW divided by W.",
        "Reach length x WUW. NA when no reach length is entered.",
        "How far the water surface sits below the bankfull level at Q.",
        "Cross-sectional area of flowing water at Q.",
        "Flow area divided by wetted width. The centre of the depth distribution.",
        "Length of submerged bed and bank at Q (water surface excluded).",
        "Flow area divided by wetted perimeter; the length scale in the resistance law.",
        "Reach-average velocity from the Ferguson (2007) resistance law; Q = A x v.",
        "v / sqrt(g d). Higher at larger flows; controls how skewed the distributions are.",
        "Weight of the lognormal (skewed) component in the depth and velocity distributions; logit(Smix) = -4.72 - 2.84 ln(Fr).",
        "Probability of the depth distribution lying below 0 or above the binned range, dropped before renormalising.",
        "Same, for the velocity distribution."),
      stringsAsFactors = FALSE)
  }, striped = TRUE, class = "def-table", sanitize.text.function = identity)

  output$geom_table <- renderTable({
    m <- model(); req(m$ok); g <- m$res$geometry
    data.frame(
      Parameter = c("Shape parameter, b", "Maximum depth, dmax", "Bankfull flow area",
                    "Bankfull wetted perimeter", "Bankfull hydraulic radius",
                    "Bankfull mean velocity", "Modelled bankfull discharge"),
      Value = c(fmt(g$b, 4), paste(fmt(g$dmax_m), "m"), paste(fmt(g$bankfull_area_m2), "m2"),
                paste(fmt(g$bankfull_perimeter_m), "m"), paste(fmt(g$bankfull_hydraulic_radius_m), "m"),
                paste(fmt(g$bankfull_velocity_ms), "m/s"), paste(fmt(g$bankfull_discharge_m3s), "m3/s")),
      Description = c(
        sprintf("Index of channel shape, 0 = rectangular, 1 = extremely skewed (%s). Default estimate is (Wb / db) / 100; 0.1 to 0.7 is typical of fish-bearing BC channels.",
                if (g$b_source == "user") "entered by you" else "estimated from Wb / db"),
        "Deepest point of the equivalent section, db / (1 - b).",
        "Bankfull width x mean bankfull depth, by construction.",
        "Submerged bed and bank length when the water is at bankfull.",
        "Bankfull flow area divided by bankfull wetted perimeter.",
        "Reach-average velocity at bankfull from the resistance law.",
        "Discharge the equivalent section carries at bankfull. Target discharges above this return no result."),
      stringsAsFactors = FALSE)
  }, striped = TRUE, class = "def-table")

  # ---- cross-section ----------------------------------------------------------
  output$xs_plot <- renderPlot({
    m <- model(); req(m$ok); r <- m$res
    sec <- r$geometry$section; eta <- -r$hydraulics$drawdown_m
    Wb <- r$inputs$bankfull_width_m
    bed <- sec[1:3, ]
    xs <- seq(0, Wb, length.out = 400)
    zb <- approx(bed$x_m, bed$z_m, xout = xs)$y
    mirror <- isTRUE(input$mirror)
    if (mirror) {
      # Fold the sorted distribution about the centreline so the deepest point
      # sits mid-channel: each half carries half of every width increment, so
      # width, area and the depth distribution are unchanged.
      xs <- c(xs / 2, Wb - rev(xs) / 2)
      zb <- c(zb, rev(zb))
      outline_x <- c(0, xs, Wb); outline_z <- c(0, zb, 0)
    } else {
      outline_x <- c(xs, Wb); outline_z <- c(zb, 0)
    }
    dmax <- r$geometry$dmax_m
    base_par(1.35)
    plot(outline_x, outline_z, type = "n", xlab = "Distance across channel (m)",
         ylab = "Elevation relative to bankfull (m)",
         ylim = c(-dmax * 1.12, 0.42 * dmax),
         main = sprintf("Equivalent section at Q = %.3g m3/s%s", r$inputs$discharge_m3s,
                        if (mirror) " (mirrored view)" else ""))
    inwater <- zb < eta
    if (any(inwater)) {
      polygon(c(xs[inwater], rev(xs[inwater])), c(zb[inwater], rep(eta, sum(inwater))),
              col = COL$water, border = NA)
    }
    lines(outline_x, outline_z, lwd = 2.5, col = "#0b0b0b")
    abline(h = 0, lty = 2, col = COL$grey, lwd = 1.5)
    abline(h = eta, lwd = 2.5, col = COL$blue)
    # Labels: bankfull just above its line, water surface just above its line,
    # each on two lines so they fit at ordinary widths.
    text(0.5 * Wb, 0,
         sprintf("Bankfull level\nWb (bankfull width) = %.2f m, db (mean bankfull depth) = %.3f m, Qbf = %.2f m3/s",
                 Wb, r$inputs$bankfull_depth_m, r$geometry$bankfull_discharge_m3s),
         adj = c(0.5, -0.25), col = COL$grey, cex = 1.1)
    text(0.5 * Wb, eta,
         sprintf("Water surface at Q\nW (wetted width) = %.2f m, d (mean depth) = %.3f m, drawdown = %.3f m",
                 r$wetted_width_m, r$hydraulics$mean_depth_m, r$hydraulics$drawdown_m),
         adj = c(0.5, -0.25), col = COL$blue, cex = 1.1)
    text(if (mirror) 0.5 * Wb else 0.97 * Wb, -dmax,
         sprintf("dmax (maximum depth) = %.3f m", dmax), adj = c(if (mirror) 0.5 else 1, 1.6),
         col = COL$grey, cex = 1.1)
    legend("top", inset = 0.01, bty = "n", cex = 1.05, text.col = COL$grey, horiz = TRUE,
           legend = c("bed", "bankfull", "water surface at Q"),
           col = c("#0b0b0b", COL$grey, COL$blue), lty = c(1, 2, 1), lwd = c(2.5, 1.5, 2.5))
  })

  # ---- flow response ----------------------------------------------------------
  curve_ok <- reactive({
    m <- model(); req(m$ok, !is.null(m$curve))
    cv <- m$curve[m$curve$status == "ok", ]
    validate(need(nrow(cv) > 1, "No discharges in the chosen range are within the model domain."))
    cv
  })

  flow_plot <- function(ycol, lab, col, main, point_y = NULL) {
    renderPlot({
      m <- model(); cv <- curve_ok(); q0 <- m$res$inputs$discharge_m3s
      base_par(1.3)
      plot(cv$discharge_m3s, cv[[ycol]], type = "n", log = "x",
           xlab = "Discharge (m3/s, log scale)", ylab = lab, main = main)
      grid(col = COL$light, lty = 1)
      abline(v = attr(m$curve, "bankfull_discharge_m3s"), lty = 2, col = COL$grey)
      abline(v = q0, lty = 3, col = COL$grey)
      lines(cv$discharge_m3s, cv[[ycol]], lwd = 2.5, col = col)
      if (!is.null(point_y)) {
        y0 <- point_y(m$res)
        points(q0, y0, pch = 19, col = col, cex = 1.5)
        text(q0, y0, fmt(y0, 2), pos = 4, col = COL$grey, cex = 1.1)
      }
    })
  }
  output$p_wuw   <- flow_plot("wuw_m", "WUW (m)", COL$blue, "Weighted usable width", function(r) r$wuw_m)
  output$p_width <- flow_plot("wetted_width_m", "Wetted width (m)", COL$orange, "Total wetted width", function(r) r$wetted_width_m)
  output$p_frac  <- flow_plot("wuw_fraction", "WUW / wetted width", COL$aqua, "Usable fraction of wetted width", function(r) r$wuw_fraction)
  output$p_depth <- flow_plot("mean_depth_m", "Mean depth (m)", COL$blue, "Reach-average depth", function(r) r$hydraulics$mean_depth_m)
  output$p_vel   <- flow_plot("mean_velocity_ms", "Mean velocity (m/s)", COL$orange, "Reach-average velocity", function(r) r$hydraulics$mean_velocity_ms)
  output$p_fr    <- flow_plot("froude", "Froude number", COL$aqua, "Froude number (sets the mixing weight)", function(r) r$hydraulics$froude)

  # ---- suitability and distributions -----------------------------------------
  output$hsi_plot <- renderPlot({
    m <- model()
    validate(need(!is.null(m$depth_hsi) || !is.null(m$velocity_hsi), "No valid HSI curve parsed."))
    base_par(1.25); par(mfrow = c(1, 2))
    if (!is.null(m$depth_hsi)) {
      plot(m$depth_hsi$depth, m$depth_hsi$hsi, type = "b", pch = 19, lwd = 2.5, col = COL$blue, ylim = c(0, 1),
           xlab = "Depth (m)", ylab = "HSI (0-1)", main = "Depth suitability curve")
    } else plot.new()
    if (!is.null(m$velocity_hsi)) {
      plot(m$velocity_hsi$velocity, m$velocity_hsi$hsi, type = "b", pch = 19, lwd = 2.5, col = COL$orange, ylim = c(0, 1),
           xlab = "Velocity (m/s)", ylab = "HSI (0-1)", main = "Velocity suitability curve")
    } else plot.new()
  })

  output$dist_plot <- renderPlot({
    m <- model(); req(m$ok); r <- m$res
    base_par(1.25); par(mfrow = c(2, 2))
    bars <- function(bins, centre, lab, main, col, mean_val) {
      w <- centre[2] - centre[1]
      plot(centre, bins$prob, type = "n", xlab = lab, ylab = "Probability per bin", main = main,
           ylim = c(0, max(bins$prob) * 1.12))
      rect(centre - w / 2, 0, centre + w / 2, bins$prob, col = col, border = "#fcfcfb", lwd = 0.5)
      abline(v = mean_val, lty = 2, col = COL$grey, lwd = 1.5)
      text(mean_val, max(bins$prob) * 1.12, "mean", adj = c(-0.15, 1), col = COL$grey, cex = 1)
    }
    steps <- function(bins, centre, curve, lab, main, col) {
      plot(centre, bins$hsi, type = "s", lwd = 2.5, col = col, ylim = c(0, 1),
           xlab = lab, ylab = "Suitability (0-1)", main = main)
      lines(curve[[1]], curve$hsi, col = COL$grey, lty = 3, lwd = 1.5)
      legend("topright", bty = "n", cex = 1, text.col = COL$grey,
             legend = c("at bin centres", "pasted curve"), col = c(col, COL$grey), lty = c(1, 3), lwd = c(2.5, 1.5))
    }
    bars(r$depth_bins, r$depth_bins$centre_m, "Depth (m)", "Predicted depth frequency", COL$blue, r$hydraulics$mean_depth_m)
    steps(r$depth_bins, r$depth_bins$centre_m, m$depth_hsi, "Depth (m)", "Depth HSI applied to each bin", COL$blue)
    bars(r$velocity_bins, r$velocity_bins$centre_ms, "Velocity (m/s)", "Predicted velocity frequency", COL$orange, r$hydraulics$mean_velocity_ms)
    steps(r$velocity_bins, r$velocity_bins$centre_ms, m$velocity_hsi, "Velocity (m/s)", "Velocity HSI applied to each bin", COL$orange)
  })

  output$joint_plot <- renderPlot({
    m <- model(); req(m$ok); r <- m$res
    base_par(1.25)
    z <- r$joint
    image(r$depth_bins$centre_m, r$velocity_bins$centre_ms, z,
          col = colorRampPalette(SEQ_BLUE)(64), xlab = "Depth (m)", ylab = "Velocity (m/s)",
          main = sprintf("WUW contribution per depth x velocity bin (darker = more); total = %.3f m", sum(z)))
    box(col = COL$grey)
  })

  # ---- assumptions checklist --------------------------------------------------
  assumptions <- model_assumptions()
  output$checklist <- renderUI({
    div(class = "checklist",
        checkboxGroupInput("checked", NULL, choices = setNames(seq_along(assumptions), assumptions),
                           width = "100%"))
  })
  output$checked_count <- renderText({
    n <- length(input$checked)
    if (n == length(assumptions)) sprintf("All %d assumptions reviewed.", n)
    else sprintf("%d of %d assumptions reviewed.", n, length(assumptions))
  })

  # ---- downloads --------------------------------------------------------------
  output$dl_curve <- downloadHandler(
    filename = function() "wuw_flow_response.csv",
    content = function(file) {
      m <- model(); req(!is.null(m$curve))
      write.csv(m$curve, file, row.names = FALSE)
    })
  output$dl_bins <- downloadHandler(
    filename = function() "wuw_bins.csv",
    content = function(file) {
      m <- model(); req(m$ok)
      d <- m$res$depth_bins; d$variable <- "depth"
      names(d) <- sub("_m$", "", names(d))
      v <- m$res$velocity_bins; v$variable <- "velocity"
      names(v) <- sub("_ms$", "", names(v))
      write.csv(rbind(d, v), file, row.names = FALSE)
    })
}

shinyApp(ui, server)
