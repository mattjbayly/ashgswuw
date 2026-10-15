# Harris Creek inputs (McParland et al. 2016) and toy HSI curves.
harris <- list(bankfull_width_m = 14.36, bankfull_depth_m = 0.705,
               gradient_m_per_m = 0.011, d84_mm = 135)

depth_toy <- data.frame(depth = c(0, 0.06, 0.3, 0.61, 0.91, 1.5),
                        hsi = c(0, 0.15, 1, 0.7, 0.4, 0.2))
velocity_toy <- data.frame(velocity = c(0, 0.03, 0.06, 0.12, 0.18, 0.5, 1.0),
                           hsi = c(0, 1, 1, 0.7, 0.4, 0.2, 0))

const_hsi <- function(value) data.frame(x = c(0, 10), hsi = c(value, value))

# Successful runs emit no messages, so none are suppressed here; tests that
# expect failures can then assert on the message.
harris_estimate <- function(discharge_m3s, ...) {
  args <- utils::modifyList(
    c(harris, list(discharge_m3s = discharge_m3s,
                   depth_hsi = depth_toy, velocity_hsi = velocity_toy)),
    list(...))
  do.call(wuw_estimate, args)
}
