llr_test <- function(code) {
  interp <- llr_env$new()
  interp$eval(code)
}
