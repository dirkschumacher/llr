llr_test <- function(code) {
  if (is.null(llr_interp <- get0("llr_interp", envir = parent.frame()))) {
    llr_interp <- llr_interp <<- llr_env$new()
  }
  llr_interp$eval(code)
}

llr_fresh_test <- function(code) {
  llr_test(code)
}
