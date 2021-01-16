#' @export
knitr_language_engine <- function(llr_interpreter = llr_env$new()) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("kntir is required to use this function")
  }
  function(options) {
    code <- paste(options$code, collapse = "\n")
    if (!options$eval) {
      knitr::engine_output(options, options$code, NULL)
    } else {
      result <- llr_interpreter$eval(code)
      if (options$results == "hide") {
        knitr::engine_output(options, options$code, NULL)
      } else {
        out <- utils::capture.output(print(result))
        knitr::engine_output(options, options$code, out)
      }
    }
  }
}
