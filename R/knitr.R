#' Create a new engine for knitr
#' @param envir the environment in which llr code is executed
#'
#' @examples
#' \dontrun{
#' # in your first knitr chunk run
#' knitr::knit_engines$set(llr = llr::knitr_engine(sys.frame()))
#' }
#' @export
knitr_engine <- function(envir) {
  function(options) {
    code <- paste0(options$code, collapse = "\n")
    options$engine <- "clojure" # for syntax highlighting
    if (options$eval == FALSE) {
      knitr::engine_output(options, options$code, NULL)
    } else {
      result <- llr::llr(code, envir)
      if (options$results == "hide") {
        knitr::engine_output(options, options$code, NULL)
      } else {
        out <- utils::capture.output(print(result))
        knitr::engine_output(options, options$code, out)
      }
    }
  }
}
