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
    if (options$eval == FALSE) {
      knitr::engine_output(options, options$code, NULL)
    } else {
      result <- llr::llr(code, envir)
      knitr::engine_output(options, options$code, result)
    }
  }
}
