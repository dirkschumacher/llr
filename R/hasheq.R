#' @export
hasheq <- function(x) {
  UseMethod("hasheq")
}

#' @import digest
#' @export
hasheq.default <- function(x) {
  attributes(x) <- NULL
  key <- serialize(x, NULL, version = 3)
  digest(key, algo = "xxhash64", raw = TRUE, ascii = FALSE)
}
