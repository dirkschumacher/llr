#' @export
#' @import utils
ral_map <- function(keys = NULL, values = NULL,
                    map = fastmap::fastmap(),
                    keymap = fastmap::fastmap()) {
  if (!is.null(keys) && !is.null(values) && length(keys) > 0 &&
      length(values) == length(keys)) {
    key_hashes <- vapply(keys, ral_map_hash, character(1))
    names(values) <- key_hashes
    names(keys) <- key_hashes
    map$mset(.list = values)
    keymap$mset(.list = keys)
  }
  copy_map <- function(x) {
    m <- fastmap::fastmap()
    if (x$size() > 0) {
      m$mset(.list = x$as_list())
    }
    m
  }
  structure(
    list(
      set = function(key, x) {
        new_map <- copy_map(map)
        new_keymap <- copy_map(keymap)
        hash <- ral_map_hash(key)
        new_map$set(hash, x)
        keymap$set(hash, key)
        ral_map(map = new_map, keymap = keymap)
      },
      get = function(key) {
        map$get(ral_map_hash(key))
      },
      contains = function(key) {
        map$has(ral_map_hash(key))
      },
      keys = function() {
        ral_list(.data = setNames(keymap$as_list(), NULL))
      },
      values = function() {
        ral_list(.data = setNames(map$as_list(), NULL))
      },
      remove = function(key) {
        new_map <- copy_map(map)
        new_keymap <- copy_map(keymap)
        hash <- ral_map_hash(key)
        new_map$remove(hash)
        new_keymap$remove(hash)
        ral_map(map = new_map, keymap = new_keymap)
      },
      length = function() {
        map$size()
      }
    ),
    class = c("ral_map")
  )
}

#' @export
#' @import digest
#' @include hasheq.R
ral_map_hash <- function(key) {
  hasheq(key)
}

#' @export
format.ral_map <- function(x, ...) {
  paste0(
    "{",
    paste0(vapply(x$keys(), function(key) {
      paste0(format(key), " ", format(x$get(key)))
    }, character(1)), collapse = " "),
    "}"
  )
}

#' @export
#' @include list.R
print.ral_map <- default_print

#' @export
as.character.ral_map <- function(x, ...) {
  format(x)
}
