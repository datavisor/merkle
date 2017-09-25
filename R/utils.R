vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

## This is needed because we can't rely on things coming in as direct
## objects from the openssl hash functions - they may have lost
## attributes that indicate which hash function generated them.  So
## instead we reduce things down to their bytes (which is all we
## really care about) and use that.
same_bytes <- function(a, b) {
  identical(as.raw(a), as.raw(b))
}

## This could do with work, but will do for now.  It's not crazy fast
hexstr_to_raw <- function(x) {
  as.raw(strtoi(strsplit(x, "(?<=..)", perl = TRUE)[[1L]], 16L))
}

as_hash <- function(x, algorithm = "generic") {
  if (is.character(x)) {
    x <- hexstr_to_raw(x)
  } else {
    assert_raw(x)
  }
  if (!inherits(x, "hash")) {
    class(x) <- c("hash", algorithm)
  }
  x
}

as_hash_list <- function(x) {
  if (is_hash(x)) {
    x <- list(x)
  } else {
    ok <- vlapply(x, is_hash)
    if (!all(ok)) {
      stop("all elements of 'x' must be hash")
    }
  }
  x
}

is_hash <- function(x) {
  inherits(x, "hash")
}
