vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}
viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

last <- function(x) {
  x[[length(x)]]
}
