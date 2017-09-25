## This is the same problem as with storr; if we want to hash an
## *object* then we need to (perhaps) skip the bytes that declare R
## version; basically digest::digest

hash_object <- function(x, algorithm, skip = TRUE) {
  ## This would be more efficient if we
  bytes <- serialize(x, NULL)
  if (skip) {
    bytes <- bytes[-seq_len(14L)]
  }
  hash_compute(bytes, algorithm)
}

hash_function <- function(algorithm) {
  if (is.function(algorithm)) {
    algorithm
  } else {
    hash_functions[[algorithm]] %||% stop("invalid hash")
  }
}

hash_compute <- function(x, algorithm) {
  as.character(hash_function(algorithm)(x))
}

hash_raw <- function(x, algorithm) {
  assert_raw(x)
  hash_function(algorithm)(x)
}
