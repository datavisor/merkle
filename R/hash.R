hash_function <- function(hash_name) {
  assert_scalar_character(hash_name)
  hash_functions[[hash_name]] %||%
    stop(sprintf("Unknown hash function '%s'", hash_name), call. = FALSE)
}

hash_functions_get <- function() {
  algos <- c("sha1", "sha224", "sha256", "sha384", "sha512",
             "md4", "md5", "blake2b", "blake2s", "ripemd160")
  res <- lapply(algos, getExportedValue, ns = "openssl")
  names(res) <- algos
  res
}

as_hash <- function(x, hash_name = NULL) {
  if (is.character(x)) {
    x <- hexstr_to_raw(x)
  } else {
    assert_raw(x)
  }
  if (!inherits(x, "hash") && !is.null(hash_name)) {
    class(x) <- c("hash", hash_name)
  }
  x
}

## This is needed because we can't rely on things coming in as direct
## objects from the openssl hash functions - they may have lost
## attributes that indicate which hash function generated them.  So
## instead we reduce things down to their bytes (which is all we
## really care about) and use that.
same_bytes_hash <- function(a, b) {
  identical(as.raw(a), as.raw(b))
}

## This could do with work, but will do for now.  It's not crazy fast
hexstr_to_raw <- function(x) {
  x <- gsub(":", "", x, fixed = TRUE)
  as.raw(strtoi(strsplit(x, "(?<=..)", perl = TRUE)[[1L]], 16L))
}
