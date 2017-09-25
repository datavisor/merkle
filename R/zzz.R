hash_functions <- NULL

.onLoad <- function(...) {
  algos <- c("sha1", "sha224", "sha256", "sha384", "sha512",
             "md4", "md5", "blake2b", "blake2s", "ripemd160")
  res <- lapply(algos, getExportedValue, ns = "openssl")
  names(res) <- algos
  hash_functions <<- res
}
