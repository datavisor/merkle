## A hash list is like an ordinary list *but* it has no attributes.
## This includes no names!  That seems a bit suboptimal frankly.  But
## this is the building block I guess
##
## This is because that implies another level of container.
hash_list <- function(x, algorithm) {
  if (!is.list(list)) {
    stop("Expected a list for 'x'")
  }
  hash <- hash_function(algorithm)
  hashes <- vcapply(x, hash_compute, hash)
  attr(x, "hashes") <- hashes
  attr(x, "hash") <- hash_compute(paste(hashes, collapse = ""), hash)
  class(x) <- "hash_list"
  x
}
