hash_functions <- NULL

.onLoad <- function(...) {
  hash_functions <<- hash_functions_get()
}
