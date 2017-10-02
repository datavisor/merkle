## This implements just the hashing bit of the merkle tree; this bit
## will collect up our hashing approach.  This could fold into being
## an R6 class but I'm not really sure at this point how much that is
## desirable (particularly to avoid the overhead around calling
## `self$` repeatedly)
##
## TODO: need to get object hashing into here with all the usual drama
merkle_hasher <- function(hash) {
  raw_0 <- as.raw(0L)
  raw_1 <- as.raw(1L)
  assert_scalar_character(hash)
  hash_name <- hash
  hash <- hash_function(hash)

  ## (void) => raw
  hash_empty <- function() {
    hash(raw())
  }
  ## raw => raw
  hash_leaf <- function(data) {
    hash(c(raw_0, data))
  }
  ## raw => raw
  hash_node <- function(left, right) {
    hash(c(raw_1, left, right))
  }
  ## TODO: need support for updating the tree after addition of 'n'
  ## leaves.
  ## list[raw] => list[raw]
  hash_tree <- function(x) {
    n <- length(x)
    if (n == 0L) {
      return(list(list(hash_empty())))
    }
    ret <- vector("list", binary_tree_height(n))
    ret[[1L]] <- x
    for (i in seq_along(ret)[-1L]) {
      x <- ret[[i]] <- hash_level(x)
    }
    ret
  }
  ## list[raw] => list[raw]
  hash_level <- function(x) {
    n <- length(x)
    is_odd <- n %% 2L == 1L
    if (is_odd) {
      n <- n - 1L
    }
    i <- seq.int(1L, n, by = 2L)
    res <- Map(hash_node, x[i], x[i + 1L])
    if (is_odd) {
      res <- c(res, x[n + 1L])
    }
    res
  }

  ## TODO: there is probably more type assertions here than are really
  ## required given that this is used at a fairly low level.
  self <- list(
    name = function() {
      hash_name
    },
    hash_empty = hash_empty,
    hash_leaf = function(data) {
      assert_raw(data)
      hash_leaf(data)
    },
    hash_node = function(left, right) {
      assert_raw(left)
      assert_raw(right)
      hash_node(left, right)
    },
    hash_tree = function(leaves) {
      assert_is(leaves, "list")
      stopifnot(all(vlapply(leaves, is.raw)))
      hash_tree(leaves)
    },
    is_equal = same_bytes_hash)
  self
}

format_hash <- function(h, w) {
  s <- as.character(h, ":")
  if (nchar(s[[1L]]) > w) {
    s <- paste0(substr(s, 1L, w - 3L), "...")
  }
  s
}

format_merkle_proof <- function(x, n = getOption("width") - 13) {
  type <- x$type
  x$type <- NULL

  if (is.null(names(x$chain))) {
    chain <- paste("  -", vcapply(x$chain, format_hash, n))
  } else {
    chain <- sprintf("  - %s (%s)",
                     vcapply(x$chain, format_hash, n), names(x$chain))
  }
  x$chain <- NULL

  i <- vlapply(x, inherits, "hash")
  x[i] <- lapply(x[i], format_hash, n)
  c(sprintf("merkle %s proof:", type),
    sprintf("  %s: %s", names(x), vcapply(x, as.character)),
    "  chain:",
    chain)
}

##' @export
print.merkle_proof <- function(x, ...) {
  cat(paste0(format_merkle_proof(x, ...), "\n", collapse = ""))
  invisible(x)
}
