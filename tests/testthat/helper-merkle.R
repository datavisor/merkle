## This is going to be somewhat useful for testing; it will create
## trees like those used in this article:
##
## https://www.codeproject.com/Articles/1176140/Understanding-Merkle-Trees-Why-use-them-who-uses-t
fake_tree <- function(n, base_0 = TRUE) {
  pool <- c(seq_len(9), letters, LETTERS)
  if (base_0) {
    pool <- c("0", pool)
  }

  ## TODO: fold this back into the main thing by properly keeping
  ## 'hash'/'hash_raw' safe as a tree operation.
  compute_tree_str <- function(leaves) {
    n <- length(leaves)
    if (n == 0L) {
      ## could assume that the concatenated hash is "" and hash that
      stop("tree has no leaves")
    }
    ret <- vector("list", ceiling(log2(n)) + 1L)
    ret[[1L]] <- leaves
    for (i in seq_along(ret)[-1L]) {
      leaves <- ret[[i]] <- compute_level_str(leaves)
    }
    ret
  }

  compute_level_str <- function(x) {
    n <- length(x)
    is_odd <- n %% 2L == 1L
    if (is_odd) {
      n <- n - 1L
    }
    i <- seq.int(1L, n, by = 2L)
    res <- mapply(function(a, b) paste0(a, b), x[i], x[i + 1L],
                  USE.NAMES = FALSE)
    if (is_odd) {
      res <- c(res, x[n + 1L])
    }
    res
  }

  compute_tree_str(pool[seq_len(n)])
}

r6_private <- function(x) {
  environment(x$initialize)$private
}
