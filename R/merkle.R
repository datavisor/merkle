##' Construct a Merkle tree.  At present this returns an R6 object but
##' this is an implementation detail subject to change.
##'
##' @title Construct a Merkle tree
##' @param hash The hash algorithm to use.  Default is sha-256.  Hash
##'   functions come from the \code{openssl} package.  Valid values
##'   are \code{sha1}, \code{sha224}, \code{sha256}, \code{sha384},
##'   \code{sha512}, \code{blake2b}, \code{blake2s} and
##'   \code{ripemd160}.  You \emph{could} use \code{md4}, \code{md5},
##'   but as these are really not secure it is not recommended.
##' @export
merkle_tree <- function(hash = "sha256") {
  R6_merkle_tree$new(hash)
}

## TODO: do some cache invalidation on the tree.  Currently there is
## no ready/not ready switch so there is no verification that the tree
## reflects the leaves.
##
## TODO: there is no checking that the hashes are of the expected type
## (on addition).  This could be mitigated by working out what to do
## with the empty tree.
R6_merkle_tree <- R6::R6Class(
  "merkle_tree",

  private = list(
    hash = NULL,
    hash_name = NULL,
    leaves = list(),
    tree = NULL
  ),

  public = list(
    initialize = function(hash) {
      assert_scalar_character(hash)
      private$hash_name <- hash
      private$hash <- hash_function(hash)
    },

    add_leaf = function(hash) {
      private$leaves <- c(private$leaves, list(hash))
      invisible(length(private$leaves))
    },

    leaf = function(index) {
      private$leaves[[index]]
    },

    length = function() {
      length(private$leaves)
    },

    height = function() {
      ceiling(log2(length(private$leaves)))
    },

    root = function() {
      private$tree[[length(private$tree)]][[1L]]
    },

    compute = function() {
      private$tree <- compute_tree(private$leaves, private$hash)
    },

    proof = function(index) {
      merkle_proof(index, private$tree)
    },

    validate = function(proof, leaf, root = NULL) {
      leaf <- as_hash(leaf, private$hash_name)
      root <- as_hash(root %||% self$root(), private$hash_name)
      merkle_validate(proof, leaf, root, private$hash)
    },

    ## This is going to end up getting changed around a bit as we
    ## support serialisation.
    digest_string = function(x) {
      hash_raw(charToRaw(x), private$hash)
    }
  ))

## This can be done more efficiently if we have already done the
## calculation already; if we are appending only then all we need to
## do is fill things in at the end.  This will practically only be an
## issue if we have a lot of records though, but seems worth getting
## sorted out as soon as possible.  We'll still need some facility to
## verify the tree though.
compute_tree <- function(leaves, hash) {
  n <- length(leaves)
  if (n == 0L) {
    ## could assume that the concatenated hash is "" and hash that
    stop("tree has no leaves")
  }
  ret <- vector("list", ceiling(log2(n)) + 1L)
  ret[[1L]] <- leaves
  for (i in seq_along(ret)[-1L]) {
    leaves <- ret[[i]] <- compute_level(leaves, hash)
  }
  ret
}

compute_level <- function(x, hash) {
  n <- length(x)
  is_odd <- n %% 2L == 1L
  if (is_odd) {
    n <- n - 1L
  }
  i <- seq.int(1L, n, by = 2L)
  res <- Map(function(a, b) hash_raw(c(a, b), hash), x[i], x[i + 1L])
  if (is_odd) {
    res <- c(res, x[n + 1L])
  }
  res
}

merkel_index <- function(i, nlevels) {
  floor((i - 1L) / 2^(seq_len(nlevels) - 1L)) + 1L
}

merkle_proof <- function(index, tree) {
  len <- lengths(tree)
  nlevels <- length(tree)
  idx <- merkel_index(index, nlevels - 1L)
  is_right <- idx %% 2 == 0L
  sibling_index <- ifelse(is_right, idx - 1L, idx + 1L)
  sibling_pos <- ifelse(is_right, "left", "right")
  include <- !(!is_right & idx == lengths(tree)[seq_along(idx)])
  proof <- Map(function(i, j) tree[[i]][[j]],
               seq_along(sibling_index)[include],
               sibling_index[include])
  names(proof) <- sibling_pos[include]

  ret <- list(chain = proof,
              leaf = tree[[1L]][[index]],
              root = tree[[length(tree)]][[1L]])
  class(ret) <- "merkle_proof"
  ret
}

merkle_validate <- function(proof, leaf_hash, root_hash, hash) {
  chain <- proof$chain

  leaf_hash <- as_hash(leaf_hash)
  root_hash <- as_hash(root_hash)

  if (length(chain) == 0L) {
    return(leaf_hash == root_hash)
  } else {
    left <- names(chain) == "left"
    chain_hash <- leaf_hash
    for (i in seq_along(chain)) {
      pair <-
        if (left[[i]]) c(chain[[i]], chain_hash) else c(chain_hash, chain[[i]])
      chain_hash <- hash_raw(pair, hash)
    }
    same_bytes(chain_hash, root_hash)
  }
}

##' @export
print.merkle_proof <- function(x, ..., n = getOption("width") - 12) {
  f <- function(h) {
    s <- as.character(h, ":")
    if (nchar(s[[1L]]) > n) {
      s <- paste0(substr(s, 1L, n - 3L), "...")
    }
    s
  }
  cat("merkle proof:\n")
  cat(sprintf("  leaf: %s\n", f(x$leaf)))
  cat(sprintf("  root: %s\n", f(x$root)))
  cat("  chain:\n")
  cat(sprintf("  - %s (%s)\n", vcapply(x$chain, f), names(x$chain)),
      sep = "")
  invisible(x)
}
