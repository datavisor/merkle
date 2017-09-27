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

  public = list(
    initialize = function(hash) {
      assert_scalar_character(hash)
      private$hash_name <- hash
      private$hash <- hash_function(hash)
    },

    add_leaf = function(hash) {
      assert_is(hash, "hash")
      self$append(hash)
    },

    append = function(x) {
      h <- as_hash_list(x)
      private$leaves <- c(private$leaves, h)
      private$compute()
      invisible(h)
    },

    leaf = function(index) {
      private$leaves[[index]]
    },

    length = function() {
      length(private$leaves)
    },

    height = function() {
      length(private$tree)
    },

    root = function() {
      private$tree[[length(private$tree)]][[1L]]
    },

    proof_audit = function(index) {
      merkle_proof_audit(index, private$tree)
    },

    validate_audit = function(proof, leaf, root = NULL) {
      leaf <- as_hash(leaf, private$hash_name)
      root <- as_hash(root %||% self$root(), private$hash_name)
      merkle_validate_audit(proof, leaf, root, private$hash)
    },

    proof_consistency = function(n) {
      merkle_proof_consistency(n, private$tree)
    },

    ## This is going to end up getting changed around a bit as we
    ## support serialisation.
    digest_string = function(x) {
      hash_raw(charToRaw(x), private$hash)
    }
  ),

  private = list(
    hash = NULL,
    hash_name = NULL,
    leaves = list(),
    tree = NULL,

    compute = function() {
      private$tree <- compute_tree(private$leaves, private$hash)
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

merkle_proof_audit <- function(index, tree) {
  len <- lengths(tree)
  nlevels <- length(tree)
  idx <- merkel_index(index, nlevels - 1L)
  is_right <- idx %% 2 == 0L
  sibling_index <- ifelse(is_right, idx - 1L, idx + 1L)
  sibling_pos <- ifelse(is_right, "left", "right")
  include <- !(!is_right & idx == lengths(tree)[seq_along(idx)])
  proof <- tree_pick(seq_along(sibling_index)[include],
                     sibling_index[include],
                     tree)
  names(proof) <- sibling_pos[include]

  ret <- list(chain = proof,
              leaf = tree[[1L]][[index]],
              root = tree[[length(tree)]][[1L]])
  class(ret) <- "merkle_proof"
  ret
}

merkle_validate_audit <- function(proof, leaf_hash, root_hash, hash) {
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

## Number of children at a point in the tree where we have:
##
## * n_leaves - number of leaves in the entire tree
##
## * level - the level of the tree (with 1 being the leaves and the
##   level 'k' having up to 2^(k - 1) children)
##
## * index - the index within this level
merkle_n_children <- function(n_leaves, level, index) {
  if (length(index) > 1L && length(level) == 1L) {
    level <- rep_len(level, length(index))
  }
  n <- 2^(level - 1)
  m <- n * index
  i <- m > n_leaves
  if (any(i)) {
    n[i] <- n_leaves - (m[i] - n[i])
    n[n < 0] <- 0
  }
  n
}

tree_pick <- function(level, index, tree) {
  Map(function(i, j) tree[[i]][[j]], level, index)
}

## So 'm' is the number of leaves in the *subtree* and n_leaves is the
## number of leaves in our real tree
consistency_proof_nodes <- function(m, n_leaves) {
  if (m > n_leaves) {
    stop("too many")
  }
  level <- floor(log2(m)) + 1L
  index <- 1L
  k <- merkle_n_children(n_leaves, level, index)

  proof_level <- level
  proof_index <- 1L

  rem <- m - k

  if (rem > 0) {
    index <- index + 1L
    repeat {
      k_sibling <- merkle_n_children(n_leaves, level, index)

      if (rem == k_sibling) {
        proof_level <- c(proof_level, level)
        proof_index <- c(proof_index, index)
        break
      } else if (rem < k_sibling) {
        level <- level - 1L
        index <- (index - 1L) * 2 + 1L # left daughter
        next
      } else { # rem > k_sibling
        proof_level <- c(proof_level, level)
        proof_index <- c(proof_index, index)
        k <- k + k_sibling
        rem <- m - k
        if (index %% 2 == 0) {
          stop("I don't think this is possible")
        }
        index <- index + 1L
      }
    }
  }
  list(level = proof_level, index = proof_index)
}
