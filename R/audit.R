merkle_audit_proof <- function(index, tree, hash_name) {
  len <- lengths(tree)
  nlevels <- length(tree)
  idx <- merkle_path(index, nlevels - 1L)
  is_right <- idx %% 2 == 0L
  ## TODO: I am not convinced about the sensibleness of ifelse here!
  sibling_index <- ifelse(is_right, idx - 1L, idx + 1L)
  sibling_pos <- ifelse(is_right, "left", "right")
  include <- !(!is_right & idx == lengths(tree)[seq_along(idx)])
  proof <- tree_pick(seq_along(sibling_index)[include],
                     sibling_index[include],
                     tree)
  names(proof) <- sibling_pos[include]

  ret <- list(chain = proof,
              hash_name = hash_name,
              leaf = tree[[1L]][[index]],
              root = last(tree)[[1L]],
              index = index)
  class(ret) <- "merkle_audit_proof"
  ret
}

merkle_audit_compute_root <- function(leaf_hash, chain, hasher) {
  if (length(chain) == 0L) {
    browser()
  } else {
    left <- names(chain) == "left"
    chain_hash <- leaf_hash
    for (i in seq_along(chain)) {
      if (left[[i]]) {
        chain_hash <- hasher$hash_node(chain[[i]], chain_hash)
      } else {
        chain_hash <- hasher$hash_node(chain_hash, chain[[i]])
      }
    }
    chain_hash
  }
}

merkle_audit_test <- function(leaf_hash, proof, root_hash, hasher,
                              error = FALSE) {
  root_hash_computed <-
    merkle_audit_compute_root(leaf_hash, proof$chain, hasher)
  same <- hasher$is_equal(root_hash_computed, root_hash)
  if (error && !same) {
    stop("Hashes do not match")
  }
  same
}

merkle_audit_proof_check <- function(proof) {
  hasher <- merkle_hasher(proof$hash_name)
  merkle_audit_test(proof$leaf, proof, proof$root, hasher)
}

##' @export
print.merkle_audit_proof <- function(x, ...) {
  print_proof(x, "audit", ...)
}
