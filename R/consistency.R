## There are two sorts of consistency proofs we might want to do; one
## is with the tree in its current state; if the tree is up to date
## then we should just use consistency_proof_nodes to identify and
## then pull out the appropriate nodes.  But if we want to check that
## the tree was consistent when it was smaller we need to compute new
## hashes and the approach taken by the google certificate
## transparency might be better.  But I think we can come back for
## that later.

merkle_consistency_proof <- function(n, tree, hash_name) {
  ## TODO: this is not correct for the zero length tree because then
  ## we do have zero leaves; I might update this a little as I flesh
  ## out that corner case...
  n_leaves_ours <- length(tree[[1L]])
  if (n > n_leaves_ours) {
    stop(sprintf("Requested proof for sizes beyond current tree: %d > %d"),
         n, self$length())
  }
  nodes <- merkle_consistency_proof_nodes(n, n_leaves_ours)
  chain <- tree_pick(nodes[[1L]], nodes[[2L]], tree)
  ret <- list(chain = chain,
              root = last(tree)[[1L]],
              hash_name = hash_name)
  class(ret) <- "merkle_consistency_proof"
  ret
}

## So 'm' is the number of leaves in the *subtree* and n_leaves_ours
## is the number of leaves in our real tree
merkle_consistency_proof_nodes <- function(m, n_leaves_ours) {
  level <- floor(log2(m)) + 1L
  index <- 1L

  proof_level <- level
  proof_index <- 1L

  rem <- m - merkle_n_children(n_leaves_ours, level, index)

  if (rem > 0) {
    index <- index + 1L
    repeat {
      k_sibling <- merkle_n_children(n_leaves_ours, level, index)

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
        rem <- rem - k_sibling
        if (index %% 2 == 0) {
          stop("I don't think this is possible")
        }
        index <- index + 1L
      }
    }
  }
  list(level = proof_level, index = proof_index)
}

merkle_consistency_compute_root <- function(chain, hasher) {
  right <- last(chain)
  for (x in rev(chain)[-1L]) {
    right <- hasher$hash_node(x, right)
  }
  right
}

merkle_consistency_test <- function(proof, root_hash, hasher, error = FALSE) {
  root_hash_computed <- merkle_consistency_compute_root(proof$chain, hasher)
  same <- hasher$is_equal(root_hash_computed, root_hash)
  if (error && !same) {
    stop("Hashes do not match")
  }
  same
}

##' @export
print.merkle_consistency_proof <- function(x, ...) {
  print_proof(x, "consistency", ...)
}
