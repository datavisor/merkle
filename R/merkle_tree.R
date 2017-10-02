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

R6_merkle_tree <- R6::R6Class(
  "merkle_tree",

  public = list(
    initialize = function(hash) {
      private$.hasher <- merkle_hasher(hash)
    },

    ## Number of leaves in the tree
    length = function() {
      length(private$.leaves)
    },

    height = function() {
      binary_tree_height(self$length())
    },

    ## The hashes
    leaves = function() {
      private$.leaves
    },
    leaf = function(index) {
      private$.leaves[[index]]
    },
    index = function(hash, nomatch = NA_integer_) {
      ## TODO: this should *probably* handle vectors of hashes too
      assert_raw(hash)
      match(list(hash), private$.leaves, nomatch)
    },

    tree = function() {
      if (is.null(private$.tree)) {
        self$compute()
      }
      private$.tree
    },

    root = function() {
      last(self$tree())[[1L]]
    },

    append = function(data) {
      leaf <- private$.hasher$hash_leaf(data)
      private$.leaves <- c(private$.leaves, list(leaf))
      private$.tree <- NULL
      invisible(self)
    },

    extend = function(data) {
      assert_is(data, "list")
      leaves <- lapply(data, private$.hasher$hash_leaf)
      private$.leaves <- c(private$.leaves, leaves)
      private$.tree <- NULL
      invisible(self)
    },

    extended = function(data) {
      new <- self$clone()
      new$extend(data)
      new
    },

    compute = function() {
      private$.tree <- private$.hasher$hash_tree(private$.leaves)
      invisible(self)
    },

    hasher = function() {
      private$.hasher
    },

    ## Names below here will change.
    proof_audit = function(index) {
      merkle_audit_proof(index, self$tree(), private$.hasher$name())
    },

    proof_consistency = function(n_leaves) {
      merkle_consistency_proof(n_leaves, self$tree(), private$.hasher$name())
    }
  ),

  private = list(
    .hasher = NULL,
    .leaves = list(),
    .tree = NULL
  ))
