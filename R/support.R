## for n = 0 we have [1] => 1
## For n = 1 we have [1, 1] => 2
## for n = 2 we have [2, 1] => 2
## for n = 3 we have [3, 2, 1] => 3
## for n = 4 we have [4, 2, 1] => 3
## for n = 5 we have [5, 4, 2, 1] => 4
## ...
## for n = 8 we have [8, 4, 2, 1] => 4
binary_tree_height <- function(n) {
  if (n >= 2) {
    ceiling(log2(n)) + 1L
  } else {
    n + 1L
  }
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

## The path to the i'th element in a tree with 'nlevels' levels
## (including the root) for a max of 2^nlevels indices.  Note that the
## first element of the path will always be 'i' and the last always
## '1L'
merkle_path <- function(i, nlevels) {
  floor((i - 1L) / 2^(seq_len(nlevels) - 1L)) + 1L
}

tree_pick <- function(level, index, tree) {
  Map(function(i, j) tree[[i]][[j]], level, index)
}
