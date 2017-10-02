context("merkle calculations")

test_that("binary tree height", {
  expect_equal(binary_tree_height(0), 1)
  expect_equal(binary_tree_height(1), 2) # [1, 1]
  expect_equal(binary_tree_height(2), 2) # [2, 1]
  expect_equal(binary_tree_height(3), 3) # [3, 2, 1]
  expect_equal(binary_tree_height(4), 3) # [4, 2, 1]
  expect_equal(binary_tree_height(5), 4) # [5, 4, 2, 1]
  expect_equal(binary_tree_height(8), 4) # [8, 4, 2, 1]
  expect_equal(binary_tree_height(9), 5) # [9, 8, 4, 2, 1]
})

test_that("merkle children, balanced tree", {
  expect_equal(merkle_n_children(8L, 1, 1:8), rep(1, 8))
  expect_equal(merkle_n_children(8L, 1, 9), 0)

  expect_equal(merkle_n_children(8L, 2, 1:4), rep(2, 4))
  expect_equal(merkle_n_children(8L, 2, 9), 0)

  expect_equal(merkle_n_children(8L, 3, 1:2), rep(4, 2))
  expect_equal(merkle_n_children(8L, 3, 9), 0)

  expect_equal(merkle_n_children(8L, 4, 1), 8)
  expect_equal(merkle_n_children(8L, 4, 9), 0)
})

test_that("merkle children, unbalanced tree", {
  expect_equal(merkle_n_children(5L, 1, 1:8), rep(1:0, c(5, 3)))
  expect_equal(merkle_n_children(5L, 2, 1:4), c(2, 2, 1, 0))
  expect_equal(merkle_n_children(5L, 3, 1:2), c(4, 1))
  expect_equal(merkle_n_children(5L, 4, 1), 5)

  expect_equal(merkle_n_children(6L, 1, 1:8), rep(1:0, c(6, 2)))
  expect_equal(merkle_n_children(6L, 2, 1:4), rep(c(2, 0), c(3, 1)))
  expect_equal(merkle_n_children(6L, 3, 1:2), c(4, 2))
  expect_equal(merkle_n_children(6L, 4, 1), 6)

  expect_equal(merkle_n_children(7L, 1, 1:8), rep(1:0, c(7, 1)))
  expect_equal(merkle_n_children(7L, 2, 1:4), c(2, 2, 2, 1))
  expect_equal(merkle_n_children(7L, 3, 1:2), c(4, 3))
  expect_equal(merkle_n_children(7L, 4, 1), 7)

  ## A bunch at once:
  level <- 1:4
  pos <- 2^(4 - level)
  level <- rep(level, pos)
  index <- sequence(pos)

  expect_equal(merkle_n_children(5L, level, index),
               c(rep(1:0, c(5, 3)), c(2, 2, 1, 0), c(4, 1), 5))
  expect_equal(merkle_n_children(6L, level, index),
               c(rep(1:0, c(6, 2)), c(2, 2, 2, 0), c(4, 2), 6))
  expect_equal(merkle_n_children(7L, level, index),
               c(rep(1:0, c(7, 1)), c(2, 2, 2, 1), c(4, 3), 7))
  expect_equal(merkle_n_children(8L, level, index),
               rep(c(1, 2, 4, 8), pos))
})

test_that("consistency proof nodes", {
  f <- function(m, n) {
    x <- merkle_consistency_proof_nodes(m, n)
    vapply(tree_pick(x$level, x$index, fake_tree(n)), identity, "")
  }

  expect_equal(f(3, 7), c("01", "2"))
  expect_equal(f(4, 7), c("0123"))
  expect_equal(f(6, 8), c("0123", "45"))
  expect_equal(f(5, 8), c("0123", "4"))
  expect_equal(f(7, 8), c("0123", "45", "6"))
})
