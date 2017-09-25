context("merkle_tree")

test_that("proof-of-concept", {
  tr <- merkle_tree()

  for (l in letters[1:6]) {
    tr$add_leaf(tr$digest_string(l))
  }

  tr$compute()

  pr <- tr$proof(1)
  expect_is(pr, "merkle_proof")
  expect_true(setequal(names(pr), c("leaf", "root", "chain")))
  expect_equal(pr$leaf, tr$leaf(1))
  expect_equal(pr$root, tr$root())
  expect_equal(length(pr$chain), 3L)
  expect_equal(names(pr$chain), rep("right", 3L))

  tree <- environment(tr$leaf)$private$tree
  expect_equal(pr$chain,
               list(right = tree[[1]][[2]],
                    right = tree[[2]][[2]],
                    right = tree[[3]][[2]]))
  expect_true(tr$validate(pr, tr$leaf(1), tr$root()))
  expect_false(tr$validate(pr, tr$leaf(2), tr$root()))
})
