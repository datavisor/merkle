context("merkle_hasher")

test_that("hasher operations", {
  h <- merkle_hasher("md4")
  expect_equal(h$name(), "md4")

  expect_equal(h$hash_empty(), openssl::md4(raw()))
  expect_equal(h$hash_leaf(as.raw(1)), openssl::md4(as.raw(0:1)))
  expect_equal(h$hash_node(as.raw(2), as.raw(3)),
               openssl::md4(as.raw(1:3)))

  expect_equal(h$hash_tree(list()), list(list(h$hash_empty())))
})
