context("merkle_tree")

test_that("empty", {
  mt <- merkle_tree()
  expect_identical(mt$length(), 0L)
  expect_identical(mt$height(), 1L)
  expect_equal(mt$root(), mt$hasher()$hash_empty())
})

test_that("build", {
  mt <- merkle_tree()
  hasher <- mt$hasher()

  d <- as.raw(0:1)

  mt$append(d[[1]])
  mt$append(d[[2]])

  expect_equal(mt$length(), 2L)
  expect_equal(mt$height(), 2L)

  leaves <- mt$leaves()
  expect_equal(leaves, lapply(d, hasher$hash_leaf))

  expect_null(r6_private(mt)$.tree)
  r <- mt$root()
  expect_equal(r6_private(mt)$.tree, list(leaves, list(r)))
  expect_equal(r, hasher$hash_node(leaves[[1]], leaves[[2]]))
})

test_that("audit proof", {
  tr <- merkle_tree()
  tr$extend(as.list(as.raw(0:5)))

  pr <- tr$proof_audit(1)
  expect_is(pr, "merkle_audit_proof")

  expect_true(setequal(names(pr),
                       c("index", "hash_name", "leaf", "root", "chain")))
  expect_equal(pr$leaf, tr$leaf(1))
  expect_equal(pr$root, tr$root())
  expect_equal(length(pr$chain), 3L)
  expect_equal(names(pr$chain), rep("right", 3L))

  tree <- tr$tree()
  expect_equal(pr$chain,
               list(right = tree[[1]][[2]],
                    right = tree[[2]][[2]],
                    right = tree[[3]][[2]]))

  expect_true(merkle_audit_proof_check(pr))
  pr$leaf <- tr$leaf(2)
  expect_false(merkle_audit_proof_check(pr))
})

test_that("index", {
  mt <- merkle_tree()
  mt$extend(as.list(as.raw(1:10)))
  expect_equal(viapply(mt$leaves(), mt$index), seq_len(10))

  h <- mt$hasher()$hash_leaf(as.raw(100))
  expect_identical(mt$index(h), NA_integer_)
  expect_identical(mt$index(h, -1L), -1L)
})

test_that("consistency proof", {
  tr3 <- merkle_tree()$extended(as.list(as.raw(1:3)))
  tr4 <- tr3$extended(as.list(as.raw(4)))
  tr6 <- tr4$extended(as.list(as.raw(5:6)))
  tr7 <- tr6$extended(as.list(as.raw(7)))

  pr3 <- tr7$proof_consistency(3)
  pr4 <- tr7$proof_consistency(4)
  pr6 <- tr7$proof_consistency(6)
  pr7 <- tr7$proof_consistency(7)

  hasher <- tr7$hasher()

  expect_true(merkle_consistency_test(pr3, tr3$root(), hasher))
  expect_true(merkle_consistency_test(pr4, tr4$root(), hasher))
  expect_true(merkle_consistency_test(pr6, tr6$root(), hasher))
  expect_true(merkle_consistency_test(pr7, tr7$root(), hasher))

  expect_false(merkle_consistency_test(pr7, tr3$root(), hasher))
})

test_that("print", {
  mt <- merkle_tree()
  hasher <- mt$hasher()

  d <- as.raw(0:1)

  mt$append(d[[1]])
  mt$append(d[[2]])

  pr <- mt$proof_audit(1)
  expected <- c("merkle audit proof:",
                "  hash_name: sha256",
                "  leaf: 96:a2:96:d2:24:f2:85:c6:7b:...",
                "  root: a2:0b:f9:a7:cc:2d:c8:a0:8f:...",
                "  index: 1", "  chain:",
                "  - b4:13:f4:7d:13:ee:2f:e6:c8:... (right)")
  recv <- capture.output(print(pr, 30))
  expect_equal(recv, expected)
})
