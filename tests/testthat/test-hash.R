context("hash")

test_that("hash_function", {
  expect_is(hash_function("md4"), "function")
  expect_error(hash_function("md7"), "Unknown hash function 'md7'",
               fixed = TRUE)
})

test_that("hash_functions_get", {
  tmp <- hash_functions_get()
  expect_identical(tmp, hash_functions)
})

test_that("as_hash", {
  bytes <- sample(as.raw(0:255), 32, replace = TRUE)
  hash <- structure(bytes, class = c("hash", "md5"))
  str <- as.character(hash)
  str2 <- as.character(hash, ":")

  expect_identical(as_hash(bytes), bytes)
  expect_identical(as_hash(str), bytes)
  expect_identical(as_hash(str2), bytes)

  expect_identical(as_hash(str, "md5"), hash)
  expect_identical(as_hash(str2, "md5"), hash)
})
