test_that("md5_hex and sample_bucket are deterministic", {
  hashes <- md5_hex(c("a", "b"))
  expect_equal(length(hashes), 2)
  expect_equal(hashes, md5_hex(c("a", "b")))

  buckets <- sample_bucket(c("s1", "s2"))
  expect_true(all(nchar(buckets) == 2))
})
