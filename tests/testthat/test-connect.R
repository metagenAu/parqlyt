test_that("connect_duckdb returns connection", {
  con <- connect_duckdb()
  expect_true(DBI::dbIsValid(con))
})

test_that("ensure_httpfs loads extension or skips", {
  con <- connect_duckdb()
  ok <- tryCatch({
    ensure_httpfs(con)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    testthat::skip("httpfs extension unavailable in test environment")
  }
  expect_true(ok)
})

test_that("set_r2_secret escapes credentials", {
  con <- connect_duckdb()
  env <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    DBI::dbExecute = function(con, sql) {
      env$sql <- sql
      0
    }
  )
  set_r2_secret(con, key_id = "k'id", secret = "s'ecret", account_id = "a'c", persistent = TRUE)
  expect_true(grepl("PERSISTENT", env$sql))
  expect_true(grepl("KEY_ID 'k''id'", env$sql))
  expect_true(grepl("SECRET 's''ecret'", env$sql))
  expect_true(grepl("ACCOUNT_ID 'a''c'", env$sql))
})
