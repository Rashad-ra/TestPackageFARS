library(testthat)
library(TestPackageFars)


test_that("fars_read works for valid file", {
  expect_error(fars_read("nonexistent.csv"), "file 'nonexistent.csv' does not exist")
  # Assuming there's a valid file "test.csv" in your working directory
  # expect_silent(fars_read("test.csv"))
})

test_that("make_filename creates correct filename", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})



# Check if data_list is a list with expected length
expect_is(data_list, "list")
expect_length(data_list, length(years))

# Test cases for fars_read function
test_that("fars_read correctly reads a valid file", {
  # Create a temporary file for testing
  tmp_file <- tempfile()
  on.exit(unlink(tmp_file))  # Cleanup

  # Write test data to the temporary file
  write.csv(data.frame(STATE = 1:10, MONTH = rep(1:12, 1)), tmp_file, row.names = FALSE)

  # Test reading the temporary file
  result <- fars_read(tmp_file)

  # Assertions using expect_* functions
  expect_is(result, "tbl_df")
  expect_equal(nrow(result), 12)  # Assuming 12 rows based on test data
})

# Test cases for make_filename function
test_that("make_filename correctly generates file names", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
  expect_equal(make_filename(2020), "accident_2020.csv.bz2")
  expect_error(make_filename("invalid"), "must be coercible to integer")
})

