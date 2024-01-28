#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c1_df <- data.frame(
  Comorbidity_1 = c("G20",  "G20", NA),
  Drug_1        = c(NA, "N04B", NA))

test_that("start_c1 works", {
  expect_equal(
    start_c1(start_c1_df),
    c("START-C1", "Appropriate", "Not Relevant"))
})

# No need for comprehensive test as the above basic test covers all scenarios.
