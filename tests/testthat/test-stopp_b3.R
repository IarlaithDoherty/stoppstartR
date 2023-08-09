#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b3_df <- data.frame(
  Drug_1 = c("C08DA01", "C08DA01", NA),
  Drug_2 = c(    "C07",        NA, NA))

test_that("stopp_b3 works", {
  expect_equal(
    stopp_b3(stopp_b3_df),
    c("STOPP-B3", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b3_drugs1 <- c("C08DA01", "C09BB10", "C08DA51", "C08DB01")
stopp_b3_drugs2 <- c("C07")

stopp_b3_trigger <- data.frame(
  Drug_1 = expand.grid(stopp_b3_drugs1, stopp_b3_drugs2)[, 1],
  Drug_2 = expand.grid(stopp_b3_drugs1, stopp_b3_drugs2)[, 2])

stopp_b3_appropriate <- data.frame(
  Drug_1 = stopp_b3_drugs1,
  Drug_2 = NA)

test_that("all triggered", {
  expect_equal(
    stopp_b3(stopp_b3_trigger),
    rep("STOPP-B3", length(stopp_b3_drugs1) * length(stopp_b3_drugs2)))
})

test_that("all appropriate", {
  expect_equal(
    stopp_b3(stopp_b3_appropriate),
    rep("Appropriate", length(stopp_b3_drugs1)))
})
