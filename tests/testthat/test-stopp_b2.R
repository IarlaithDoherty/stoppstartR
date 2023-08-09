#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b2_df <- data.frame(
  Comorbidity_1 = c(    "I50", "I50", NA),
  Drug_1        = c("C08DB01",    NA, NA))

test_that("stopp_b2 works", {
  expect_equal(
    stopp_b2(stopp_b2_df),
    c("STOPP-B2", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b2_comorbs <- c("I50", "I11.0", "I13.0", "I13.2")
stopp_b2_drugs   <- c("C08DB01", "C08DA01", "C09BB10", "C08DA51")

stopp_b2_trigger <- data.frame(
  Comorbidity_1 = expand.grid(stopp_b2_comorbs, stopp_b2_drugs)[, 1],
  Drug_1        = expand.grid(stopp_b2_comorbs, stopp_b2_drugs)[, 2])

stopp_b2_appropriate <- data.frame(
  Comorbidity_1 = stopp_b2_comorbs,
  Drug_1        = NA)

test_that("all triggered", {
  expect_equal(
    stopp_b2(stopp_b2_trigger),
    rep("STOPP-B2", length(stopp_b2_comorbs) * length(stopp_b2_drugs)))
})

test_that("all appropriate", {
  expect_equal(
    stopp_b2(stopp_b2_appropriate),
    rep("Appropriate", length(stopp_b2_comorbs)))
})
