#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b6_df <- data.frame(
  Comorbidity_1 = c("I10", "I10", "I10", "I10", NA),
  Comorbidity_2 = c(NA,    NA, "I50",    NA, NA),
  Drug_1        = c(NA,    NA,    NA, "C07", NA),
  Drug_2        = c("C03C",    NA,    NA,    NA, NA))

test_that("stopp_b6 works", {
  expect_equal(
    stopp_b6(stopp_b6_df),
    c("STOPP-B6", "Appropriate",
      "Not Relevant", "Not Relevant", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b6_comorbs1 <- c("I10", "I15")
stopp_b6_comorbs2 <- c("I50", "I11.0", "I13.0", "I13.2")
stopp_b6_drugs1   <- c("C07", "C08", "C09", "C03A", "C03EA")
stopp_b6_drugs2   <- c("C03C", "C03EB")

stopp_b6_trigger <- data.frame(
  Comorbidity_1 = expand.grid(stopp_b6_comorbs1, stopp_b6_drugs2)[, 1],
  Comorbidity_2 = NA,
  Drug_1        = NA,
  Drug_2        = expand.grid(stopp_b6_comorbs1, stopp_b6_drugs2)[, 2])

stopp_b6_appropriate <- data.frame(
  Comorbidity_1 = stopp_b6_comorbs1,
  Comorbidity_2 = NA,
  Drug_1        = NA,
  Drug_2        = NA)

test_that("all triggered", {
  expect_equal(
    stopp_b6(stopp_b6_trigger),
    rep("STOPP-B6", length(stopp_b6_comorbs1) * length(stopp_b6_drugs2)))
})

test_that("all appropriate", {
  expect_equal(
    stopp_b6(stopp_b6_appropriate),
    rep("Appropriate", length(stopp_b6_comorbs1)))
})
