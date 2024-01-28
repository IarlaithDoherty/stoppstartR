#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b1_df <- data.frame(
  Comorbidity_1 = c("I50", "I50", "I50", NA),
  Comorbidity_2 = c(NA,    NA, "I48", NA),
  Drug_1        = c("C01AA05",    NA,    NA, NA))

test_that("stopp_b1 works", {
  expect_equal(
    stopp_b1(stopp_b1_df),
    c("STOPP-B1", "Appropriate", "Not Relevant", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b1_comorbs1 <- c("I50", "I11.0", "I13.0", "I13.2")
stopp_b1_comorbs2 <- c("I48")
stopp_b1_drugs    <- c("C01AA05")

stopp_b1_trigger <- data.frame(
  Comorbidity_1 = expand.grid(stopp_b1_comorbs1, stopp_b1_drugs)[, 1],
  Comorbidity_2 = NA,
  Drug_1        = expand.grid(stopp_b1_comorbs1, stopp_b1_drugs)[, 2])

stopp_b1_appropriate <- data.frame(
  Comorbidity_1 = stopp_b1_comorbs1,
  Comorbidity_2 = NA,
  Drug_1        = NA)

test_that("all triggered", {
  expect_equal(
    stopp_b1(stopp_b1_trigger),
    rep("STOPP-B1", length(stopp_b1_comorbs1) * length(stopp_b1_drugs)))
})

test_that("all appropriate", {
  expect_equal(
    stopp_b1(stopp_b1_appropriate),
    rep("Appropriate", length(stopp_b1_comorbs1)))
})
