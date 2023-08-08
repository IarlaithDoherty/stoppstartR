#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c6_df <- data.frame(
  GFR = c(     31,        31, 31,      30),
  Comorbidity_1      = c("G25.8",   "G25.8", NA, "G25.8"),
  Drug_1             = c(     NA, "N04BC04", NA,      NA))
colnames(start_c6_df)[1] <- "Lab Values: eGFR"

test_that("start_c6 works", {
  expect_equal(
    start_c6(start_c6_df),
    c("START-C6", "Appropriate", "Not Relevant", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c6_comorbs <- c("G25.8")
start_c6_drugs   <- c("N04BC04", "N04BC05", "N04BC09")

start_c6_trigger <- data.frame(
  GFR           = 31,
  Comorbidity_1 = start_c6_comorbs,
  Drug_1        = NA)
colnames(start_c6_trigger)[1] <- "Lab Values: eGFR"

start_c6_appropriate <- data.frame(
  GFR           = 31,
  Comorbidity_1 = expand.grid(start_c6_comorbs, start_c6_drugs)[, 1],
  Drug_1        = expand.grid(start_c6_comorbs, start_c6_drugs)[, 2])
colnames(start_c6_appropriate)[1] <- "Lab Values: eGFR"

test_that("all triggered", {
  expect_equal(
    start_c6(start_c6_trigger),
    rep("START-C6", length(start_c6_comorbs)))
})

test_that("all appropriate", {
  expect_equal(
    start_c6(start_c6_appropriate),
    rep("Appropriate", length(start_c6_comorbs)*length(start_c6_drugs)))
})
