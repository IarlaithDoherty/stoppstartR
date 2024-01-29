#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a6_df <- data.frame(
  Comorbidity_1 = c("I20", "I20", NA),
  Drug_1        = c(NA, "C09", NA)
)

test_that("start_a6 works", {
  expect_equal(
    start_a6(start_a6_df),
    c("START-A6", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a6_comorbs <- c("I20", "I21", "I22", "I24", "I25", "Z95.1", "Z95.5")
start_a6_drugs   <- c("C09")

start_a6_trigger <- data.frame(
  Comorbidity_1 = start_a6_comorbs,
  Drug_1        = NA
)

start_a6_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_a6_comorbs, start_a6_drugs)[, 1],
  Drug_1        = expand.grid(start_a6_comorbs, start_a6_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_a6(start_a6_trigger),
    rep("START-A6", length(start_a6_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_a6(start_a6_appropriate),
    rep("Appropriate", length(start_a6_comorbs) * length(start_a6_drugs))
  )
})
