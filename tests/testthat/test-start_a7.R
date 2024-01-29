#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a7_df <- data.frame(
  Comorbidity_1 = c("I20", "I20", NA),
  Drug_1        = c(NA, "C07", NA)
)

test_that("start_a7 works", {
  expect_equal(
    start_a7(start_a7_df),
    c("START-A7", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a7_comorbs <- c("I20", "I21", "I22", "I24", "I25", "Z95.1", "Z95.5")
start_a7_drugs   <- c("C07")

start_a7_trigger <- data.frame(
  Comorbidity_1 = start_a7_comorbs,
  Drug_1        = NA
)

start_a7_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_a7_comorbs, start_a7_drugs)[, 1],
  Drug_1        = expand.grid(start_a7_comorbs, start_a7_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_a7(start_a7_trigger),
    rep("START-A7", length(start_a7_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_a7(start_a7_appropriate),
    rep("Appropriate", length(start_a7_comorbs) * length(start_a7_drugs))
  )
})
