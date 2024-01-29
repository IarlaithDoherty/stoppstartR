#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a1_df <- data.frame(
  Comorbidity_1 = c("I48.2", "I48.2", NA),
  Drug_1        = c(NA, "B01AA", NA)
)

test_that("start_a1 works", {
  expect_equal(
    start_a1(start_a1_df),
    c("START-A1", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a1_comorbs <- c("I48.2")
start_a1_drugs   <- c("B01AA", "B01AE", "B01AF")

start_a1_trigger <- data.frame(
  Comorbidity_1 = start_a1_comorbs,
  Drug_1        = NA
)

start_a1_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_a1_comorbs, start_a1_drugs)[, 1],
  Drug_1        = expand.grid(start_a1_comorbs, start_a1_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_a1(start_a1_trigger),
    rep("START-A1", length(start_a1_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_a1(start_a1_appropriate),
    rep("Appropriate", length(start_a1_comorbs) * length(start_a1_drugs))
  )
})
