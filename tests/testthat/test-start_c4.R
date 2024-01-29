#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c4_df <- data.frame(
  Comorbidity_1 = c("H40.1", "H40.1", NA),
  Drug_1        = c(NA, "S01EE", NA)
)

test_that("start_c4 works", {
  expect_equal(
    start_c4(start_c4_df),
    c("START-C4", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c4_comorbs <- c("H40.1")
start_c4_drugs   <- c("S01EE", "S01ED")

start_c4_trigger <- data.frame(
  Comorbidity_1 = start_c4_comorbs,
  Drug_1        = NA
)

start_c4_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_c4_comorbs, start_c4_drugs)[, 1],
  Drug_1        = expand.grid(start_c4_comorbs, start_c4_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_c4(start_c4_trigger),
    rep("START-C4", length(start_c4_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_c4(start_c4_appropriate),
    rep("Appropriate", length(start_c4_comorbs) * length(start_c4_drugs))
  )
})
