#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_d2_df <- data.frame(
  Comorbidity_1 = c("K57",   "K57", "K57",      NA),
  Comorbidity_2 = c("K59.0", "K59.0",    NA, "K59.0"),
  Drug_1        = c(NA, "A06AC",    NA,      NA))

test_that("start_d2 works", {
  expect_equal(
    start_d2(start_d2_df),
    c("START-D2", "Appropriate", "Not Relevant", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_d2_comorbs1 <- c("K57")
start_d2_comorbs2 <- c("K59.0")
start_d2_drugs    <- c("A06AC")

start_d2_trigger <- data.frame(
  Comorbidity_1 = expand.grid(start_d2_comorbs1, start_d2_comorbs2)[, 1],
  Comorbidity_2 = expand.grid(start_d2_comorbs1, start_d2_comorbs2)[, 2],
  Drug_1        = NA)

start_d2_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_d2_comorbs1, start_d2_comorbs2,
                              start_d2_drugs)[, 1],
  Comorbidity_2 = expand.grid(start_d2_comorbs1, start_d2_comorbs2,
                              start_d2_drugs)[, 2],
  Drug_1        = expand.grid(start_d2_comorbs1, start_d2_comorbs2,
                              start_d2_drugs)[, 3])

test_that("all triggered", {
  expect_equal(
    start_d2(start_d2_trigger),
    rep("START-D2", length(start_d2_comorbs1) * length(start_d2_comorbs2)))
})

test_that("all appropriate", {
  expect_equal(
    start_d2(start_d2_appropriate),
    rep("Appropriate",
        length(start_d2_comorbs1)
        * length(start_d2_comorbs2) * length(start_d2_drugs)))
})
