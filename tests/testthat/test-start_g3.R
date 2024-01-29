#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_g3_df <- data.frame(
  Gender        = c("F",       "F", "F",     "M",        NA),
  Comorbidity_1 = c("N95.2",   "N95.2",  NA, "N95.2",   "N95.2"),
  Drug_1        = c(NA, "G03CA03",  NA,      NA, "G03CA03")
)

test_that("start_g3 works", {
  expect_equal(
    start_g3(start_g3_df),
    c("START-G3", "Appropriate", "Not Relevant", "Not Relevant", NA)
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_g3_comorbs <- c("N95.2")
start_g3_drugs   <- c("G03CA03", "G03CA04")

start_g3_trigger <- data.frame(
  Gender        = "F",
  Comorbidity_1 = start_g3_comorbs,
  Drug_1        = NA
)

start_g3_appropriate <- data.frame(
  Gender        = "F",
  Comorbidity_1 = expand.grid(start_g3_comorbs, start_g3_drugs)[, 1],
  Drug_1        = expand.grid(start_g3_comorbs, start_g3_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_g3(start_g3_trigger),
    rep("START-G3", length(start_g3_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_g3(start_g3_appropriate),
    rep("Appropriate", length(start_g3_comorbs) * length(start_g3_drugs))
  )
})
