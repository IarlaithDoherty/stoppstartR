#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a8_df <- data.frame(
  Comorbidity_1 = c("I50",     "I50", NA),
  Drug_1        = c(NA, "C07AB07", NA)
)

test_that("start_a8 works", {
  expect_equal(
    start_a8(start_a8_df),
    c("START-A8", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

comorbs <- c("I50", "I11.0", "I13.0", "I13.2")
drugs   <- c("C07AB07", "C07AB12", "C07AB02", "C07AG02",
             "C07FB07", "C07BB07", "C07FB12", "C07BB12",
             "C07FB02", "C07CB02", "C07BB02", "C07BB52")

start_a8_trigger <- data.frame(
  Comorbidity_1 = comorbs,
  Drug_1        = NA
)

start_a8_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(comorbs, drugs)[, 1],
  Drug_1        = expand.grid(comorbs, drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_a8(start_a8_trigger),
    rep("START-A8", length(comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_a8(start_a8_appropriate),
    rep("Appropriate", length(comorbs) * length(drugs))
  )
})
