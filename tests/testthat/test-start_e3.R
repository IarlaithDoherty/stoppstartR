#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e3_df <- data.frame(
  Comorbidity_1 = c("M80",   "M80",   "M80", NA),
  Drug_1        = c("A11CC",      NA, "A11CC", NA),
  Drug_2        = c(NA, "A12AA", "A12AA", NA)
)

test_that("start_e3 works", {
  expect_equal(
    start_e3(start_e3_df),
    c("START-E3", "START-E3", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e3_comorbs <- c("M80", "M81",
                      "S12", "S22", "S32", "S42", "S52",
                      "S62", "S72", "S82", "S92",
                      "T02", "T08", "T10", "T12", "T14.2")
start_e3_drugs1  <- c("A11CB", "A12AX", "A11CC",
                      "M05BB03", "M05BB04", "M05BB05",
                      "M05BB06", "M05BB07", "M05BB08")
start_e3_drugs1b <- c("A11CB", "A11CC",
                      "M05BB03", "M05BB06", "M05BB07")
start_e3_drugs2  <- c("A11GB01", "A12AA02", "A12AX", "A12AA",
                      "M05BB01", "M05BB02", "M05BB04", "M05BB05", "M05BB08")
start_e3_drugs2b <- c("A11GB01", "A12AA02", "A12AA",
                      "M05BB01", "M05BB02")


start_e3_trigger1 <- data.frame(
  Comorbidity_1 = expand.grid(start_e3_comorbs, NA, start_e3_drugs2b)[, 1],
  Drug_1        = NA,
  Drug_2        = expand.grid(start_e3_comorbs, NA, start_e3_drugs2b)[, 3]
)

start_e3_trigger2 <- data.frame(
  Comorbidity_1 = expand.grid(start_e3_comorbs, NA, start_e3_drugs2b)[, 1],
  Drug_1        = expand.grid(start_e3_comorbs, NA, start_e3_drugs2b)[, 2],
  Drug_2        = NA
)

start_e3_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_e3_comorbs,
                              start_e3_drugs1, start_e3_drugs2)[, 1],
  Drug_1        = expand.grid(start_e3_comorbs,
                              start_e3_drugs1, start_e3_drugs2)[, 2],
  Drug_2        = expand.grid(start_e3_comorbs,
                              start_e3_drugs1, start_e3_drugs2)[, 3]
)

test_that("all triggered 1", {
  expect_equal(
    start_e3(start_e3_trigger1),
    rep("START-E3", length(start_e3_comorbs) * length(start_e3_drugs2b))
  )
})

test_that("all triggered 2", {
  expect_equal(
    start_e3(start_e3_trigger2),
    rep("START-E3", length(start_e3_comorbs) * length(start_e3_drugs1b))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_e3(start_e3_appropriate),
    rep("Appropriate",
        length(start_e3_comorbs) * length(start_e3_drugs1)
        * length(start_e3_drugs2))
  )
})
