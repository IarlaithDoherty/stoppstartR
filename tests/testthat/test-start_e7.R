#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e7_df <- data.frame(
  Drug_1 = c("L04AX03", "L04AX03", NA),
  Drug_2 = c(NA,   "B03AD", NA)
)

test_that("start_e7 works", {
  expect_equal(
    start_e7(start_e7_df),
    c("START-E7", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e7_drugs1 <- c("L04AX03", "L01BA01")
start_e7_drugs2 <- c("B03AD", "B03AE02", "B03AE01", "B03BB")

start_e7_trigger <- data.frame(
  Drug_1 = start_e7_drugs1,
  Drug_2 = NA
)

start_e7_appropriate <- data.frame(
  Drug_1 = expand.grid(start_e7_drugs1, start_e7_drugs2)[, 1],
  Drug_2 = expand.grid(start_e7_drugs1, start_e7_drugs2)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_e7(start_e7_trigger),
    rep("START-E7", length(start_e7_drugs1))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_e7(start_e7_appropriate),
    rep("Appropriate", length(start_e7_drugs1) * length(start_e7_drugs2))
  )
})
