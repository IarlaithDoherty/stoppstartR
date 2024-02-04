test_that("Not Relevant", {
  start_a3_irrelevant1 <- expand.grid(
    Comorbidity_1 = c("I48"),
    Comorbidity_2 = c(
      NA, "I20", "I21", "I22", "I24", "I25", "I63", "I64", "I65", "I66",
      "I73.9", "I74", "G45", "Z95.1", "Z95.5", "Z95.8"),
    Drug_1 = c(NA, "B01AC")
  )

  start_a3_irrelevant2 <- expand.grid(
    Comorbidity_1 = c(NA),
    Comorbidity_2 = c(NA),
    Drug_1 = c(NA, "B01AC")
  )

  start_a3_irrelevant <- rbind(start_a3_irrelevant1, start_a3_irrelevant2)

  expect_setequal(
    start_a3(start_a3_irrelevant,
             comorb_string = "Comorbidity_", drug_string = "Drug_"),
    "Not Relevant"
  )
})

test_that("Appropriate", {
  start_a3_appropriate <- expand.grid(
    Comorbidity_1 = c(NA),
    Comorbidity_2 = c(
      "I20", "I21", "I22", "I24", "I25", "I63", "I64", "I65", "I66",
      "I73.9", "I74", "G45", "Z95.1", "Z95.5", "Z95.8"),
    Drug_1 = c("B01AC")
  )

  expect_setequal(
    start_a3(start_a3_appropriate,
             comorb_string = "Comorbidity_", drug_string = "Drug_"),
    "Appropriate"
  )
})

test_that("Triggered", {
  start_a3_triggered <- expand.grid(
    Comorbidity_1 = c(NA),
    Comorbidity_2 = c(
      "I20", "I21", "I22", "I24", "I25", "I63", "I64", "I65", "I66",
      "I73.9", "I74", "G45", "Z95.1", "Z95.5", "Z95.8"),
    Drug_1 = c(NA)
  )

  expect_setequal(
    start_a3(start_a3_triggered,
             comorb_string = "Comorbidity_", drug_string = "Drug_"),
    "START-A3"
  )
})

