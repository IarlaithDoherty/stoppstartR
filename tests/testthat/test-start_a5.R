test_that("Not Relevant", {
  start_a5_irrelevant1 <- expand.grid(
    Age = c(85),
    Comorbidity_1 = c("I20", "I21", "I22", "I24", "I25", "I63", "I64", "I65",
                      "I66", "I73.9", "I74", "G45", "Z95.1", "Z95.5", "Z95.8"),
    Drug_1 = c(NA, "C10AA")
  )

  start_a5_irrelevant2 <- expand.grid(
    Age = c(85),
    Comorbidity_1 = c(NA),
    Drug_1 = c(NA, "C10AA")
  )

  start_a5_irrelevant <- rbind(start_a5_irrelevant1, start_a5_irrelevant2)

  expect_setequal(
    start_a5(start_a5_irrelevant,
             comorb_string = "Comorbidity_", drug_string = "Drug_",
             age_column = "Age"),
    "Not Relevant"
  )
})

test_that("Appropriate", {
  start_a5_appropriate <- expand.grid(
    Age = c(84),
    Comorbidity_1 = c("I20", "I21", "I22", "I24", "I25", "I63", "I64", "I65",
                      "I66", "I73.9", "I74", "G45", "Z95.1", "Z95.5", "Z95.8"),
    Drug_1 = c("C10AA")
  )

  expect_setequal(
    start_a5(start_a5_appropriate,
             comorb_string = "Comorbidity_", drug_string = "Drug_",
             age_column = "Age"),
    "Appropriate"
  )
})

test_that("Triggered", {
  start_a5_triggered <- expand.grid(
    Age = c(84),
    Comorbidity_1 = c("I20", "I21", "I22", "I24", "I25", "I63", "I64", "I65",
                      "I66", "I73.9", "I74", "G45", "Z95.1", "Z95.5", "Z95.8"),
    Drug_1 = c(NA)
  )

  expect_setequal(
    start_a5(start_a5_triggered,
             comorb_string = "Comorbidity_", drug_string = "Drug_",
             age_column = "Age"),
    "START-A5"
  )
})
