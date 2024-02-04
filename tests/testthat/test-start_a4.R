test_that("Not Relevant", {
  start_a4_irrelevant1 <- expand.grid(
    Diastolic = c(90),
    Systolic = c(160),
    Comorbidity_1 = c(NA),
    Drug_1 = c(NA, "C07", "C08", "C09", "C03A", "C03EA")
  )

  start_a4_irrelevant2 <- expand.grid(
    Diastolic = c(90),
    Systolic = c(140),
    Comorbidity_1 = c("E10", "E11", "E12", "E13", "E14"),
    Drug_1 = c(NA, "C07", "C08", "C09", "C03A", "C03EA")
  )

  start_a4_irrelevant <- rbind(start_a4_irrelevant1, start_a4_irrelevant2)

  expect_setequal(
    start_a4(start_a4_irrelevant,
             comorb_string = "Comorbidity_", drug_string = "Drug_",
             diastolic_column = "Diastolic",
             systolic_column = "Systolic"),
    "Not Relevant"
  )
})

test_that("Appropriate", {
  start_a4_appropriate1 <- expand.grid(
    Diastolic = c(90),
    Systolic = c(161),
    Comorbidity_1 = c(NA),
    Drug_1 = c("C07", "C08", "C09", "C03A", "C03EA")
  )

  start_a4_appropriate2 <- expand.grid(
    Diastolic = c(91),
    Systolic = c(160),
    Comorbidity_1 = c(NA),
    Drug_1 = c("C07", "C08", "C09", "C03A", "C03EA")
  )

  start_a4_appropriate3 <- expand.grid(
    Diastolic = c(90),
    Systolic = c(141),
    Comorbidity_1 = c("E10", "E11", "E12", "E13", "E14"),
    Drug_1 = c("C07", "C08", "C09", "C03A", "C03EA")
  )

  start_a4_appropriate <- rbind(start_a4_appropriate1, start_a4_appropriate2,
                                start_a4_appropriate3)

  expect_setequal(
    start_a4(start_a4_appropriate,
             comorb_string = "Comorbidity_", drug_string = "Drug_",
             diastolic_column = "Diastolic",
             systolic_column = "Systolic"),
    "Appropriate"
  )
})

test_that("Triggered", {
  start_a4_triggered1 <- expand.grid(
    Diastolic = c(90),
    Systolic = c(161),
    Comorbidity_1 = c(NA),
    Drug_1 = c(NA)
  )

  start_a4_triggered2 <- expand.grid(
    Diastolic = c(91),
    Systolic = c(160),
    Comorbidity_1 = c(NA),
    Drug_1 = c(NA)
  )

  start_a4_triggered3 <- expand.grid(
    Diastolic = c(90),
    Systolic = c(141),
    Comorbidity_1 = c("E10", "E11", "E12", "E13", "E14"),
    Drug_1 = c(NA)
  )

  start_a4_triggered <- rbind(start_a4_triggered1, start_a4_triggered2,
                              start_a4_triggered3)


  expect_setequal(
    start_a4(start_a4_triggered,
             comorb_string = "Comorbidity_", drug_string = "Drug_",
             diastolic_column = "Diastolic",
             systolic_column = "Systolic"),
    "START-A4"
  )
})

