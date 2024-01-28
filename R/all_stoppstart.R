#' Title
#'
#' @param df Patient info
#' @param comorb_string .
#' @param drug_string .
#' @param diastolic_column .
#' @param systolic_column .
#' @param age_column .
#' @param HR_column .
#' @param K_column .
#' @param gender_column .
#' @param excluded_criteria .
#' @param included_criteria .
#'
#' @return Data frame of character vectors.
#' @export
all_stoppstart <- function(
    df,
    comorb_string = "Comorbidity_",
    drug_string = "Drug_",
    diastolic_column = "Diastolic_BP",
    systolic_column = "Systolic_BP",
    age_column = "Age",
    HR_column = "HR_column",
    K_column = "K_column",
    gender_column = "Gender",
    excluded_criteria = c(),
    included_criteria = c(
      "start_a1",   "start_a3",   "start_a4",   "start_a5",   "start_a6",
      "start_a7",   "start_a8",   "start_b1",   "start_c1",   "start_c2",
      "start_c3a",  "start_c3b",  "start_c4",   "start_c5",   "start_c6",
      "start_d1",   "start_d2",   "start_e1",   "start_e2",   "start_e3",
      "start_e4",   "start_e5",   "start_e6",   "start_e7",   "start_g1",
      "start_g2",   "start_g3",   "start_h2",
      "stopp_b1",   "stopp_b2",   "stopp_b3",   "stopp_b4",   "stopp_b5",
      "stopp_b6",   "stopp_b7",   "stopp_b8",   "stopp_b9",   "stopp_b10",
      "stopp_b11",  "stopp_b13a", "stopp_b13b", "stopp_c1",   "stopp_c2",
      "stopp_c5",   "stopp_c7",   "stopp_c8",   "stopp_c9",   "stopp_c10",
      "stopp_c11",  "stopp_d1",   "stopp_d3",   "stopp_d4",   "stopp_d5",
      "stopp_d6",   "stopp_d7",   "stopp_d8",   "stopp_d9",   "stopp_d10",
      "stopp_d11",  "stopp_d12",  "stopp_d13",  "stopp_d14",  "stopp_e1",
      "stopp_e2",   "stopp_e3",   "stopp_e4",   "stopp_e5",   "stopp_e6",
      "stopp_f1",   "stopp_f2",   "stopp_f3",   "stopp_f4",   "stopp_g1",
      "stopp_g2",   "stopp_g4",   "stopp_h1",   "stopp_h2",   "stopp_h4",
      "stopp_h5",   "stopp_h6",   "stopp_h7",   "stopp_h8",   "stopp_h9",
      "stopp_i1",   "stopp_i2",   "stopp_j1",   "stopp_j2",   "stopp_j4",
      "stopp_j5",   "stopp_j6",   "stopp_k1",   "stopp_k2",   "stopp_k3",
      "stopp_k4",   "stopp_l2",   "stopp_l3",   "stopp_m1"
    )
) {

  included_criteria <- setdiff(included_criteria, excluded_criteria)

  output <- as.data.frame(matrix(nrow = nrow(df), ncol = 0))

  if ("start_a1" %in% included_criteria) {
    output$start_a1 <- start_a1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_a3" %in% included_criteria) {
    output$start_a3 <- start_a3(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_a4" %in% included_criteria) {
    output$start_a4 <- start_a4(
      df, comorb_string = comorb_string, drug_string = drug_string,
      diastolic_column = diastolic_column,
      systolic_column = systolic_column
    )
  }

  if ("start_a5" %in% included_criteria) {
    output$start_a5 <- start_a5(
      df, comorb_string = comorb_string, drug_string = drug_string,
      age_column = age_column
    )
  }

  if ("start_a6" %in% included_criteria) {
    output$start_a6 <- start_a6(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_a7" %in% included_criteria) {
    output$start_a7 <- start_a7(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_a8" %in% included_criteria) {
    output$start_a8 <- start_a8(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_b1" %in% included_criteria) {
    output$start_b1 <- start_b1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_c1" %in% included_criteria) {
    output$start_c1 <- start_c1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_c2" %in% included_criteria) {
    output$start_c2 <- start_c2(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_c3a" %in% included_criteria) {
    output$start_c3a <- start_c3a(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_c3b" %in% included_criteria) {
    output$start_c3b <- start_c3b(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_c4" %in% included_criteria) {
    output$start_c4 <- start_c4(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_c5" %in% included_criteria) {
    output$start_c5 <- start_c5(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_c6" %in% included_criteria) {
    output$start_c6 <- start_c6(
      df, comorb_string = comorb_string, drug_string = drug_string,
      GFR_column = "Lab Values: eGFR"
    )
  }

  if ("start_d1" %in% included_criteria) {
    output$start_d1 <- start_d1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_d2" %in% included_criteria) {
    output$start_d2 <- start_d2(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_e1" %in% included_criteria) {
    output$start_e1 <- start_e1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_e2" %in% included_criteria) {
    output$start_e2 <- start_e2(
      df, drug_string = drug_string
    )
  }

  if ("start_e3" %in% included_criteria) {
    output$start_e3 <- start_e3(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_e4" %in% included_criteria) {
    output$start_e4 <- start_e4(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_e5" %in% included_criteria) {
    output$start_e5 <- start_e5(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_e6" %in% included_criteria) {
    output$start_e6 <- start_e6(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("start_e7" %in% included_criteria) {
    output$start_e7 <- start_e7(
      df, drug_string = drug_string
    )
  }

  if ("start_g1" %in% included_criteria) {
    output$start_g1 <- start_g1(
      df, comorb_string = comorb_string, drug_string = drug_string,
      gender_column = gender_column
    )
  }

  if ("start_g2" %in% included_criteria) {
    output$start_g2 <- start_g2(
      df, comorb_string = comorb_string, drug_string = drug_string,
      gender_column = gender_column
    )
  }

  if ("start_g3" %in% included_criteria) {
    output$start_g3 <- start_g3(
      df, comorb_string = comorb_string, drug_string = drug_string,
      gender_column = gender_column
    )
  }

  if ("start_h2" %in% included_criteria) {
    output$start_h2 <- start_h2(
      df, drug_string = drug_string
    )
  }

  if ("stopp_b1" %in% included_criteria) {
    output$stopp_b1 <- stopp_b1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_b2" %in% included_criteria) {
    output$stopp_b2 <- stopp_b2(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_b3" %in% included_criteria) {
    output$stopp_b3 <- stopp_b3(
      df, drug_string = drug_string
    )
  }

  if ("stopp_b4" %in% included_criteria) {
    output$stopp_b4 <- stopp_b4(
      df, comorb_string = comorb_string, drug_string = drug_string,
      HR_column = "Lab Values: Heart Rate"
    )
  }

  if ("stopp_b5" %in% included_criteria) {
    output$stopp_b5 <- stopp_b5(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_b6" %in% included_criteria) {
    output$stopp_b6 <- stopp_b6(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_b7" %in% included_criteria) {
    output$stopp_b7 <- stopp_b7(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_b8" %in% included_criteria) {
    output$stopp_b8 <- stopp_b8(
      df, comorb_string = comorb_string, drug_string = drug_string,
      K_column = "Lab Values: K",
      Na_column = "Lab Values: Na",
      CCa_column = "Lab Values: Corrected Ca"
    )
  }

  if ("stopp_b9" %in% included_criteria) {
    output$stopp_b9 <- stopp_b9(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_b10" %in% included_criteria) {
    output$stopp_b10 <- stopp_b10(
      df, drug_string = drug_string
    )
  }

  if ("stopp_b11" %in% included_criteria) {
    output$stopp_b11 <- stopp_b11(
      df, comorb_string = comorb_string, drug_string = drug_string,
      K_column = "Lab Values: K"
    )
  }

  if ("stopp_b13a" %in% included_criteria) {
    output$stopp_b13a <- stopp_b13a(
      df, comorb_string = comorb_string, drug_string = drug_string,
      systolic_column = systolic_column
    )
  }

  if ("stopp_b13b" %in% included_criteria) {
    output$stopp_b13b <- stopp_b13b(
      df, drug_string = drug_string
    )
  }

  if ("stopp_c1" %in% included_criteria) {
    output$stopp_c1 <- stopp_c1(
      df, drug_string = drug_string,
      asa_column = "Aspirin dose >150mg"
    )
  }

  if ("stopp_c2" %in% included_criteria) {
    output$stopp_c2 <- stopp_c2(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_c5" %in% included_criteria) {
    output$stopp_c5 <- stopp_c5(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_c7" %in% included_criteria) {
    output$stopp_c7 <- stopp_c7(
      df, drug_string = drug_string
    )
  }

  if ("stopp_c8" %in% included_criteria) {
    output$stopp_c8 <- stopp_c8(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_c9" %in% included_criteria) {
    output$stopp_c9 <- stopp_c9(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_c10" %in% included_criteria) {
    output$stopp_c10 <- stopp_c10(
      df, drug_string = drug_string
    )
  }

  if ("stopp_c11" %in% included_criteria) {
    output$stopp_c11 <- stopp_c11(
      df, drug_string = drug_string
    )
  }

  if ("stopp_d1" %in% included_criteria) {
    output$stopp_d1 <- stopp_d1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_d3" %in% included_criteria) {
    output$stopp_d3 <- stopp_d3(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_d4" %in% included_criteria) {
    output$stopp_d4 <- stopp_d4(
      df, drug_string = drug_string,
      Na_column = "Lab Values: Na"
    )
  }

  if ("stopp_d5" %in% included_criteria) {
    output$stopp_d5 <- stopp_d5(
      df, drug_string = drug_string
    )
  }

  if ("stopp_d6" %in% included_criteria) {
    output$stopp_d6 <- stopp_d6(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_d7" %in% included_criteria) {
    output$stopp_d7 <- stopp_d7(
      df, drug_string = drug_string
    )
  }

  if ("stopp_d8" %in% included_criteria) {
    output$stopp_d8 <- stopp_d8(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_d9" %in% included_criteria) {
    output$stopp_d9 <- stopp_d9(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_d10" %in% included_criteria) {
    output$stopp_d10 <- stopp_d10(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_d11" %in% included_criteria) {
    output$stopp_d11 <- stopp_d11(
      df, comorb_string = comorb_string, drug_string = drug_string,
      HR_column = "Lab Values: Heart Rate"
    )
  }

  if ("stopp_d12" %in% included_criteria) {
    output$stopp_d12 <- stopp_d12(
      df, drug_string = drug_string
    )
  }

  if ("stopp_d13" %in% included_criteria) {
    output$stopp_d13 <- stopp_d13(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_d14" %in% included_criteria) {
    output$stopp_d14 <- stopp_d14(
      df, drug_string = drug_string
    )
  }

  if ("stopp_e1" %in% included_criteria) {
    output$stopp_e1 <- stopp_e1(
      df, drug_string = drug_string,
      GFR_column = "Lab Values: eGFR",
      digoxin_column = "Digoxin Dose >125mcg"
    )
  }

  if ("stopp_e2" %in% included_criteria) {
    output$stopp_e2 <- stopp_e2(
      df, drug_string = drug_string,
      GFR_column = "Lab Values: eGFR"
    )
  }

  if ("stopp_e3" %in% included_criteria) {
    output$stopp_e3 <- stopp_e3(
      df, drug_string = drug_string,
      GFR_column = "Lab Values: eGFR"
    )
  }

  if ("stopp_e4" %in% included_criteria) {
    output$stopp_e4 <- stopp_e4(
      df, drug_string = drug_string,
      GFR_column = "Lab Values: eGFR"
    )
  }

  if ("stopp_e5" %in% included_criteria) {
    output$stopp_e5 <- stopp_e5(
      df, drug_string = drug_string,
      GFR_column = "Lab Values: eGFR"
    )
  }

  if ("stopp_e6" %in% included_criteria) {
    output$stopp_e6 <- stopp_e6(
      df, drug_string = drug_string,
      GFR_column = "Lab Values: eGFR"
    )
  }

  if ("stopp_f1" %in% included_criteria) {
    output$stopp_f1 <- stopp_f1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_f2" %in% included_criteria) {
    output$stopp_f2 <- stopp_f2(
      df, drug_string = drug_string,
      ppi_column = "Full Dose PPI"
    )
  }

  if ("stopp_f3" %in% included_criteria) {
    output$stopp_f3 <- stopp_f3(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_f4" %in% included_criteria) {
    output$stopp_f4 <- stopp_f4(
      df, drug_string = drug_string,
      Fe_column = "Elemental Iron >600mg"
    )
  }

  if ("stopp_g1" %in% included_criteria) {
    output$stopp_g1 <- stopp_g1(
      df, drug_string = drug_string
    )
  }

  if ("stopp_g2" %in% included_criteria) {
    output$stopp_g2 <- stopp_g2(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_g4" %in% included_criteria) {
    output$stopp_g4 <- stopp_g4(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_h1" %in% included_criteria) {
    output$stopp_h1 <- stopp_h1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_h2" %in% included_criteria) {
    output$stopp_h2 <- stopp_h2(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_h4" %in% included_criteria) {
    output$stopp_h4 <- stopp_h4(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_h5" %in% included_criteria) {
    output$stopp_h5 <- stopp_h5(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_h6" %in% included_criteria) {
    output$stopp_h6 <- stopp_h6(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_h7" %in% included_criteria) {
    output$stopp_h7 <- stopp_h7(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_h8" %in% included_criteria) {
    output$stopp_h8 <- stopp_h8(
      df, drug_string = drug_string
    )
  }

  if ("stopp_h9" %in% included_criteria) {
    output$stopp_h9 <- stopp_h9(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_i1" %in% included_criteria) {
    output$stopp_i1 <- stopp_i1(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_i2" %in% included_criteria) {
    output$stopp_i2 <- stopp_i2(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_j1" %in% included_criteria) {
    output$stopp_j1 <- stopp_j1(
      df, drug_string = drug_string
    )
  }

  if ("stopp_j2" %in% included_criteria) {
    output$stopp_j2 <- stopp_j2(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_j4" %in% included_criteria) {
    output$stopp_j4 <- stopp_j4(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_j5" %in% included_criteria) {
    output$stopp_j5 <- stopp_j5(
      df, comorb_string = comorb_string, drug_string = drug_string,
      gender_column = gender_column
    )
  }

  if ("stopp_j6" %in% included_criteria) {
    output$stopp_j6 <- stopp_j6(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_k1" %in% included_criteria) {
    output$stopp_k1 <- stopp_k1(
      df, drug_string = drug_string
    )
  }

  if ("stopp_k2" %in% included_criteria) {
    output$stopp_k2 <- stopp_k2(
      df, drug_string = drug_string
    )
  }

  if ("stopp_k3" %in% included_criteria) {
    output$stopp_k3 <- stopp_k3(
      df, comorb_string = comorb_string, drug_string = drug_string
    )
  }

  if ("stopp_k4" %in% included_criteria) {
    output$stopp_k4 <- stopp_k4(
      df, drug_string = drug_string
    )
  }

  if ("stopp_l2" %in% included_criteria) {
    output$stopp_l2 <- stopp_l2(
      df, drug_string = drug_string
    )
  }

  if ("stopp_l3" %in% included_criteria) {
    output$stopp_l3 <- stopp_l3(
      df, drug_string = drug_string
    )
  }

  if ("stopp_m1" %in% included_criteria) {
    output$stopp_m1 <- stopp_m1(
      df, drug_string = drug_string
    )
  }

  return(output)
}
