#' Implement all STOPP/START criteria or a subset of them.
#'
#' @param df Patient info
#' @param comorb_string .
#' @param drug_string .
#' @param diastolic_column .
#' @param systolic_column .
#' @param age_column .
#' @param gender_column .
#' @param gfr_column .
#' @param hr_column .
#' @param potassium_column .
#' @param sodium_column .
#' @param calcium_column .
#' @param asa_column .
#' @param digoxin_column .
#' @param iron_column .
#' @param ppi_column .
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
  gender_column = "Gender",
  gfr_column = "Lab Values: eGFR",
  hr_column = "Lab Values: Heart Rate",
  potassium_column = "Lab Values: K",
  sodium_column = "Lab Values: Na",
  calcium_column = "Lab Values: Corrected Ca",
  asa_column = "Aspirin dose >150mg",
  digoxin_column = "Digoxin Dose >125mcg",
  iron_column = "Elemental Iron >600mg",
  ppi_column = "Full Dose PPI",
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

  all_criteria <- c(
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

  included_criteria <- setdiff(included_criteria, excluded_criteria)

  inclusion_vector <- all_criteria %in% included_criteria
  names(inclusion_vector) <- all_criteria

  output <- as.data.frame(matrix(nrow = nrow(df), ncol = 0))


  output$start_a1 <- try(start_a1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_a1"])

  output$start_a3 <- try(start_a3(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_a3"])

  output$start_a4 <- try(start_a4(
    df, comorb_string = comorb_string, drug_string = drug_string,
    diastolic_column = diastolic_column,
    systolic_column = systolic_column
  ), silent = !inclusion_vector["start_a4"])

  output$start_a5 <- try(start_a5(
    df, comorb_string = comorb_string, drug_string = drug_string,
    age_column = age_column
  ), silent = !inclusion_vector["start_a5"])

  output$start_a6 <- try(start_a6(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_a6"])

  output$start_a7 <- try(start_a7(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_a7"])

  output$start_a8 <- try(start_a8(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_a8"])

  output$start_b1 <- try(start_b1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_b1"])

  output$start_c1 <- try(start_c1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_c1"])

  output$start_c2 <- try(start_c2(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_c2"])

  output$start_c3a <- try(start_c3a(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_c3a"])

  output$start_c3b <- try(start_c3b(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_c3b"])

  output$start_c4 <- try(start_c4(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_c4"])

  output$start_c5 <- try(start_c5(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_c5"])

  output$start_c6 <- try(start_c6(
    df, comorb_string = comorb_string, drug_string = drug_string,
    gfr_column = gfr_column
  ), silent = !inclusion_vector["start_c6"])

  output$start_d1 <- try(start_d1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_d1"])

  output$start_d2 <- try(start_d2(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_d2"])

  output$start_e1 <- try(start_e1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_e1"])

  output$start_e2 <- try(start_e2(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["start_e2"])

  output$start_e3 <- try(start_e3(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_e3"])

  output$start_e4 <- try(start_e4(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_e4"])

  output$start_e5 <- try(start_e5(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_e5"])

  output$start_e6 <- try(start_e6(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["start_e6"])

  output$start_e7 <- try(start_e7(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["start_e7"])

  output$start_g1 <- try(start_g1(
    df, comorb_string = comorb_string, drug_string = drug_string,
    gender_column = gender_column
  ), silent = !inclusion_vector["start_g1"])

  output$start_g2 <- try(start_g2(
    df, comorb_string = comorb_string, drug_string = drug_string,
    gender_column = gender_column
  ), silent = !inclusion_vector["start_g2"])

  output$start_g3 <- try(start_g3(
    df, comorb_string = comorb_string, drug_string = drug_string,
    gender_column = gender_column
  ), silent = !inclusion_vector["start_g3"])

  output$start_h2 <- try(start_h2(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["start_h2"])

  output$stopp_b1 <- try(stopp_b1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b1"])

  output$stopp_b2 <- try(stopp_b2(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b2"])

  output$stopp_b3 <- try(stopp_b3(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b3"])

  output$stopp_b4 <- try(stopp_b4(
    df, comorb_string = comorb_string, drug_string = drug_string,
    hr_column = hr_column
  ), silent = !inclusion_vector["stopp_b4"])

  output$stopp_b5 <- try(stopp_b5(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b5"])

  output$stopp_b6 <- try(stopp_b6(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b6"])

  output$stopp_b7 <- try(stopp_b7(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b7"])

  output$stopp_b8 <- try(stopp_b8(
    df, comorb_string = comorb_string, drug_string = drug_string,
    potassium_column = potassium_column,
    sodium_column = sodium_column,
    calcium_column = calcium_column
  ), silent = !inclusion_vector["stopp_b8"])

  output$stopp_b9 <- try(stopp_b9(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b9"])

  output$stopp_b10 <- try(stopp_b10(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b10"])

  output$stopp_b11 <- try(stopp_b11(
    df, comorb_string = comorb_string, drug_string = drug_string,
    potassium_column = potassium_column
  ), silent = !inclusion_vector["stopp_b11"])

  output$stopp_b13a <- try(stopp_b13a(
    df, comorb_string = comorb_string, drug_string = drug_string,
    systolic_column = systolic_column
  ), silent = !inclusion_vector["stopp_b13a"])

  output$stopp_b13b <- try(stopp_b13b(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_b13b"])

  output$stopp_c1 <- try(stopp_c1(
    df, drug_string = drug_string,
    asa_column = asa_column
  ), silent = !inclusion_vector["stopp_c1"])

  output$stopp_c2 <- try(stopp_c2(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_c2"])

  output$stopp_c5 <- try(stopp_c5(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_c5"])

  output$stopp_c7 <- try(stopp_c7(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_c7"])

  output$stopp_c8 <- try(stopp_c8(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_c8"])

  output$stopp_c9 <- try(stopp_c9(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_c9"])

  output$stopp_c10 <- try(stopp_c10(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_c10"])

  output$stopp_c11 <- try(stopp_c11(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_c11"])

  output$stopp_d1 <- try(stopp_d1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d1"])

  output$stopp_d3 <- try(stopp_d3(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d3"])

  output$stopp_d4 <- try(stopp_d4(
    df, drug_string = drug_string,
    sodium_column = sodium_column
  ), silent = !inclusion_vector["stopp_d4"])

  output$stopp_d5 <- try(stopp_d5(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d5"])

  output$stopp_d6 <- try(stopp_d6(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d6"])

  output$stopp_d7 <- try(stopp_d7(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d7"])

  output$stopp_d8 <- try(stopp_d8(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d8"])

  output$stopp_d9 <- try(stopp_d9(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d9"])

  output$stopp_d10 <- try(stopp_d10(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d10"])

  output$stopp_d11 <- try(stopp_d11(
    df, comorb_string = comorb_string, drug_string = drug_string,
    hr_column = hr_column
  ), silent = !inclusion_vector["stopp_d11"])

  output$stopp_d12 <- try(stopp_d12(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d12"])

  output$stopp_d13 <- try(stopp_d13(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d13"])

  output$stopp_d14 <- try(stopp_d14(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_d14"])

  output$stopp_e1 <- try(stopp_e1(
    df, drug_string = drug_string,
    gfr_column = gfr_column,
    digoxin_column = digoxin_column
  ), silent = !inclusion_vector["stopp_e1"])

  output$stopp_e2 <- try(stopp_e2(
    df, drug_string = drug_string,
    gfr_column = gfr_column
  ), silent = !inclusion_vector["stopp_e2"])

  output$stopp_e3 <- try(stopp_e3(
    df, drug_string = drug_string,
    gfr_column = gfr_column
  ), silent = !inclusion_vector["stopp_e3"])

  output$stopp_e4 <- try(stopp_e4(
    df, drug_string = drug_string,
    gfr_column = gfr_column
  ), silent = !inclusion_vector["stopp_e4"])

  output$stopp_e5 <- try(stopp_e5(
    df, drug_string = drug_string,
    gfr_column = gfr_column
  ), silent = !inclusion_vector["stopp_e5"])

  output$stopp_e6 <- try(stopp_e6(
    df, drug_string = drug_string,
    gfr_column = gfr_column
  ), silent = !inclusion_vector["stopp_e6"])

  output$stopp_f1 <- try(stopp_f1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_f1"])

  output$stopp_f2 <- try(stopp_f2(
    df, drug_string = drug_string,
    ppi_column = ppi_column
  ), silent = !inclusion_vector["stopp_f2"])

  output$stopp_f3 <- try(stopp_f3(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_f3"])

  output$stopp_f4 <- try(stopp_f4(
    df, drug_string = drug_string,
    iron_column = iron_column
  ), silent = !inclusion_vector["stopp_f4"])

  output$stopp_g1 <- try(stopp_g1(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_g1"])

  output$stopp_g2 <- try(stopp_g2(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_g2"])

  output$stopp_g4 <- try(stopp_g4(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_g4"])

  output$stopp_h1 <- try(stopp_h1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_h1"])

  output$stopp_h2 <- try(stopp_h2(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_h2"])

  output$stopp_h4 <- try(stopp_h4(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_h4"])

  output$stopp_h5 <- try(stopp_h5(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_h5"])

  output$stopp_h6 <- try(stopp_h6(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_h6"])

  output$stopp_h7 <- try(stopp_h7(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_h7"])

  output$stopp_h8 <- try(stopp_h8(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_h8"])

  output$stopp_h9 <- try(stopp_h9(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_h9"])

  output$stopp_i1 <- try(stopp_i1(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["i1"])

  output$stopp_i2 <- try(stopp_i2(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_i2"])

  output$stopp_j1 <- try(stopp_j1(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_j1"])

  output$stopp_j2 <- try(stopp_j2(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_j2"])

  output$stopp_j4 <- try(stopp_j4(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_j4"])

  output$stopp_j5 <- try(stopp_j5(
    df, comorb_string = comorb_string, drug_string = drug_string,
    gender_column = gender_column
  ), silent = !inclusion_vector["stopp_j5"])

  output$stopp_j6 <- try(stopp_j6(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_j6"])

  output$stopp_k1 <- try(stopp_k1(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_k1"])

  output$stopp_k2 <- try(stopp_k2(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_k2"])

  output$stopp_k3 <- try(stopp_k3(
    df, comorb_string = comorb_string, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_k3"])

  output$stopp_k4 <- try(stopp_k4(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_k4"])

  output$stopp_l2 <- try(stopp_l2(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_l2"])

  output$stopp_l3 <- try(stopp_l3(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_l3"])

  output$stopp_m1 <- try(stopp_m1(
    df, drug_string = drug_string
  ), silent = !inclusion_vector["stopp_m1"])

  output <- output[, inclusion_vector]

  return(output)
}
