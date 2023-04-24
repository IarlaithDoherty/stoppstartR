#' Title
#'
#' @param df Patient info
#' @param comorb_string .
#' @param drug_string .
#' @param diastolic_column .
#' @param systolic_column .
#' @param age_column .
#'
#' @return Data frame of character vectors.
#' @export
#'
#' @examples all_stoppstart(session13)
all_stoppstart <- function(df, comorb_string = "Comorbidity_",
                           drug_string = "Drug_",
                           diastolic_column = "Diastolic_BP",
                           systolic_column = "Systolic_BP",
                           age_column = "Age") {
  starts <- data.frame(
    a1 = start_a1(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    a3 = start_a3(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    a4 = start_a4(df,
                  comorb_string = comorb_string, drug_string = drug_string,
                  diastolic_column = diastolic_column,
                  systolic_column = systolic_column),
    a5 = start_a5(df,
                  comorb_string = comorb_string, drug_string = drug_string,
                  age_column = age_column),
    a6 = start_a6(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    a7 = start_a7(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    a8 = start_a8(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    b1 = start_b1(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    c1 = start_c1(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    c2 = start_c2(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    c4 = start_c4(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    c5 = start_c5(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    c6 = start_c6(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    d1 = start_d1(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    d2 = start_d2(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    e1 = start_e1(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    e2 = start_e2(df,
                  drug_string = drug_string),
    e4 = start_e4(df,
                  comorb_string = comorb_string, drug_string = drug_string),
    e6 = start_e6(df,
                  comorb_string = comorb_string, drug_string = drug_string)
  )

  return(starts)
}
