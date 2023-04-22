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
                  age_column = age_column)
  )

  return(starts)
}
