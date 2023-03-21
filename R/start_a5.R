#' Function to implement START-A5 rule.
#'
#' @param df Dataframe of patient information.
#'
#' @return List with two elements:
#' `bool` is a logical vector indicating whether each patient has triggered START-A5,
#' `text` is a character vector of the same length whose entries are either blank
#' or contain the instruction message for START-A5.
#' @export
#'
start_a5 <- function(df) {
  comorbs_cols <- paste0("Comorbidity_", 1:20)
  drugs_cols <- paste0("Drug_", 1:30)

  extras_cond1 <- df$Age < 85

  comorbs_set1 <- c("I20", "I21", "I22", "I24", "I25")
  comorbs_set1_paste <- paste(comorbs_set1, collapse = "|")

  comorbs_check1 <- list()
  for (i in 1:20) {
    comorbs_check1[[i]] <- grepl(x = df[[comorbs_cols[i]]], pattern = comorbs_set1_paste)
  }
  comorbs_cond1 <- Reduce(x = comorbs_check1, f = "|")

  drugs_check1 <- list()
  for (i in 1:30) {
    drugs_check1[[i]] <- !grepl(x = df[[drugs_cols[i]]], pattern = "C10AA")
  }
  drugs_cond1 <- Reduce(x = drugs_check1, f = "&")

  bool <- Reduce(x = list(extras_cond1, comorbs_cond1, drugs_cond1), f = "&")
  text <- ifelse(bool, "START A5", "")

  return(list(bool = bool, text = text))
}
