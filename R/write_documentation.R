write_title <- function(criterion_name) {
  paste0(
    "Implement ", criterion_name, " criterion."
  )
}

write_description <- function(criterion_name) {
  paste0(
    "Determine which patients triggered the conditions defining ",
    criterion_name
  )
}

write_details_relevant <- function(criterion_name) {
  paste0(
    "For the ", criterion_name, " criterion to be relevant for a patient,",
    " all of the following statements must be true."
  )
}

write_details_triggered <- function(criterion_name) {
  paste0(
    "For the ", criterion_name, " criterion to be triggered for a patient,",
    " all of the following statements must also be true."
  )
}

write_return_start <- function(criterion_name) {
  paste0(
    "A character vector with one entry per patient / row in `df`. ",
    "Each entry takes one of the following three values:\n",
    "* \"Not Relevant\" if the conditions are not satisfied.\n",
    "* \"Appropriate\" if the conditions are satisfied but the correct ",
    "drugs have been prescribed.\n",
    "* \"", criterion_name,
    "\" if the conditions are satisfied and the correct ",
    "drugs have not been prescribed. ",
    "That is, if ", criterion_name, " has been triggered."
  )
}

write_return_stopp <- function(criterion_name) {
  paste0(
    "A character vector with one entry per patient / row in `df`. ",
    "Each entry takes one of the following three values:\n",
    "* \"Not Relevant\" if the conditions are not satisfied.\n",
    "* \"Appropriate\" if the conditions are satisfied but the incorrect ",
    "drugs have not been prescribed.\n",
    "* \"", criterion_name,
    "\" if the conditions are satisfied and the incorrect ",
    "drugs have been prescribed. ",
    "That is, if ", criterion_name, " has been triggered."
  )
}
