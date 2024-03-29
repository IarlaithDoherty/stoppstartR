% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/start_a.R
\name{start_a4}
\alias{start_a4}
\title{Implement START-A4 criterion}
\usage{
start_a4(
  df,
  comorb_string = "Comorbidity_",
  drug_string = "Drug_",
  systolic_column = "Systolic",
  diastolic_column = "Diastolic"
)
}
\arguments{
\item{df}{data.frame: Patient information.}

\item{comorb_string}{Character string: Common text contained in the name of
each comorbidity column which uniquely identifies them.}

\item{drug_string}{Character string: Common text contained in the name of
each drug column which uniquely identifies them.}

\item{systolic_column}{Character string: The name of the systolic column.}

\item{diastolic_column}{Character string: The name of the diastolic column.}
}
\value{
A character vector with one entry per patient / row in \code{df}. Each entry takes one of the following three values:
\itemize{
\item "Not Relevant" if the conditions are not satisfied.
\item "Appropriate" if the conditions are satisfied but the correct medications have been prescribed.
\item "START-A4" if the conditions are satisfied and the correct medications have not been prescribed. That is, if START-A4 has been triggered.
}
}
\description{
Determine which patients triggered the conditions defining START-A4.
}
\details{
For the START-A4 criterion to be relevant for a patient, any of the following statements must be true.
\itemize{
\item The patient has a Systolic Blood Pressure of greater than 160.
\item The patient has a Diastolic Blood Pressure of greater than 90.
\item The patient has a Systolic Blood Pressue of greater than 140 and they have
been diagnosed with any of the following comorbidities:
\itemize{
\item E10, E11, E12, E13, E14.
}
}

For the START-A4 criterion to be triggered for a patient, all of the following statements must also be true.
\itemize{
\item The patient has not been prescribed any of the following medications:
\itemize{
\item C07, C08, C09, C03A, C03EA.
}
}
}
