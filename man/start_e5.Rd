% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/start_de.R
\name{start_e5}
\alias{start_e5}
\title{Implement START-E5 criterion.}
\usage{
start_e5(df, comorb_string = "Comorbidity_", drug_string = "Drug_")
}
\arguments{
\item{df}{data.frame: Patient information.}

\item{comorb_string}{Character string: Common text contained in the name of
each comorbidity column which uniquely identifies them.}

\item{drug_string}{Character string: Common text contained in the name of
each drug column which uniquely identifies them.}
}
\value{
A character vector with one entry per patient / row in \code{df}. Each entry takes one of the following three values:
\itemize{
\item "Not Relevant" if the conditions are not satisfied.
\item "Appropriate" if the conditions are satisfied but the correct medications have been prescribed.
\item "START-A1" if the conditions are satisfied and the correct medications have not been prescribed. That is, if START-A1 has been triggered.
}
}
\description{
Determine which patients triggered the conditions defining START-E5.
}
\details{
START-E5 requires all of the following conditions to be satisfied:
\itemize{
\item Any of the following comorbidities:

R29.6, R26.3, W01, W05, W06, W07, W08, W10, W18, W19
\item None of the following drugs:

A11CB, A12AX, M05BB03, M05BB04, M05BB05, M05BB06, M05BB07, M05BB08,	A11CC
}
}
