% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/start_de.R
\name{start_d2}
\alias{start_d2}
\title{Implement START-D2 criterion.}
\usage{
start_d2(df, comorb_string = "Comorbidity_", drug_string = "Drug_")
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
Determine which patients triggered the conditions defining START-D1.
}
\details{
START-D2 requires all of the following conditions to be satisfied:
\itemize{
\item Any of the following comorbidities:

K57
\item Any of the following comorbidities:

K59.0
\item None of the following drugs:

A06AC
}
}
