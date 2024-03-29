% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TranslateR.R
\name{Tslate}
\alias{Tslate}
\title{Translate any DNA or RNA sequence to IUPAC amino acid code}
\usage{
Tslate(sequence)
}
\arguments{
\item{sequence}{a text string containg a DNA or RNA sequence}
}
\value{
A text string that represents the translated amino acid sequence
}
\description{
Translate any DNA or RNA sequence to IUPAC amino acid code
}
\examples{
sequence <- "ATGCTGTAG"
Tslate(sequence)
}
