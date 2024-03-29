% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_paired_chisquare.R
\name{my_paired_chisquare}
\alias{my_paired_chisquare}
\title{my_paired_chisquare}
\usage{
my_paired_chisquare(
  a,
  time,
  observations,
  void_string = "-",
  alpha_value = 0.05,
  multiple_alphas = c(0.05, 0.01, 0.001),
  wise = TRUE,
  direction = "Stable"
)
}
\arguments{
\item{a}{Factor vector. Dependent/Group variable (using long format for data-frame). Default: None.}

\item{time}{Factor vector. Independent/Time variable (using long format for data-frame). Default: None.}

\item{observations}{Factor vector, with the observation identifier (using long format for data-frame). Default: None.}

\item{void_string}{String to be used if the number cannot be represented correctly. String. Default: '-'.}

\item{alpha_value}{Statistical significance. Numeric value. Default: 0.050.}

\item{multiple_alphas}{Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).}

\item{wise}{Boolean, if true, the most appropriate test is used according to the data. Default: TRUE}

\item{direction}{Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.}
}
\value{
A list with results: 'test' (string, with results of the Chi-squared test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the group variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'groups' (string, frequencies of levels of time by levels of a), 'groups_pairs' (string, actually a void string), 'groups_pairs_p' (string, actually a void string).
}
\description{
Function to do a Chi-squared test using McNemar test as alternative or completion.
}
