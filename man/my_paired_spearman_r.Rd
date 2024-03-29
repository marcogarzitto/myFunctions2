% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_paired_spearman_r.R
\name{my_paired_spearman_r}
\alias{my_paired_spearman_r}
\title{my_paired_spearman_r}
\usage{
my_paired_spearman_r(
  y,
  time,
  void_string = "-",
  alpha_value = 0.05,
  multiple_alphas = c(0.05, 0.01, 0.001),
  direction = "Stable"
)
}
\arguments{
\item{y}{Numeric vector. Dependent variable (using long format for data-frame). Default: None.}

\item{time}{Numeric vector. Independent variable (using long format for data-frame). Default: None.}

\item{void_string}{String to be used if the number cannot be represented correctly. String. Default: '-'.}

\item{alpha_value}{Statistical significance. Numeric value. Default: 0.050.}

\item{multiple_alphas}{Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).}

\item{direction}{Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.}
}
\value{
A list with results: 'test' (string, with results of the Spearman's correlation), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, reporting the direction of the correlation), 'es' (string, effect-size for statistically significant results), 'groups' (string, actually a void string), 'groups_pairs' (string, actually a void string), 'groups_pairs_p' (string, actually a void string).
}
\description{
Function to do a withween-subjects Spearman's correlation.
}
