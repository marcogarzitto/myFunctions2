#' my_round
#'
#' Function to represent a number rounded to the required decimal places.
#' From a numeric value to a string.
#'
#' @param value Numeric value. Default: None.
#' @param decimals Number of decimals. Numeric value. Default: 3.
#' @return A string for the number with required decimals.
#' @export
my_round <- function (value, decimals = 3)
{
 return(format(round(value, decimals), nsmall = decimals))
}

#
