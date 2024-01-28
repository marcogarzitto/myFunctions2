#' my_nice_p
#'
#' Function to represent p-values.
#' Includes asterisks for statistical significance.
#'
#' @param value Numeric value. Default: None.
#' @param decimals Number of decimals. Numeric value. Default: 3.
#' @param with_p Shows 'p' (and equal, or unequal, sign) before the p-value. Boolean. Default: TRUE.
#' @param with_equal_sign Equal, or unequal, sign (always present if there is 'p'). Boolean. Default: FALSE.
#' @param with_stars Shows an asterisk if statistically significant. Boolean. Default: TRUE.
#' @param multiple_stars Shows one-to-three asterisks if statistically significant (depending on level, see 'multiple_alphas'). Boolean. Default: TRUE.
#' @param alpha Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param give_only_stars Returns only the asterisks (without the p-value). Boolean. Default = FALSE.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @return A string for the p-value with required decimals, possibly with several ornaments.
#' @export
my_nice_p <- function (value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), give_only_stars = FALSE, void_string = '-')
{
 if (with_p)
 {
  text <- 'p'
  with_equal_sign = TRUE
 } else
 {
  text <- ''
 }
 #
 if (is.na(value) | !is.numeric(value) | is.infinite(value)) { return(my_nice(text = text, value = value)) }
 #
 if (with_stars & !multiple_stars & (value < alpha))
 {
  stars <- '*'
 }
 else
 {
  stars <- ''
 }
 if (multiple_stars)
 {
  stars <- ''
  if (value < multiple_alphas[1])
  {
   stars <- paste(stars, '*', sep = '')
  }
  if (value < multiple_alphas[2])
  {
   stars <- paste(stars, '*', sep = '')
  }
  if (value < multiple_alphas[3])
  {
   stars <- paste(stars, '*', sep = '')
  }
 }
 #
 min_value <- 10**(-1 * (decimals))
 max_value <- 1 - min_value
 #
 result <- my_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = FALSE, min_value = min_value, max_value = max_value, void_string = void_string)
 if (give_only_stars)
 {
  result <- stars
 } else
 {
  result <- paste(result, stars, sep = '')
 }
 #
 return(result)
}

#
