#' my_nice
#'
#' Function to represent a number rounded to the required decimal places.
#' From a numeric value to a string.
#'
#' @param value Numeric value. Default: None.
#' @param decimals Number of decimals. Numeric value. Default: 3.
#' @param text Text to name the value. String. Default: ''.
#' @param with_equal_sign Equal, or unequal, sign (always present if there is some text in 'text'). Boolean. Default: FALSE.
#' @param with_sign Mark of +/- in front of the number. Boolean. Default: TRUE.
#' @param min_value Minimum permissible/possible value. If 'value' is lower than it, will be rounded to this. Numeric value. Default: -Inf.
#' @param max_value Maximum permissible/possible value. If 'value' is higher than it, will be rounded to this. Numeric value. Default: Inf.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @return A string for the number with required decimals, possibly with the name of the value and/or with several ornaments.
#' @export
my_nice <- function (value, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -Inf, max_value = Inf, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 if (with_equal_sign) { equal_sign <- '=' } else { equal_sign <- '' }
 if (is.na(value) | !is.numeric(value))
 {
  return(paste(text, equal_sign, void_string, sep = ''))
 }
 if (is.infinite(value))
 {
  return(paste(text, equal_sign, void_string, sep = ''))
 }
 #
 if (with_sign)
 {
  if (value == 0)
  {
   plus_sign <- ''
  } else if (value > 0)
  {
   plus_sign <- '+'
  } else
  {
   plus_sign <- ''
  }
 } else
 {
  plus_sign <- ''
 }
 #
 if (max_value <= min_value)
 {
  max_value <- NA
  min_value <- NA
 }
 #
 if (!is.infinite(max_value) & !is.na(max_value) & (value > max_value))
 {
  value <- max_value
  equal_sign <- '>'
 }
 if (!is.infinite(min_value) & !is.na(min_value) & (value < min_value))
 {
  value <- min_value
  equal_sign <- '<'
 }
 #
 result <- my_round(value = value, decimals = decimals)
 result <- paste(text, equal_sign, plus_sign, result, sep = '')
 return(result)
}

#
