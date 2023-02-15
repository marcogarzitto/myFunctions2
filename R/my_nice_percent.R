#' my_nice_percent
#'
#' Function to represent percentages.
#'
#' @param value Numeric value. Default: None.
#' @param decimals Number of decimals. Numeric value. Default: 1.
#' @param text Text to name the value. String. Default: ''.
#' @param percent_sign Uses the percentage sign. Boolean. Default: TRUE.
#' @param per_level Uses the percentage sign for 1/100, 1/1000, or 1/10000. String. Default: '100'.
#' @param min_value Minimum permissible/possible value. If 'value' is lower than it, will be rounded to this. Numeric value. Default: -500.
#' @param max_value Maximum permissible/possible value. If 'value' is higher than it, will be rounded to this. Numeric value. Default: 500.
#' @param with_equal_sign Equal, or unequal, sign (always present if there is some text in 'text'). Boolean. Default: FALSE.
#' @param with_sign Mark of +/- in front of the number. Boolean. Default: TRUE.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @return A string for the percentage with required decimals, possibly with several ornaments.
#' @export
my_nice_percent <- function (value, decimals = 1, text = '', percent_sign = TRUE, per_level = '100', min_value = -500, max_value = 500, with_equal_sign = FALSE, with_sign = TRUE, void_string = '-')
{
 if (text != '')
 {
  with_equal_sign <- TRUE
 }
 if (percent_sign)
 {
  if (per_level == '100') { percent_sign <- '\u0025' } # per cent
  if (per_level == '1000') { percent_sign <- '\u2030' } # per thousand
  if (per_level == '10000') { percent_sign <- '\u2031' } # per ten thousand
 } else
 {
  percent_sign <- ''
 }
 #
 result <- my_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = with_sign, min_value = min_value, max_value = max_value, void_string = void_string)
 result <- paste(result, percent_sign, sep = '')
 return(result)
}

#
