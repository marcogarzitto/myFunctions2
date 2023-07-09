#' my_nice_z
#'
#' Function for representing z-values (but also applicable to other standardisations).
#' E.g., To represent T-values, use: 'standard_mean = 50', 'standard_sd = 10'.
#' 
#' @param value Numeric value. Default: None.
#' @param decimals Number of decimals. Numeric value. Default: 3.
#' @param text Text to name the value. String. Default: ''.
#' @param standard_mean Mean of the standard score. Numeric value. Default: 0.
#' @param standard_sd SD of the standard score. Numeric value. Default: 1.
#' @param min_z Minimum (in SD from the mean) permissible/possible value. If 'value' is lower than it, will be rounded to this. Numeric value. Default: -5.
#' @param max_z Maximum (in SD from the mean) permissible/possible value. If 'value' is higher than it, will be rounded to this. Numeric value. Default: 5.
#' @param with_equal_sign Equal, or unequal, sign (always present if there is some text in 'text'). Boolean. Default: FALSE.
#' @param with_sign Mark of +/- in front of the number. Boolean. Default: TRUE.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @return A string for the standard value with required decimals, possibly with several ornaments.
#' @export
my_nice_z <- function (value, decimals = 3, text = '', standard_mean = 0, standard_sd = 1, min_z = -5, max_z = 5, with_equal_sign = FALSE, with_sign = TRUE, void_string = '-')
{
 if (text != '')
 {
  with_equal_sign <- TRUE
 }
 #
 if (!is.na(min_z))
 {
  min_value <- standard_mean - (abs(min_z) * standard_sd)
 } else
 {
  min_value <- NA
 }
 if (!is.na(max_z))
 {
  max_value <- standard_mean + (abs(max_z) * standard_sd)
  } else
  {
   max_value <- NA
  }
 #
 result <- my_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#
