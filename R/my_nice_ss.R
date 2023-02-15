#' my_nice_ss
#'
#' Function to represent SS-values (mean: 100, SD: 15).
#'
#' @param value Numeric value. Default: None.
#' @param decimals Number of decimals. Numeric value. Default: 1.
#' @param text Text to name the value. String. Default: 'SS'.
#' @param min_value Minimum permissible/possible value. If 'value' is lower than it, will be rounded to this. Numeric value. Default: 25.
#' @param max_value Maximum permissible/possible value. If 'value' is higher than it, will be rounded to this. Numeric value. Default: 175.
#' @param with_equal_sign Equal, or unequal, sign (always present if there is some text in 'text'). Boolean. Default: FALSE.
#' @param with_sign Mark of +/- in front of the number. Boolean. Default: FALSE.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @return A string for the SS-value with required decimals, possibly with several ornaments.
#' @export
my_nice_ss <- function (value, decimals = 1, text = 'SS', min_value = 25, max_value = 175, with_equal_sign = FALSE, with_sign = FALSE, void_string = '-')
{
 if (text != '')
 {
  with_equal_sign <- TRUE
 }
 #
 result <- my_nice_z(value = value, decimals = decimals, text = text, standard_mean = 100, standard_sd = 15, min_z = (min_value - 100) / 15, max_z = (max_value - 100) / 15, with_equal_sign = with_equal_sign, with_sign = with_sign, void_string = void_string)
 #
 return(result)
}

#
