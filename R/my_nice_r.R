#' my_nice_r
#'
#' Function to represent correlations.
#'
#' @param value Numeric value. Default: None.
#' @param decimals Number of decimals. Numeric value. Default: 3.
#' @param with_r Shows 'r'/'rS' (and equal, or unequal, sign) before the value. Boolean. Default: TRUE.
#' @param spearman Shows 'rS' instead of 'r' if there is 'with_r'. Boolean. Default: FALSE.
#' @param with_equal_sign Equal, or unequal, sign (always present if there is 'with_r'). Boolean. Default: FALSE.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @return A string for the correlation with required decimals, possibly with several ornaments.
#' @export
my_nice_r <- function (value, decimals = 3, with_r = TRUE, spearman = FALSE, with_equal_sign = FALSE, void_string = '-')
{
 if (with_r)
 {
  if (spearman)
  {
   text <- 'r\u209B'
  } else
  {
   text <- 'r'
  }
  with_equal_sign = TRUE
 } else
 {
  text <- ''
 }
 #
 min_value <- -1
 max_value <- 1
 #
 if (abs(value) > 1)
 {
  value <- NA
 }
 #
 result <- my_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#
