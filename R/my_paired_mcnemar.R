#' my_paired_mcnemar
#'
#' Function to do a McNemar's paired test.
#'
#' @param a Factor vector (with 2 levels). Default: None.
#' @param b Factor vector (with 2 levels). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the McNemar's test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the time variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'times' (string, frequencies of levels of b by levels of a), 'times_pairs' (string, actually a void string), 'times_pairs_p' (string, actually a void string).
#' @export
my_paired_mcnemar <- function (a, b, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), direction = 'Stable')
{
 result <- void_string
 p_value <- 1.0
 significance <- void_string
 comparison <- void_string
 effect_size <- void_string
 times_description <- void_string
 times_pairs <- void_string
 times_pairs_p <- void_string
 #
 if (direction == 'Stable') { test_direction = 'two.sided' ; tails = 'two-tail' }
 if (direction == 'Increase') { test_direction = 'less' ; tails = 'one-tails' }
 if (direction == 'Decrease') { test_direction = 'greater' ; tails = 'one-tails' }
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, times = times_description, times_pairs = times_pairs, times_pairs_p = times_pairs_p)
 #
 
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, times = times_description, times_pairs = times_pairs, times_pairs_p = times_pairs_p)
 return(RESULTS)
}

#
