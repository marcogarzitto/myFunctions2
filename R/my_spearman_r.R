#' my_spearman_r
#'
#' Function to do a Pearson's correlation.
#'
#' @param y Numeric vector. Default: None.
#' @param x Numeric vector. Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the Pearson's correlation), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, reporting the direction of the correlation), 'es' (string, effect-size for statistically significant results), 'groups' (string, actually a void string).
#' @export
my_spearman_r <- function (y, x, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), direction = 'Stable')
{
 result <- void_string
 p_value <- 1.0
 significance <- void_string
 comparison <- void_string
 effect_size <- void_string
 groups_description <- void_string
 #
 if (direction == 'Stable') { direction = 'two.sided' ; tails = 'two-tail' }
 if (direction == 'Increase') { direction = 'less' ; tails = 'one-tails' }
 if (direction == 'Decrease') { direction = 'greater' ; tails = 'one-tails' }
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 #
 DATA <- na.omit(data.frame(Y = y, X = x))
 DATA$Y <- as.numeric(DATA$Y)
 DATA$X <- as.numeric(DATA$X)
 #
 if (dim(DATA)[1] <= 3) { return(RESULTS) }
 if (identical(DATA$Y, DATA$X)) { return(RESULTS) }
 #
 TEST <- cor.test(DATA$Y, DATA$X, method = 'spearman')
 #
 result <- paste(my_nice_r(value = TEST$estimate, decimals = 3, with_r = TRUE, spearman = TRUE, with_equal_sign = FALSE, void_string = void_string), ',', ' ',
                 my_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                 sep = '')
 #
 p_value <- as.numeric(TEST$p.value)
 if (p_value < alpha_value) { significance <- '*' }
 #
 if (p_value < alpha_value)
 {
  if (TEST$estimate > 0) { comparison <- paste('Positive correlation', sep = '') }
  if (TEST$estimate < 0) { comparison <- paste('Negative correlation', sep = '') }
 } else
 {
  comparison <- paste('No-correlation', sep = '')
 }
 #
 if (p_value < alpha_value)
 {
  effect_size <- paste(my_nice_r(value = TEST$estimate, decimals = 3, with_r = TRUE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string),
                 sep = '')
  effect_size_interpretation <- ''
                             if (!is.na(TEST$estimate) & (abs(TEST$estimate) <= 0.1)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                             if (!is.na(TEST$estimate) & (abs(TEST$estimate)  > 0.1) & (abs(TEST$estimate) <= 0.3)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                             if (!is.na(TEST$estimate) & (abs(TEST$estimate)  > 0.3) & (abs(TEST$estimate) <= 0.7)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                             if (!is.na(TEST$estimate) & (abs(TEST$estimate)  > 0.7)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
                             effect_size <- paste(effect_size, effect_size_interpretation, sep = '')
 }
 groups_description <- void_string
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 return(RESULTS)
}

#
