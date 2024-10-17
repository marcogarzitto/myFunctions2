#' my_pearson_r
#'
#' Function to do a Pearson's correlation.
#'
#' @param y Numeric vector. Dependent variable (using wide format for data-frame). Default: None.
#' @param x Numeric vector. Independent variable (using wide format for data-frame). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param wise Boolean, if true, the most appropriate test is used according to the data. Default: TRUE
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the Pearson's correlation), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, reporting the direction of the correlation), 'es' (string, effect-size for statistically significant results), 'groups' (string, actually a void string), 'groups_pairs' (string, actually a void string), 'groups_pairs_p' (string, actually a void string).
#' @export
my_pearson_r <- function (y, x, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), wise = TRUE, direction = 'Stable')
{
 result <- void_string
 p_value <- 1.0
 significance <- void_string
 comparison <- void_string
 effect_size <- void_string
 groups_description <- void_string
 groups_pairs <- void_string
 groups_pairs_p <- void_string
 #
 if (direction == 'Stable') { test_direction = 'two.sided' ; tails = 'two-tail' }
 if (direction == 'Increase') { test_direction = 'less' ; tails = 'one-tails' }
 if (direction == 'Decrease') { test_direction = 'greater' ; tails = 'one-tails' }
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description, groups_pairs = groups_pairs, groups_pairs_p = groups_pairs_p)
 #
 DATA <- na.omit(data.frame(Y = y, X = x))
 DATA$Y <- as.numeric(DATA$Y)
 DATA$X <- as.numeric(DATA$X)
 #
 if (dim(DATA)[1] <= 3) { return(RESULTS) }
 if (identical(DATA$Y, DATA$X)) { return(RESULTS) }
 #
 if (wise & (shapiro.test(DATA$Y)[2] < 0.050) | (wise & shapiro.test(DATA$X)[2] < 0.050)) { return(my_spearman_r(y = y, x = x, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas)) }
 #
 note <- ''
      if ((shapiro.test(DATA$Y)[2] < 0.050) | (shapiro.test(DATA$X)[2] < 0.050)) { note <- '!not-applicable! ' }
 TEST <- cor.test(DATA$Y, DATA$X, method = 'pearson', alternative = test_direction)
 #
 result <- paste(note,
                 my_nice_r(value = TEST$estimate, decimals = 3, with_r = TRUE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string),
                 ' ', '[', my_nice_r(value = TEST$conf.int[1], decimals = 3, with_r = FALSE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string),
                 ',', ' ', my_nice_r(value = TEST$conf.int[2], decimals = 3, with_r = FALSE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string), ']',
                 ', ',
                 # ' ', '(', tails, ')', ',', ' ',
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
                 ' ', '[', my_nice_r(value = TEST$conf.int[1], decimals = 3, with_r = FALSE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string),
                 ',', ' ', my_nice_r(value = TEST$conf.int[2], decimals = 3, with_r = FALSE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string), ']',
                 sep = '')
  effect_size_interpretation <- ''
                             if (!is.na(TEST$estimate) & (abs(TEST$estimate) <= 0.1)) { effect_size_interpretation <- paste(',', ' ', 'negligible', sep = '') }
                             if (!is.na(TEST$estimate) & (abs(TEST$estimate)  > 0.1) & (abs(TEST$estimate) <= 0.3)) { effect_size_interpretation <- paste(',', ' ', 'small', sep = '') }
                             if (!is.na(TEST$estimate) & (abs(TEST$estimate)  > 0.3) & (abs(TEST$estimate) <= 0.7)) { effect_size_interpretation <- paste(',', ' ', 'moderate', sep = '') }
                             if (!is.na(TEST$estimate) & (abs(TEST$estimate)  > 0.7)) { effect_size_interpretation <- paste(',', ' ', 'large', sep = '') }
                             effect_size <- paste(effect_size, effect_size_interpretation, sep = '')
 }
 groups_description <- void_string
 #
 groups_pairs <- void_string
 groups_pairs_p <- void_string
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description, groups_pairs = groups_pairs, groups_pairs_p = groups_pairs_p)
 return(RESULTS)
}

#
