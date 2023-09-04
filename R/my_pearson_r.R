#' my_pearson_r
#'
#' Function to do a Pearson's correlation.
#'
#' @param y Numeric vector. Default: None.
#' @param x Numeric vector. Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @return A list with results: 'test' (string, with results of the Pearson's correlation), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, reporting the direction of the correlation), 'es' (string, effect-size for statistically significant results), 'groups' (string, actually a void string).
#' @export
my_pearson_r <- function (y, x, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 result <- void_string
 p_value <- 1.0
 significance <- void_string
 comparison <- void_string
 effect_size <- void_string
 groups_description <- void_string
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 #
 y <- as.numeric(y)
 x <- as.numeric(x)
 DATA <- na.omit(data.frame(Y = y, X = x))
 #
 if (dim(DATA)[1] <= 3) { return(RESULTS) }
 if (identical(DATA$Y, DATA$X)) { return(RESULTS) }
 #
 note <- ''
      if ((shapiro.test(DATA$Y)[2] < 0.050) & (shapiro.test(DATA$X)[2] < 0.050)) { note <- ' (not-applicable)' }
 TEST <- cor.test(DATA$Y, DATA$X, method = 'pearson')
 #
 result <- paste(my_nice_r(value = TEST$estimate, decimals = 3, with_r = TRUE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string),
                 ' ', '[', my_nice_r(value = TEST$conf.int[1], decimals = 3, with_r = FALSE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string),
                 ',', ' ', my_nice_r(value = TEST$conf.int[2], decimals = 3, with_r = FALSE, spearman = FALSE, with_equal_sign = FALSE, void_string = void_string), ']',
                 note, ',', ' ',
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
