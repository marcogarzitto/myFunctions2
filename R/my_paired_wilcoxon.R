#' my_paired_wilcoxon
#'
#' Function to do a within-subjects Wilcoxon's test.
#'
#' @param y Numeric vector. Dependent variable (using long format for data-frame). Default: None.
#' @param time Factor vector (with 2 levels). Independent/Time variable (using long format for data-frame). Default: None.
#' @param observations Factor vector, with the observation identifier (using long format for data-frame). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the withween-subjects Wilcoxon's test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the time variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'times' (string, mean and SD for the levels of the time variable), 'times_pairs' (list of vectors, every vector reports two times corresponding to post-hoc comparisons), 'times_pairs_p' (vector of numeric, significances corresponding to post-hoc comparisons).
#' @export
my_paired_wilcoxon <- function (y, time, observations, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), direction = 'Stable')
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
 DATA <- na.omit(data.frame(Y = y, T = time))
 DATA$Y <- as.numeric(DATA$Y)
 if (!is.factor(DATA$T)) { DATA$T <- ordered(DATA$T) }
 levels_input_all <- levels(DATA$T)
 DATA$T <- droplevels(DATA$T)
 levels_input_drop <- levels(DATA$T)
 if (length(levels_input_all) == length(levels_input_drop)) { empty_levels <- 'All levels represented' } else { empty_levels <- paste('Empy levels (excluded)', ':', ' ', paste(levels_input_all[!(levels_input_all %in% levels_input_drop)], collapse = paste(',', ' ', sep = '')), sep = '') }
 #
 if (length(levels(DATA$T)) != 2) { return(RESULTS) }
 #
 Y1 <- DATA$Y[DATA$T == levels(DATA$T)[1]]
 Y2 <- DATA$Y[DATA$T == levels(DATA$T)[2]]
 if (identical(Y1, Y2)) { return(RESULTS) }
 #
 if ((min(table(DATA$T)) < 3) | (min(table(!is.na(DATA$Y), DATA$T)) < 3) | (sd(DATA$Y) <= 0) | (is.na(sd(DATA$Y)))) { return(RESULTS) }
 #
 TEST <- wilcox.test(Y ~ T, data = DATA, exact = TRUE, correct = TRUE, paired = TRUE, alternative = test_direction)
 #
 result <- paste(my_nice(value = TEST$statistic, decimals = 1, text = 'U', with_equal_sign = TRUE, with_sign = FALSE, min_value = 0, max_value = 9999.99, void_string = void_string), ' ', '(', tails, ')', ',', ' ',
                 my_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                 sep = '')
 #
 p_value <- as.numeric(TEST$p.value)
 if (p_value < alpha_value) { significance <- '*' }
 #
 if (p_value < alpha_value)
 {
  if (mean(Y1, na.rm = TRUE) > mean(Y2, na.rm = TRUE)) { comparison <- paste(levels(DATA$T)[1], ' ', '>', ' ', levels(DATA$T)[2], sep = '') }
  if (mean(Y1, na.rm = TRUE) < mean(Y2, na.rm = TRUE)) { comparison <- paste(levels(DATA$T)[1], ' ', '<', ' ', levels(DATA$T)[2], sep = '') }
 } else
 {
  comparison <- paste(levels(DATA$T)[1], ' ', '=', ' ', levels(DATA$T)[2], sep = '')
 }
 #
 if (p_value < alpha_value)
 {
  ES <- rcompanion::wilcoxonPairedRC(x = DATA$Y, g = DATA$T)
     if (ES == 1)
     {
      ES <- data.frame(est = ES, lower.ci = NA, upper.ci = NA)
     } else
     {
      ES <- rcompanion::wilcoxonPairedRC(x = DATA$Y, g = DATA$T, ci = TRUE)
         names(ES) <- c('est', 'lower.ci', 'upper.ci')
     }
  effect_size <- paste(my_nice(ES$est, decimals = 3, text = 'Rank-biserial r', with_equal_sign = TRUE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ' ', '[', my_nice(ES$lower.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ',', ' ', my_nice(ES$upper.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string), ']',
                       sep = '')
  effect_size_interpretation <- ''
                             if (!is.na(ES$est) & (abs(ES$est) <= 0.1)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                             if (!is.na(ES$est) & (abs(ES$est) > 0.1) & (abs(ES$est) <= 0.3)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                             if (!is.na(ES$est) & (abs(ES$est) > 0.3) & (abs(ES$est) <= 0.7)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                             if (!is.na(ES$est) & (abs(ES$est) > 0.7)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
  effect_size <- paste(effect_size, effect_size_interpretation, sep = '')
 }
 times_description <- paste(levels(DATA$T)[1], ':', ' ',
                             my_nice(mean(Y1, na.rm = TRUE), decimals = 2, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                             ' ', '\u00B1', my_nice(sd(Y1, na.rm = TRUE), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string),
                             ' ', '-vs-', ' ',
                             levels(DATA$T)[2], ':', ' ',
                             my_nice(mean(Y2, na.rm = TRUE), decimals = 2, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                             ' ', '\u00B1', my_nice(sd(Y2, na.rm = TRUE), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string),
                             sep = '') 
 times_description <- paste(c(times_description, empty_levels), collapse = paste(';', ' ', sep = ''))
 #
 times_pairs <- list(levels(DATA$T))
 times_pairs_p <- c(p_value)
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, times = times_description, times_pairs = times_pairs, times_pairs_p = times_pairs_p)
 return(RESULTS)
}

#
