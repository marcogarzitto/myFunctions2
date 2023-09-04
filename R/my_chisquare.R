#' my_chisquare
#'
#' Function to do a Fisher's exact test.
#'
#' @param a Factor vector. Default: None.
#' @param b Factor vector. Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @return A list with results: 'test' (string, with results of the Chi-squared test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the group variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'groups' (string, frequencies of levels of b by levels of a).
#' @export
my_chisquare <- function (a, b, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 result <- void_string
 p_value <- 1.0
 significance <- void_string
 comparison <- void_string
 effect_size <- void_string
 groups_description <- void_string
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 #
 if (!is.factor(a)) { a <- ordered(a) }
 if (!is.factor(b)) { b <- ordered(b) }
 a <- droplevels(a)
 b <- droplevels(b)
 DATA <- na.omit(data.frame(A = a, B = b))
 #
 if ((length(levels(DATA$A)) < 2) | (length(levels(DATA$B)) < 2)) { return(RESULTS) }
 if (identical(DATA$A, DATA$B)) { return(RESULTS) }
 #
 EXPECTED <- ((as.matrix(apply(table(DATA), 2, sum)) %*% apply(table(DATA), 1, sum)) / sum(table(DATA)))
 note <- ''
 if (sum(!apply(EXPECTED, 1, is.na)) > 0)
 {
  if (min(EXPECTED) < 5) { note <- ' (not-applicable)' }
 }
 #
 TEST <- chisq.test(DATA$A, DATA$B)
 #
 result <- paste('\u03C7', '\u00B2',
                 '(', TEST$parameter, ')', '=',
                 my_nice(value = TEST$statistic, decimals = 2, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
                 note, ',', ' ',
                 my_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                 sep = '')
 #
 p_value <- as.numeric(TEST$p.value)
 if (p_value < alpha_value) { significance <- '*' }
 #
 if (p_value < alpha_value)
 {
  comparison <- 'Inhomogeneous'
 } else
 {
  comparison <- 'Homogeneous'
 }
 #
 if (p_value < alpha_value)
 {
  ES <- rcompanion::cramerV(table(DATA$A, DATA$B), ci = TRUE)
  effect_size <- paste(my_nice(ES$Cramer.V, decimals = 3, text = "Cramer's V", with_equal_sign = TRUE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ' ', '[', my_nice(ES$lower.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ',', ' ', my_nice(ES$upper.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string), ']',
                       sep = '')
  effect_size_interpretation <- ''
                             if (min(length(levels(DATA$A)) - 1, length(levels(DATA$B)) - 1) == 1)
                             {
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V)  < 0.10)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.10) & (abs(ES$Cramer.V) < 0.30)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.30) & (abs(ES$Cramer.V) < 0.50)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.50)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
                             }
                             if (min(length(levels(DATA$A)) - 1, length(levels(DATA$B)) - 1) == 2)
                             {
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V)  < 0.07)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.07) & (abs(ES$Cramer.V) < 0.21)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.21) & (abs(ES$Cramer.V) < 0.35)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.35)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
                             }
                             if (min(length(levels(DATA$A)) - 1, length(levels(DATA$B)) - 1) > 2)
                             {
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V)  < 0.06)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.06) & (abs(ES$Cramer.V) < 0.17)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.17) & (abs(ES$Cramer.V) < 0.29)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                              if (!is.na(ES$Cramer.V) & (abs(ES$Cramer.V) >= 0.29)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
                             }
  effect_size <- paste(effect_size, effect_size_interpretation, sep = '')
 }
 #
 groups_description <- c()
 for (level_a in levels(DATA$A))
 {
  inside_group <- c()
  for (level_b in levels(DATA$B))
  {
   inside_group <- c(inside_group,
                     paste(my_nice_percent(100 * length(DATA$A[(DATA$A == level_a) & (DATA$B == level_b)]) / length(DATA$A[(DATA$A == level_a)]), decimals = 2, text = '', percent_sign = TRUE, per_level = '100', min_value = 0, max_value = 100, with_equal_sign = FALSE, with_sign = FALSE, void_string = void_string),
                           ' ', '(', level_b, ')',
                           sep = ''))
  }
  groups_description <- c(groups_description, paste(level_a, ':', ' ', paste(inside_group, collapse = paste(' ', 'and', ' ', sep = '')), sep = ''))
 }
 groups_description <- paste(groups_description, collapse = paste(' ', '-vs-', ' ', sep = ''))
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 return(RESULTS)
}

#
