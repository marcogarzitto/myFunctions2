#' my_chisquare
#'
#' Function to do a Fisher's exact test.
#'
#' @param a Factor vector. Default: None.
#' @param b Factor vector. Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param wise Boolean, if true, the most appropriate test is used according to the data. Default: TRUE
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the Chi-squared test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the group variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'groups' (string, frequencies of levels of b by levels of a).
#' @export
my_chisquare <- function (a, b, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), wise = TRUE, direction = 'Stable')
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
 DATA <- na.omit(data.frame(A = a, B = b))
 if (!is.factor(DATA$A)) { DATA$A <- ordered(DATA$A) }
 if (!is.factor(DATA$B)) { DATA$B <- ordered(DATA$B) }
 levels_input_all_a <- levels(DATA$A)
 levels_input_all_b <- levels(DATA$B)
 DATA$A <- droplevels(DATA$A)
 DATA$B <- droplevels(DATA$B)
 levels_input_drop_a <- levels(DATA$A)
 levels_input_drop_b <- levels(DATA$B)
 if (length(levels_input_all_a) == length(levels_input_drop_a)) { empty_levels_a <- 'All levels represented (1st-variable)' } else { empty_levels_a <- paste('Empy levels (excluded, 1st-variable)', ':', ' ', paste(levels_input_all_a[!(levels_input_all_a %in% levels_input_drop_a)], collapse = paste(',', ' ', sep = '')), sep = '') }
 if (length(levels_input_all_b) == length(levels_input_drop_b)) { empty_levels_b <- 'All levels represented (2nd-variable)' } else { empty_levels_b <- paste('Empy levels (excluded, 2nd-variable)', ':', ' ', paste(levels_input_all_b[!(levels_input_all_b %in% levels_input_drop_b)], collapse = paste(',', ' ', sep = '')), sep = '') }
 #
 if (wise & (length(levels(DATA$A)) == 2) & (length(levels(DATA$B)) == 2)) { return(my_fisher(a = a, b = b, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas)) }
 #
 if ((length(levels(DATA$A)) < 2) | (length(levels(DATA$B)) < 2)) { return(RESULTS) }
 if (identical(DATA$A, DATA$B)) { return(RESULTS) }
 #
 EXPECTED <- ((as.matrix(apply(table(DATA), 2, sum)) %*% apply(table(DATA), 1, sum)) / sum(table(DATA)))
 note <- ''
 if (sum(!apply(EXPECTED, 1, is.na)) > 0)
 {
  if (min(EXPECTED) < 5) { note <- '!not-applicable! ' }
 }
 #
 TEST <- chisq.test(DATA$A, DATA$B)
 #
 result <- paste(note,
                 '\u03C7', '\u00B2',
                 '(', TEST$parameter, ')', '=',
                 my_nice(value = TEST$statistic, decimals = 2, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string), ',', ' ',
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
  ES <- rcompanion::cramerV(table(DATA$A, DATA$B), ci = FALSE)
     if ((ES == 0) | (ES >= 0.999))
     {
      ES <- data.frame(est = ES, lower.ci = NA, upper.ci = NA)
     } else
     {
      ES <- rcompanion::cramerV(table(DATA$A, DATA$B), ci = TRUE)
         names(ES) <- c('est', 'lower.ci', 'upper.ci')
     }
  effect_size <- paste(my_nice(ES$est, decimals = 3, text = "Cramer's V", with_equal_sign = TRUE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ' ', '[', my_nice(ES$lower.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ',', ' ', my_nice(ES$upper.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string), ']',
                       sep = '')
  effect_size_interpretation <- ''
                             if (min(length(levels(DATA$A)) - 1, length(levels(DATA$B)) - 1) == 1)
                             {
                              if (!is.na(ES$est) & (abs(ES$est)  < 0.10)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.10) & (abs(ES$est) < 0.30)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.30) & (abs(ES$est) < 0.50)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.50)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
                             }
                             if (min(length(levels(DATA$A)) - 1, length(levels(DATA$B)) - 1) == 2)
                             {
                              if (!is.na(ES$est) & (abs(ES$est)  < 0.07)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.07) & (abs(ES$est) < 0.21)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.21) & (abs(ES$est) < 0.35)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.35)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
                             }
                             if (min(length(levels(DATA$A)) - 1, length(levels(DATA$B)) - 1) > 2)
                             {
                              if (!is.na(ES$est) & (abs(ES$est)  < 0.06)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.06) & (abs(ES$est) < 0.17)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.17) & (abs(ES$est) < 0.29)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                              if (!is.na(ES$est) & (abs(ES$est) >= 0.29)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
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
 empty_levels <- paste(c(empty_levels_a, empty_levels_b), collapse = paste(';', ' ', sep = ''))
 groups_description <- paste(c(groups_description, empty_levels), collapse = paste(';', ' ', sep = ''))
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 return(RESULTS)
}

#
