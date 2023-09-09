#' my_fisher
#'
#' Function to do a Fisher's exact test.
#'
#' @param a Factor vector (with 2 levels). Default: None.
#' @param b Factor vector (with 2 levels). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @return A list with results: 'test' (string, with results of the Fisher's exact test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the group variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'groups' (string, frequencies of levels of b by levels of a).
#' @export
my_fisher <- function (a, b, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 result <- void_string
 p_value <- 1.0
 significance <- void_string
 comparison <- void_string
 effect_size <- void_string
 groups_description <- void_string
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
 if ((length(levels(DATA$A)) != 2) & (length(levels(DATA$B)) != 2)) { return(RESULTS) }
 if (identical(DATA$A, DATA$B)) { return(RESULTS) }
 #
 A1 <- DATA$A[DATA$B == levels(DATA$B)[1]]
 A2 <- DATA$A[DATA$B == levels(DATA$B)[2]]
 if (identical(A1, A2)) { return(RESULTS) }
 #
 TEST <- fisher.test(DATA$A, DATA$B)
 #
 annotation <- ''
            if (!is.na(TEST$estimate) & (TEST$estimate  < 1.5)) { annotation <- paste(',', ' ', 'negligible effect', sep = '') }
            if (!is.na(TEST$estimate) & (TEST$estimate >= 1.5) & (TEST$estimate < 3.5)) { annotation <- paste(',', ' ', 'small effect', sep = '') }
            if (!is.na(TEST$estimate) & (TEST$estimate >= 3.5) & (TEST$estimate < 9.0)) { annotation <- paste(',', ' ', 'moderate effect', sep = '') }
            if (!is.na(TEST$estimate) & (TEST$estimate >= 9.0)) { annotation <- paste(',', ' ', 'large effect', sep = '') }
 annotation <- paste(my_nice(value = abs(TEST$estimate), decimals = 3, text = 'OR', with_equal_sign = TRUE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
                 ' ', '[', my_nice(value = abs(TEST$conf.int[1]), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
                 ',', ' ', my_nice(value = abs(TEST$conf.int[2]), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
                 ']', annotation,
                 sep = '')
 result <- paste(my_nice(value = abs(TEST$estimate), decimals = 3, text = 'OR', with_equal_sign = TRUE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
                 ' ', '[', my_nice(value = abs(TEST$conf.int[1]), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
                 ',', ' ', my_nice(value = abs(TEST$conf.int[2]), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
                 ']',
                 ',', ' ', my_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
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
  ES <- rcompanion::phi(table(DATA$A, DATA$B), ci = TRUE)
  effect_size <- paste(my_nice(ES$phi, decimals = 3, text = "\u03C6", with_equal_sign = TRUE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ' ', '[', my_nice(ES$lower.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ',', ' ', my_nice(ES$upper.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string), ']',
                       sep = '')
  effect_size_interpretation <- ''
                             if (!is.na(ES$phi) & (abs(ES$phi)  < 0.1)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                             if (!is.na(ES$phi) & (abs(ES$phi) >= 0.1) & (abs(ES$phi) < 0.3)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                             if (!is.na(ES$phi) & (abs(ES$phi) >= 0.3) & (abs(ES$phi) < 0.5)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                             if (!is.na(ES$phi) & (abs(ES$phi) >= 0.5)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
  effect_size <- paste(effect_size, effect_size_interpretation, sep = '')
  effect_size <- paste(effect_size, ' ', '(', annotation, ')', sep = '')
 }
 groups_description <- paste(levels(DATA$A)[1], ':', ' ',
                             my_nice_percent(100 * length(DATA$A[(DATA$A == levels(DATA$A)[1]) & (DATA$B == levels(DATA$B)[1])]) / length(DATA$A[(DATA$A == levels(DATA$A)[1])]), decimals = 2, text = '', percent_sign = TRUE, per_level = '100', min_value = 0, max_value = 100, with_equal_sign = FALSE, with_sign = FALSE, void_string = void_string),
                             ' ', '(', levels(DATA$B)[1], ')',
                             ' ', 'and', ' ',
                             my_nice_percent(100 * length(DATA$A[(DATA$A == levels(DATA$A)[1]) & (DATA$B == levels(DATA$B)[2])]) / length(DATA$A[(DATA$A == levels(DATA$A)[1])]), decimals = 2, text = '', percent_sign = TRUE, per_level = '100', min_value = 0, max_value = 100, with_equal_sign = FALSE, with_sign = FALSE, void_string = void_string),
                             ' ', '(', levels(DATA$B)[2], ')',
                             ' ', '-vs-', ' ',
                             levels(DATA$A)[2], ':', ' ',
                             my_nice_percent(100 * length(DATA$A[(DATA$A == levels(DATA$A)[2]) & (DATA$B == levels(DATA$B)[1])]) / length(DATA$A[(DATA$A == levels(DATA$A)[2])]), decimals = 2, text = '', percent_sign = TRUE, per_level = '100', min_value = 0, max_value = 100, with_equal_sign = FALSE, with_sign = FALSE, void_string = void_string),
                             ' ', '(', levels(DATA$B)[1], ')',
                             ' ', 'and', ' ',
                             my_nice_percent(100 * length(DATA$A[(DATA$A == levels(DATA$A)[2]) & (DATA$B == levels(DATA$B)[2])]) / length(DATA$A[(DATA$A == levels(DATA$A)[2])]), decimals = 2, text = '', percent_sign = TRUE, per_level = '100', min_value = 0, max_value = 100, with_equal_sign = FALSE, with_sign = FALSE, void_string = void_string),
                             ' ', '(', levels(DATA$B)[2], ')',
                             sep = '')
 empty_levels <- paste(c(empty_levels_a, empty_levels_b), collapse = paste(';', ' ', sep = ''))
 groups_description <- paste(c(groups_description, empty_levels), collapse = paste(';', ' ', sep = ''))
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 return(RESULTS)
}

#
