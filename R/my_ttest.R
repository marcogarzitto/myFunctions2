#' my_ttest
#'
#' Function to do a betwee-subjects Test t.
#'
#' @param y Numeric vector. Dependent variable (using wide format for data-frame). Default: None.
#' @param group Factor vector (with 2 levels). Independent/Group variable (using wide format for data-frame). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param wise Boolean, if true, the most appropriate test is used according to the data. Default: TRUE
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the between-subjects t-test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the group variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'groups' (string, mean and SD for the levels of the group variable), 'groups_pairs' (list of vectors, every vector reports two groups corresponding to post-hoc comparisons), 'groups_pairs_p' (vector of numeric, significances corresponding to post-hoc comparisons).
#' @export
my_ttest <- function (y, group, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), wise = TRUE, direction = 'Stable')
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
 DATA <- na.omit(data.frame(Y = y, G = group))
 DATA$Y <- as.numeric(DATA$Y)
 if (!is.factor(DATA$G)) { DATA$G <- ordered(DATA$G) }
 levels_input_all <- levels(DATA$G)
 DATA$G <- droplevels(DATA$G)
 levels_input_drop <- levels(DATA$G)
 if (length(levels_input_all) == length(levels_input_drop)) { empty_levels <- 'All levels represented' } else { empty_levels <- paste('Empy levels (excluded)', ':', ' ', paste(levels_input_all[!(levels_input_all %in% levels_input_drop)], collapse = paste(',', ' ', sep = '')), sep = '') }
 #
 if (length(levels(DATA$G)) != 2) { return(RESULTS) }
 #
 Y1 <- DATA$Y[DATA$G == levels(DATA$G)[1]]
 Y2 <- DATA$Y[DATA$G == levels(DATA$G)[2]]
 if (identical(Y1, Y2)) { return(RESULTS) }
 #
 if ((min(table(DATA$G)) < 3) | (min(table(!is.na(DATA$Y), DATA$G)) < 3) | (sd(DATA$Y) <= 0) | (is.na(sd(DATA$Y)))) { return(RESULTS) }
 #
 LEVENE <- car::leveneTest(Y ~ G, data = DATA, center = median)
 #
 NORMALITY_VIOLATION <- DATA %>% group_by(G) %>% summarise(sd = sd(Y))
 if (TRUE %in% ((NORMALITY_VIOLATION$sd <= 0) | is.na(NORMALITY_VIOLATION$sd)))
 {
  NORMALITY_VIOLATION <- TRUE
 } else
 {
  NORMALITY_VIOLATION <- DATA %>% dplyr::group_by(G) %>% rstatix::shapiro_test(Y)
  NORMALITY_VIOLATION <- TRUE %in% (NORMALITY_VIOLATION$p < 0.050)
 }
 #
 if (wise & ((LEVENE$'Pr(>F)'[1] < 0.050) | NORMALITY_VIOLATION)) { return(my_mannwhitney(y = y, group = group, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, direction = direction)) }
 #
 note <- ''
      if ((LEVENE$'Pr(>F)'[1] < 0.050) | NORMALITY_VIOLATION) { note <- '!not-applicable! ' }
 TEST <- t.test(Y ~ G, data = DATA, alternative = test_direction)
 #
 result <- paste(note, 
                 't',
                 '(', my_nice(value = TEST$parameter, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0, max_value = Inf, void_string = void_string), ')',
                 my_nice(value = TEST$statistic, decimals = 2, text = '', with_equal_sign = TRUE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                 ' ', '(', tails, ')', ',', ' ',
                 my_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                 sep = '')
 #
 p_value <- as.numeric(TEST$p.value)
 if (p_value < alpha_value) { significance <- '*' }
 #
 if (p_value < alpha_value)
 {
  if (mean(Y1, na.rm = TRUE) > mean(Y2, na.rm = TRUE)) { comparison <- paste(levels(DATA$G)[1], ' ', '>', ' ', levels(DATA$G)[2], sep = '') }
  if (mean(Y1, na.rm = TRUE) < mean(Y2, na.rm = TRUE)) { comparison <- paste(levels(DATA$G)[1], ' ', '<', ' ', levels(DATA$G)[2], sep = '') }
 } else
 {
  comparison <- paste(levels(DATA$G)[1], ' ', '=', ' ', levels(DATA$G)[2], sep = '')
 }
 #
 if (p_value < alpha_value)
 {
  COHEN <- effsize::cohen.d(DATA$Y, DATA$G, pooled = TRUE, paired = FALSE, na.rm = FALSE, hedges.correction = TRUE, conf.level = 0.950)
  effect_size <- paste(my_nice(COHEN$estimate, decimals = 3, text = "Hedges-corrected Cohen's d", with_equal_sign = TRUE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ' ', '[', my_nice(COHEN$conf.int[1], decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ',', ' ', my_nice(COHEN$conf.int[2], decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string), ']',
                       sep = '')
  effect_size_interpretation <- ''
                             if (!is.na(COHEN$estimate) & (abs(COHEN$estimate) <= 0.2)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                             if (!is.na(COHEN$estimate) & (abs(COHEN$estimate)  > 0.2) & (abs(COHEN$estimate) <= 0.5)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                             if (!is.na(COHEN$estimate) & (abs(COHEN$estimate)  > 0.5) & (abs(COHEN$estimate) <= 0.8)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                             if (!is.na(COHEN$estimate) & (abs(COHEN$estimate)  > 0.8)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
  effect_size <- paste(effect_size, effect_size_interpretation, sep = '')
 }
 groups_description <- paste(levels(DATA$G)[1], ':', ' ',
                             my_nice(mean(Y1, na.rm = TRUE), decimals = 2, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                             ' ', '\u00B1', my_nice(sd(Y1, na.rm = TRUE), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string),
                             ' ', '-vs-', ' ',
                             levels(DATA$G)[2], ':', ' ',
                             my_nice(mean(Y2, na.rm = TRUE), decimals = 2, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                             ' ', '\u00B1', my_nice(sd(Y2, na.rm = TRUE), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string),
                             sep = '')
 groups_description <- paste(c(groups_description, empty_levels), collapse = paste(';', ' ', sep = ''))
 #
 groups_pairs <- list(levels(DATA$G))
 groups_pairs_p <- c(p_value)
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description, groups_pairs = groups_pairs, groups_pairs_p = groups_pairs_p)
 return(RESULTS)
}

#
