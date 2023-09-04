#' my_ttest
#'
#' Function to do a betwee-subjects Test t.
#'
#' @param y Numeric vector. Default: None.
#' @param group Factor vector (with 2 levels). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @return A list with results: 'test' (string, with results of the between-subjects t-test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the group variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'groups' (string, mean and SD for the levels of the group variable).
#' @export
my_ttest <- function (y, group, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
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
 if (!is.factor(group)) { group <- ordered(group) }  
 group <- droplevels(group)
 DATA <- na.omit(data.frame(Y = y, G = group))
 #
 if (length(levels(DATA$G)) != 2) { return(RESULTS) }
 #
 Y1 <- DATA$Y[DATA$G == levels(DATA$G)[1]]
 Y2 <- DATA$Y[DATA$G == levels(DATA$G)[2]]
 if (identical(Y1, Y2)) { return(RESULTS) }
 #
 if ((min(table(DATA$G)) < 3) & (sd(DATA$Y) <= 0)) { return(RESULTS) }
 #
 LEVENE <- car::leveneTest(Y ~ G, data = DATA, center = median)
        note <- ''
        if (LEVENE$'Pr(>F)'[1] < 0.050) { note <- ' (not-applicable)' }
 TEST <- t.test(Y ~ G, data = DATA)
 #
 result <- paste('t',
                 '(', my_nice(value = TEST$parameter, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0, max_value = Inf, void_string = void_string), ')',
                 my_nice(value = TEST$statistic, decimals = 2, text = '', with_equal_sign = TRUE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string),
                 note, ',', ' ',
                 my_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                 sep = '')
 #
 p_value <- as.numeric(TEST$p.value)
 if (p_value < alpha_value) { significance <- '*' }
 #
 if (p_value < alpha_value)
 {
  if (mean(Y1, na.rm = TRUE) > mean(Y1, na.rm = TRUE)) { comparison <- paste(levels(DATA$G)[1], ' ', '>', ' ', levels(DATA$G)[2], sep = '') }
  if (mean(Y2, na.rm = TRUE) < mean(Y2, na.rm = TRUE)) { comparison <- paste(levels(DATA$G)[1], ' ', '<', ' ', levels(DATA$G)[2], sep = '') }
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
                             if (!is.na(COHEN$estimate) & (COHEN$estimate <= 0.2)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                             if (!is.na(COHEN$estimate) & (COHEN$estimate  > 0.2) & (COHEN$estimate <= 0.5)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                             if (!is.na(COHEN$estimate) & (COHEN$estimate  > 0.5) & (COHEN$estimate <= 0.8)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                             if (!is.na(COHEN$estimate) & (COHEN$estimate  > 0.8)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
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
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 return(RESULTS)
}

#
