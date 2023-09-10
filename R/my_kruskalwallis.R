#' my_kruskalwallis
#'
#' Function to do a Kruskal-Wallis' rank sum test.
#'
#' @param y Numeric vector. Default: None.
#' @param group Factor vector. Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @return A list with results: 'test' (string, with results of the Kruskal-Wallis' rank sum test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the group variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'groups' (string, mean and SD for the levels of the group variable).
#' @export
my_kruskalwallis <- function (y, group, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 result <- void_string
 p_value <- 1.0
 significance <- void_string
 comparison <- void_string
 effect_size <- void_string
 groups_description <- void_string
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 #
 DATA <- na.omit(data.frame(Y = y, G = group))
 DATA$Y <- as.numeric(DATA$Y)
 if (!is.factor(DATA$G)) { DATA$G <- ordered(DATA$G) }
 levels_input_all <- levels(DATA$G)
 DATA$G <- droplevels(DATA$G)
 levels_input_drop <- levels(DATA$G)
 if (length(levels_input_all) == length(levels_input_drop)) { empty_levels <- '' } else { empty_levels <- paste('Empy levels (excluded)', ':', ' ', paste(levels_input_all[!(levels_input_all %in% levels_input_drop)], collapse = paste(',', ' ', sep = '')), sep = '') }
 levels_new <- gsub('-', '§§§', levels_input_drop)
 levels(DATA$G) <- levels_new
 #
 if (length(levels(DATA$G)) < 2) { return(RESULTS) }
 #
 if ((min(table(DATA$G)) < 3) | (min(table(!is.na(DATA$Y), DATA$G)) < 3) | (sd(DATA$Y) <= 0) | (is.na(sd(DATA$Y)))) { return(RESULTS) }
 #
 MOD <- lm(Y ~ G, data = DATA)
 TEST <- kruskal.test(Y ~ G, data = DATA)
 #
 result <- paste('K','(', TEST$parameter, ')', my_nice(TEST$statistic, decimals = 2, text = '', with_equal_sign = TRUE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = '-'),
                   ', ', my_nice_p(TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                   sep = '')
 #
 p_value <- as.numeric(TEST$p.value)
 if (p_value < alpha_value) { significance <- '*' }
 #
 if (p_value < alpha_value)
 {
  POST <- as.data.frame(FSA::dunnTest(DATA$Y, DATA$G, method = 'bh')[2])
       names(POST) <- c('groups', 'z', 'P.unadj', 'p')
  # POST <- as.data.frame(TukeyHSD(aov(MOD))[1])
  #      names(POST) <- c('d', 'l', 'u', 'p')
  ROWS <- c()
  for (ROW in POST$groups)
  {
   SIGN <- '='
   if (POST[POST$groups == ROW, c('p')] < 0.050)
   {
    if (POST[POST$groups == ROW, c('z')] < 0) { SIGN <- '<' }
    if (POST[POST$groups == ROW, c('z')] > 0) { SIGN <- '>' }
   } else { SIGN <- '=' }
   ROW <- paste(gsub('-', paste(SIGN, sep = ''), ROW),
                ' ', '(', my_nice_p(POST[POST$groups == ROW, c('p')], decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string), ')', sep = '')
   ROW <- gsub('§§§', '-', ROW)
   ROWS <- c(ROWS, ROW)
  }
  comparison <- paste(ROWS, collapse = paste(';', ' ', sep = ''))
 } else
 {
  comparison <- paste(levels(DATA$G), collapse = paste(' ', '=', ' ', sep = ''))
 }
 comparison <- gsub('§§§', '-', comparison)
 #
 if (p_value < alpha_value)
 {
  ES <- rcompanion::epsilonSquared(DATA$Y, DATA$G, ci = TRUE, conf = 0.950)
     names(ES) <- c('est', 'lower.ci', 'upper.ci')
  effect_size <- paste(my_nice(ES$est, decimals = 3, text = "\u03B5\u00B2", with_equal_sign = TRUE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ' ', '[', my_nice(ES$lower.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ',', ' ', my_nice(ES$upper.ci, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string), ']',
                       sep = '')
  effect_size_interpretation <- ''
                             if (!is.na(ES$est) & (ES$est <= 0.01)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                             if (!is.na(ES$est) & (ES$est  > 0.01) & (ES$est <= 0.04)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                             if (!is.na(ES$est) & (ES$est  > 0.04) & (ES$est <= 0.36)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                             if (!is.na(ES$est) & (ES$est  > 0.36)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
  effect_size <- paste(effect_size, effect_size_interpretation, sep = '')
 }
 groups_description  <- c()
 for (level_a in levels(DATA$G))
 {
  inside_group <- as.data.frame(psych::describeBy(DATA$Y, DATA$G, mat = TRUE)[, c('group1', 'mean', 'sd')])
  inside_group$group1 <- paste(inside_group$group1, ':', sep = '')
  inside_group$mean <- sapply(inside_group$mean, my_nice, decimals = 2, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string)
  inside_group$sd <- paste('\u00B1', sapply(inside_group$sd, my_nice, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string), sep = '')
  inside_group <- apply(inside_group, 1, paste, collapse = ' ')
 }
 groups_description <- paste(inside_group, collapse = paste(' ', '-vs-', ' ', sep = ''))
 groups_description <- paste(c(groups_description, empty_levels), collapse = paste(';', ' ', sep = ''))
 groups_description <- gsub('§§§', '-', groups_description)
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, groups = groups_description)
 return(RESULTS)
}

#
