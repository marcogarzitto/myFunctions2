#' my_paired_anova
#'
#' Function to do a one-way withween-subjects ANOVA.
#'
#' @param y Numeric vector. Dependent variable (using long format for data-frame). Default: None.
#' @param time Factor vector. Independent/Time variable (using long format for data-frame). Default: None.
#' @param observations Factor vector, with the observation identifier (using long format for data-frame). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param wise Boolean, if true, the most appropriate test is used according to the data. Default: TRUE
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the one-way withween-subjects ANOVA), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the time variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'times' (string, mean and SD for the levels of the time variable), 'times_pairs' (list of vectors, every vector reports two times corresponding to post-hoc comparisons), 'times_pairs_p' (vector of numeric, significances corresponding to post-hoc comparisons).
#' @export
my_paired_anova <- function (y, time, observations, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), wise = TRUE, direction = 'Stable')
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
 DATA <- na.omit(data.frame(Y = y, T = time, O = observations))
 DATA$Y <- as.numeric(DATA$Y)
 if (!is.factor(DATA$T)) { DATA$T <- ordered(DATA$T) }
 levels_input_all <- levels(DATA$T)
 DATA$T <- droplevels(DATA$T)
 levels_input_drop <- levels(DATA$T)
 #
 if (!is.factor(DATA$O)) { DATA$O <- ordered(DATA$O) }
 observations_input_all <- levels(DATA$O)
 observations_input_drop <- (table(DATA$O) == length(levels_input_drop))
 DATA <- subset(DATA, O %in% names(observations_input_drop)[observations_input_drop])
 DATA$O <- droplevels(DATA$O)
 observations_input_drop <- levels(DATA$O)
 #
 if (length(levels_input_all) == length(levels_input_drop)) { empty_levels <- 'All levels represented' } else { empty_levels <- paste('Empy levels (excluded)', ':', ' ', paste(levels_input_all[!(levels_input_all %in% levels_input_drop)], collapse = paste(',', ' ', sep = '')), sep = '') }
 levels_new <- gsub('-', '§§§', levels_input_drop)
 levels(DATA$T) <- levels_new
 #
 if (wise & (length(levels(DATA$T)) == 2)) { return(my_paired_ttest(y = y, time = time, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction)) }
 #
 if (length(levels(DATA$T)) < 2) { return(RESULTS) }
 #
 if ((min(table(DATA$T)) < 3) | (min(table(!is.na(DATA$Y), DATA$T)) < 3) | (sd(DATA$Y) <= 0) | (is.na(sd(DATA$Y)))) { return(RESULTS) }
 #
 OUTLIERS_EXTREME <- DATA %>% dplyr::group_by(T) %>% rstatix::identify_outliers(Y)
          OUTLIERS_EXTREME_N <- sum(OUTLIERS_EXTREME$is.extreme)
          OUTLIERS_EXTREME_ANY <- TRUE %in% OUTLIERS_EXTREME$is.extreme
 #
 NORMALITY_VIOLATION <- DATA %>% dplyr::group_by(T) %>% rstatix::shapiro_test(Y)
                     NORMALITY_VIOLATION <- TRUE %in% (NORMALITY_VIOLATION$p < 0.050)
 #
 SPHERICITY <- ez::ezANOVA(data = DATA, dv = Y, wid = O, within = T, between = NULL, type = 3, detailed = TRUE)
            SPHERICITY<- SPHERICITY$"Mauchly's Test for Sphericity"
            SPHERICITY <- (SPHERICITY$p < 0.050)
 #
 if (wise & NORMALITY_VIOLATION) { return(my_paired_friedman(y = y, time = time, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction)) }
 #
 note <- ''
      if (NORMALITY_VIOLATION) { note <- '!not-applicable! ' }
      if (OUTLIERS_EXTREME_ANY) { note <- paste(note, '!Extreme-outliers: ', OUTLIERS_EXTREME_N,'! ', sep = '') }
      if (SPHERICITY) { note <- paste(note, '!Sphericity-correction! ', sep = '') }
 #
 MODEL <- nlme::lme(Y ~ T, data = DATA, random = ~ 1 | O)
 ANOVA <- ez::ezANOVA(data = DATA, dv = Y, wid = O, within = T, between = NULL, type = 3, detailed = TRUE)
 if (SPHERICITY)
 {
  ANOVA$ANOVA[2] <- ANOVA$'Sphericity Corrections'$'p[HF]'
 }
 ANOVA <- ANOVA$ANOVA
 #
 result <- paste(note, 
                 'F','(', ANOVA$DFn[2], ',', ANOVA$DFd[2], ')', my_nice(ANOVA$F[2], decimals = 2, text = '', with_equal_sign = TRUE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string), ',', ' ', 
                 myFunctions::give_nice_p(ANOVA$p[2], decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                 sep = '')
 #
 p_value <- as.numeric(ANOVA$p[2])
 if (p_value < alpha_value) { significance <- '*' }
 #
 if (p_value < alpha_value)
 {
  POST <- MODEL
       POST <- summary(multcomp::glht(POST, linfct = multcomp::mcp(T = 'Tukey')), test = multcomp::adjusted(type = 'bonferroni'))
       POST <- data.frame(n = names(POST$test$pvalues),
                          d = POST$test$coefficients,
                          p = POST$test$pvalues)
  times_pairs <- POST$n
  times_pairs_p <- POST$p
  ROWS <- c()
  for (ROW in row.names(POST))
  {
   SIGN <- '='
   if (POST[ROW, c('p')] < 0.050)
   {
    if (POST[ROW, c('d')] < 0) { SIGN <- '<' }
    if (POST[ROW, c('d')] > 0) { SIGN <- '>' }
   } else { SIGN <- '=' }
  ROW <- paste(gsub(' - ', paste(' ', SIGN, ' ', sep = ''), ROW),
                    ' ', '(', my_nice_p(POST[ROW, c('p')], decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string), ')', sep = '')
  ROW <- gsub('§§§', '-', ROW)
  ROWS <- c(ROWS, ROW)
  }
  comparison <- paste(ROWS, collapse = paste(';', ' ', sep = ''))
 } else
 {
  POST <- MODEL
       POST <- summary(multcomp::glht(POST, linfct = multcomp::mcp(T = 'Tukey')), test = multcomp::adjusted(type = 'bonferroni')) 
       POST <- data.frame(n = names(POST$test$pvalues),
                          d = POST$test$coefficients,
                          p = POST$test$pvalues)
  times_pairs <- POST$n
  times_pairs_p <- POST$p
  comparison <- paste(levels(DATA$T), collapse = paste(' ', '=', ' ', sep = ''))
 }
 comparison <- gsub('§§§', '-', comparison)
 #
 if (p_value < alpha_value)
 {
  # ANOVA$ges[2]
  # ES <- effectsize::eta_squared(MODEL, partial = TRUE, alternative = test_direction, ci = 0.950, generalized = FALSE)
  ES <- effectsize::omega_squared(MODEL, partial = TRUE, alternative = test_direction, ci = 0.950, generalized = FALSE)
  effect_size <- paste(my_nice(ES$Omega2, decimals = 3, text = "\u03C9\u00B2\u209A", with_equal_sign = TRUE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ' ', '[', my_nice(ES$CI_low, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string),
                       ',', ' ', my_nice(ES$CI_high, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string), ']',
                       sep = '')
  effect_size_interpretation <- ''
                             if (!is.na(ES$Omega2) & (abs(ES$Omega2) <= 0.02)) { effect_size_interpretation <- paste(',', ' ', 'negligible effect', sep = '') }
                             if (!is.na(ES$Omega2) & (abs(ES$Omega2)  > 0.02) & (abs(ES$Omega2) <= 0.13)) { effect_size_interpretation <- paste(',', ' ', 'small effect', sep = '') }
                             if (!is.na(ES$Omega2) & (abs(ES$Omega2)  > 0.13) & (abs(ES$Omega2) <= 0.26)) { effect_size_interpretation <- paste(',', ' ', 'moderate effect', sep = '') }
                             if (!is.na(ES$Omega2) & (abs(ES$Omega2)  > 0.26)) { effect_size_interpretation <- paste(',', ' ', 'large effect', sep = '') }
  effect_size <- paste(effect_size, effect_size_interpretation, sep = '')
 }
 #
 times_description  <- c()
 for (level_a in levels(DATA$T))
 {
  inside_time <- as.data.frame(psych::describeBy(DATA$Y, DATA$T, mat = TRUE)[, c('group1', 'mean', 'sd')])
  inside_time$group1 <- paste(inside_time$group1, ':', sep = '')
  inside_time$mean <- sapply(inside_time$mean, my_nice, decimals = 2, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -1000, max_value = 1000, void_string = void_string)
  inside_time$sd <- paste('\u00B1', sapply(inside_time$sd, my_nice, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string), sep = '')
  inside_time <- apply(inside_time, 1, paste, collapse = ' ')
 }
 times_description <- paste(inside_time, collapse = paste(' ', '-vs-', ' ', sep = ''))
 times_description <- paste(c(times_description, empty_levels), collapse = paste(';', ' ', sep = ''))
 times_description <- gsub('§§§', '-', times_description)
 #
 times_pairs <- strsplit(times_pairs, split = " - ")
 times_pairs_p <- times_pairs_p
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, times = times_description, times_pairs = times_pairs, times_pairs_p = times_pairs_p)
 return(RESULTS)
}

#
