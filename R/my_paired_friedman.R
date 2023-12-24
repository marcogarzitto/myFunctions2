#' my_paired_friedman
#'
#' Function to do a withween-subjects Friedman's test.
#'
#' @param y Numeric vector. Dependent variable (using long format for data-frame). Default: None.
#' @param time Factor vector. Independent/Time variable (using long format for data-frame). Default: None.
#' @param observations Factor vector, with the observation identifier (using long format for data-frame). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param wise Boolean, if true, the most appropriate test is used according to the data. Default: TRUE
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the Friedman's test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the time variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'times' (string, mean and SD for the levels of the time variable), 'times_pairs' (list of vectors, every vector reports two times corresponding to post-hoc comparisons), 'times_pairs_p' (vector of numeric, significances corresponding to post-hoc comparisons).
#' @export
my_paired_friedman <- function (y, time, observations, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), wise = TRUE, direction = 'Stable')
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
 








 
 #
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, times = times_description, times_pairs = times_pairs, times_pairs_p = times_pairs_p)
 return(RESULTS)
}

#
