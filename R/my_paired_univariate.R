#' my_paired_univariate
#'
#' Function to do an appropriate univariate withween-subjects analysis (with two variable).
#'
#' @param dv Numeric/Factor vector. Dependent variable (using long format for data-frame). Default: None.
#' @param iv Numeric/Factor vector. Independent/Time variable (using long format for data-frame). Default: None.
#' @param observations Factor vector, with the observation identifier (using long format for data-frame). Default: None.
#' @param void_string String to be used if the number cannot be represented correctly. String. Default: '-'.
#' @param alpha_value Statistical significance. Numeric value. Default: 0.050.
#' @param multiple_alphas Numeric vector with three levels of statistical significance (for multiple asterisks). Numeric vector. Default: c(0.050, 0.010, 0.001).
#' @param wise Boolean, if true, the most appropriate test is used according to the data. Default: TRUE
#' @param direction Specifying the alternative hypothesis (using: 'Stable', 'Increase', 'Decrease'). String. Default: 'Stable'.
#' @return A list with results: 'test' (string, with results of the selected test), 'p_value' (numeric, the value of p associated with the test), 'significance' (string, with an asterisk for statistically significant results), 'comparison' (string, comparisons between levels of the time variable marked when the result is statistically significant), 'es' (string, effect-size for statistically significant results), 'times' (string, mean and SD for the levels of the time variable), 'times_pairs' (list of vectors, every vector reports two times corresponding to post-hoc comparisons), 'times_pairs_p' (vector of numeric, significances corresponding to post-hoc comparisons).
#' @export
my_paired_univariate <- function (dv, iv, observations, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), wise = TRUE, direction = 'Stable')
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
 RESULTS <- list(test = result, p_value = p_value, significance = significance, comparison = comparison, es = effect_size, times = times_description, times_pairs = times_pairs, times_pairs_p = times_pairs_p)
 #
 DATA <- na.omit(data.frame(DV = dv, IV = iv))
 dv_classification <- my_variable_classification(variable = 'DV', df = DATA)
 iv_classification <- my_variable_classification(variable = 'IV', df = DATA)
 #
 if ((dv_classification == 'Dichotomous') | (dv_classification == 'Polytomous')) { DATA$DV <- droplevels(DATA$DV) }
 if ((iv_classification == 'Dichotomous') | (iv_classification == 'Polytomous')) { DATA$IV <- droplevels(DATA$IV) }
 # 
 if ((dv_classification == 'Unknown') |
     (dv_classification == 'Empty') |
     (dv_classification == 'Empty continuous') |
     (dv_classification == 'Empty categorical') |
     (dv_classification == 'High polytomous') |
     (dv_classification == 'Date') |
     (iv_classification == 'Unknown') |
     (iv_classification == 'Empty') |
     (iv_classification == 'Empty continuous') |
     (iv_classification == 'Empty categorical') |
     (iv_classification == 'High polytomous') |
     (iv_classification == 'Date'))
 {
  RESULTS <- RESULTS
 }
 #
 if ((dv_classification == 'Continuous') |
     (dv_classification == 'Poor continuous') |
     (dv_classification == 'Dummy'))
 {
  if (iv_classification == 'Continuous') { RESULTS <- my_paired_pearson_r(y = dv, x = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Poor continuous') { RESULTS <- my_paired_pearson_r(y = dv, x = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Dummy') { RESULTS <- my_paired_pearson_r(y = dv, x = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Dichotomous') { RESULTS <- my_paired_ttest(y = dv, time = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Polytomous') { RESULTS <- my_paired_anova(y = dv, time = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
 }
 #
 if ((dv_classification == 'Dichotomous'))
 {
  if (iv_classification == 'Continuous') { RESULTS <- my_paired_ttest(y = iv, time = dv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Poor continuous') { RESULTS <- my_paired_ttest(y = iv, time = dv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Dummy') { RESULTS <- my_paired_ttest(y = iv, time = dv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Dichotomous') { RESULTS <- my_paired_mcnemar(a = dv, b = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, direction = direction) }
  if (iv_classification == 'Polytomous') { RESULTS <- my_paired_chisquare(a = dv, b = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
 }
 #
 if ((dv_classification == 'Polytomous'))
 {
  if (iv_classification == 'Continuous') { RESULTS <- my_paired_anova(y = iv, time = dv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Poor continuous') { RESULTS <- my_paired_anova(y = iv, time = dv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Dummy') { RESULTS <- my_paired_anova(y = iv, time = dv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Dichotomous') { RESULTS <- my_paired_mcnemar(a = dv, b = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
  if (iv_classification == 'Polytomous') { RESULTS <- my_paired_chisquare(a = dv, b = iv, observations = observations, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas, wise = wise, direction = direction) }
 }
 #
 return(RESULTS)
}

#
