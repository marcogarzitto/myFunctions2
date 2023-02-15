#' my_mean_ci
#'
#' Function to obtain the confidence interval of the mean. Using: 'psych' package.
#'
#' @param x Numeric vector.
#' @param confidence Confidence interval desired. Numeric. Default: 0.950.
#' @param distribution z-distributionn or t-distribution. For small or poorly distributed samples, use t-distribution. String. Default: 'z'.
#' @return A vector: c(y, ymin, ymax).
#' @export
my_mean_ci <- function (x, confidence = 0.950, distribution = 'z')
{
 x <- psych::describe(x)
 if (distribution == 'z')
 {
  plus_minus <- (abs(qnorm((1 - confidence) / 2)) * x$se)
 } else if (distribution == 't')
 {
  plus_minus <- (qt(confidence + (1 - confidence) / 2, df = (x$n - 1)) * x$se)
 } else
 {
  plus_minus <- 0
 }
 return(c(y = x$mean,
          ymin = x$mean - plus_minus,
          ymax = x$mean + plus_minus))
}

#
