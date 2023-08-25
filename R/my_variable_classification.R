#' my_variable_classification
#'
#' Function to classify a variable (for further analyses).
#'
#' @param A variable of a data-frame to classify. String. Default: ''.
#' @param A a data.frame (where the variable to classify is). Data-frame. Default: DF.
#' @return A string: 'Unknown', 'Continuous', 'Poor continuous', 'Dummy', 'Empty continuous', 'Dichotomous', 'Polytomous', 'High polytomous', 'Empty categorical', 'Date', 'Empty'
#' @export
my_variable_classification <- function (variable = '', df = DF)
{
 Y <- df[, c(variable)]
 OUT <- 'Unknown'
 #
 if (is.numeric(Y)) { OUT <- 'Continuous' }
 if (is.numeric(Y) & (length(levels(as.factor(as.character(Y)))) < 5)) { OUT <- 'Poor continuous' }
 if (is.numeric(Y) & (length(levels(as.factor(as.character(Y)))) == 2)) { OUT <- 'Dummy' }
 if (is.numeric(Y) & (length(levels(as.factor(as.character(Y)))) == 1)) { OUT <- 'Empty continuous' }
 if (is.factor(Y) & (length(levels(ordered(as.character(Y)))) == 2) ) { OUT <- 'Dichotomous' }
 if (is.factor(Y) & (length(levels(ordered(as.character(Y)))) > 2)) { OUT <- 'Polytomous' }
 if (is.factor(Y) & (length(levels(ordered(as.character(Y)))) > 10)) { OUT <- 'High polytomous' }
 if (is.factor(Y) & (length(levels(ordered(as.character(Y)))) == 1)) { OUT <- 'Empty categorical' }
 if (!is.numeric(Y) & (sum(paste(sapply(Y, substr, 6, 6), sapply(Y, substr, 3, 3), sep = '') == '..') == sum(!is.na(Y)))) { OUT <- 'Date' }
 if (inherits(Y, 'Date')) { OUT <- 'Date' }
 if (sum(!is.na(Y)) == 0) { OUT <- 'Empty' }
 return(OUT)
}

#
