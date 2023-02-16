#' my_in_categorical_variable
#'
#' Function to insert a categorical variable, creating dummy variables (one for each level).
#'
#' @param name_in Name of the variable in the input data.frame. String. Default: None.
#' @param df_in Input data.frame (with the 'name_in' variable). Data.frame. Default: IN.
#' @param levels_in_to_exclude Levels to be excluded. String vector. Default: c('NA').
#' @param name_out Name for the variable in the resulting data.frame (if absent, 'name_in' is used). String. Default: ''.
#' @param levels_out Levels to be used in the in the resulting data.frame (should be in the same number and order as the original levels; if absent, original levels are used). String vector. Default: c().
#' @return A data.frame with the main variable and associated dummy variables.
#' @export
my_in_categorical_variable <- function (name_in, df_in = IN, levels_in_to_exclude = c('NA'), name_out = '', levels_out = c())
{
 if (is.na(name_out) | name_out == '')
 {
  name_out <- name_in
 }
 #
 OUT <- data.frame(out = ordered(as.character(df_in[, c(name_in)]), exclude = levels_in_to_exclude))
 #
 if (is.null(levels_out))
 {
  levels_out <- levels(OUT$out)
 }
 levels(OUT$out) <- levels_out
 Hmisc::label(OUT$out) <- name_out
 #
 if (length(levels_out) > 1)
 {
  contrasts(OUT$out) <- contr.treatment(length(levels_out), base = 1)
  for (enne in c(1:length(levels(OUT$out))))
  {
   OUT[, c(paste(name_out, '___d', enne, sep = ''))] <- ordered(OUT$out == levels(OUT$out)[enne], levels = c(FALSE, TRUE))
                                                        levels(OUT[, c(enne + 1)]) <- c('No', 'Yes')
                                                        contrasts(OUT[, c(enne + 1)]) <- contr.treatment(2, base = 1)
  }
 }
 #
 names(OUT)[1] <- name_out
 return(OUT)
}

#