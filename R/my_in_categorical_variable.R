#' my_in_categorical_variable
#'
#' Function to insert a categorical variable from a data-frame into another data-frame, creating dummy variables (one for each level).
#'
#' @param name_in Name of the variable in the input data-frame. String. Default: None.
#' @param df_in Input data-frame (with the 'name_in' variable). Data-frame. Default: IN.
#' @param levels_in Levels to be used in the in the original variable (should be in the desired order; if absent, available levels are used). String vector. Default: c().
#' @param levels_in_to_exclude Levels to be excluded. String vector. Default: c('NA', NA).
#' @param name_out Name for the variable in the resulting data-frame (if absent, 'name_in' is used). String. Default: ''.
#' @param df_out Output data-frame (possibly without the 'name_out' variable). Data-frame. Default: DF.
#' @param label_out Label (using 'Hmisc::label()') for the variable in the resulting data-frame (if absent, 'Hmisc:label()' of 'name_in' is used, possibly set to 'name_in'). String. Default: ''.
#' @param levels_out Levels to be used in the in the resulting data-frame (should correspond to those indicated in 'levels_in'; if absent, original levels are used). String vector. Default: c().
#' @return A data-frame with the main variable and associated dummy variables (column-binded to 'df_out').
#' @export
my_in_categorical_variable <- function (name_in, df_in = IN, levels_in = c(), levels_in_to_exclude = c('NA', NA), name_out = '', df_out = DF, label_out = '', levels_out = c())
{
 IN <- df_in[, c(name_in)]
 if (is.null(levels_in))
 {
  levels_in <- levels(ordered(as.character(IN)))
 }
 if (Hmisc::label(IN) == '')
 {
  Hmisc::label(IN) <- name_in
 }
 if (is.na(name_out) | name_out == '')
 {
  name_out <- name_in
 }
 if (is.na(label_out) | label_out == '')
 {
  label_out <- Hmisc::label(IN)
 }
 #
 OUT <- data.frame(out = ordered(as.character(IN), levels = levels_in, exclude = levels_in_to_exclude))
 #
 if (is.null(levels_out))
 {
  levels_out <- levels(OUT$out)
 }
 levels(OUT$out) <- levels_out
 Hmisc::label(OUT$out) <- label_out
 #
 levels_out <- unique(levels_out)
 #
 if (length(levels_out) > 1)
 {
  contrasts(OUT$out) <- contr.treatment(length(levels_out), base = 1)
  for (enne in c(1:length(levels(OUT$out))))
  {
   OUT[, c(paste(name_out, '___d', enne, sep = ''))] <- ordered(OUT$out == levels(OUT$out)[enne], levels = c(FALSE, TRUE))
                                                        levels(OUT[, c(enne + 1)]) <- c('No', 'Yes')
                                                        contrasts(OUT[, c(enne + 1)]) <- contr.treatment(2, base = 1)
                                                        Hmisc::label(OUT[, c(enne + 1)]) <- paste(label_out, ':', ' ', levels_out[enne], sep = '')
  }
 }
 #
 names(OUT)[1] <- name_out
 OUT <- cbind(df_out, OUT)
 return(OUT)
}

#
