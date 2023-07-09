#' my_in_continuous_variable
#'
#' Function to insert a continuous variable from a data-frame into another data-frame.
#'
#' @param name_in Name of the variable in the input data-frame. String. Default: None.
#' @param df_in Input data-frame (with the 'name_in' variable). Data-frame. Default: IN.
#' @param name_out Name for the variable in the resulting data-frame (if absent, 'name_in' is used). String. Default: ''.
#' @param df_out Output data-frame (possible without the 'name_out' variable). Data-frame. Default: DF.
#' @param label_out Label (using 'Hmisc::label()') for the variable in the resulting data-frame (if absent, 'Hmisc:label()' of 'name_in' is used, possibly set to 'name_in'). String. Default: ''.
#' @return A data-frame with the main variable (column-binded to 'df_out').
#' @export
my_in_continuous_variable <- function (name_in, df_in = IN, name_out = '', df_out = DF, label_out = '')
{
 if(Hmisc::label(df_in[, c(name_in)]) == '')
 {
  Hmisc::label(df_in[, c(name_in)]) <- name_in
 }
 if (is.na(name_out) | name_out == '')
 {
  name_out <- name_in
 }
 if (is.na(label_out) | label_out == '')
 {
  label_out <- Hmisc::label(df_in[, c(name_in)])
 }
 #
 OUT <- data.frame(out = as.numeric(as.character(df_in[, c(name_in)])))
 #
 Hmisc::label(OUT$out) <- label_out
 #
 names(OUT)[1] <- name_out
 OUT <- cbind(df_out, OUT)
 return(OUT)
}

#
