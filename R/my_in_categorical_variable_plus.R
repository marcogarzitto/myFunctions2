#' my_in_categorical_variable_plus
#'
#' Function to insert a categorical variable, creating dummy variables (one for each level) and variables with any combined levels.
#' <<< Experimental! >>>
#' 
#' @param name_in Name of the variable in the input data.frame. String. Default: None.
#' @param df_in Input data.frame (with the 'name_in' variable). Data.frame. Default: IN.
#' @param levels_in_to_exclude Levels to be excluded. String vector. Default: c('NA').
#' @param name_out Name for the variable in the resulting data.frame (if absent, 'name_in' is used). String. Default: ''.
#' @param levels_out Levels to be used in the in the resulting data.frame (should be in the same number and order as the original levels; if absent, original levels are used). String vector. Default: c().
#' @return A data.frame with the main variable and associated dummy variables.
#' @export
my_in_categorical_variable_plus <- function (name_in, df_in = IN, levels_in_to_exclude = c('NA'), name_out = '', levels_out = c())
{
 OUT <- my_in_categorical_variable(name_in = name_in, df_in = df_in, levels_in_to_exclude = levels_in_to_exclude, name_out = name_out, levels_out = levels_out)
 OUT <- OUT[1]
 levels <- levels(OUT[, c(1)])
 if (length(levels) > 2)
 {
  combinations <- combn(levels, 2)
  added <- c()
  for (enne in c(1:dim(combinations)[2]))
  {
   temp <- levels
   temp[temp == combinations[1, enne]] <- apply(combinations, 2, paste, collapse = ' or ')[enne]
   temp[temp == combinations[2, enne]] <- apply(combinations, 2, paste, collapse = ' or ')[enne]
   print(temp)
   name <- paste(names(OUT)[1], '___c', enne, sep = '')
   OUT[, c(name)] <- OUT[, c(1)]
   levels(OUT[, c(name)]) <- temp
   added <- c(added, name)
  }
  for (variable in added)
  {
   if (length(levels(OUT[, c(variable)])) > 2)
   {
    OUT <- cbind(OUT, my_in_categorical_variable_plus(name_in = variable, df_in = OUT)[, -c(1)])
   }
  }
 }
 OUT <- cbind(my_in_categorical_variable(name_in = name_in, df_in = df_in, levels_in_to_exclude = levels_in_to_exclude, name_out = name_out, levels_out = levels_out),
              OUT[, -c(1)])
 return(OUT)
}

#
