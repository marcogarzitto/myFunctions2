#' my_save_table
#'
#' Function to save a data-frame in 'xlsx' and/or 'RData' and/or 'csv'. Using: 'xlsx' package.
#'
#' @param what .
#' @param file_name Name by which the file will be saved. String. Default: 'DF'.
#' @param where Destination folder. String. Default: current folder (with 'getwd()').
#' @param row_names Use of row names. Bolean. Default: FALSE.
#' @param with_rdata Save as 'RData'. Bolean. Default: TRUE.
#' @param with_csv Save as 'csv' (both standard and Italian-version 'csv' are saved). Bolean. Default: FALSE.
#' @param fileEncoding File encoding. String. Default: 'latin1'.
#' @return Nothing.
#' @export
my_save_table <- function (what, file_name = 'DF', where = getwd(), row_names = FALSE, with_rdata = TRUE, with_csv = FALSE, fileEncoding = 'latin1')
{
 xlsx::write.xlsx(what, file = paste(where, '/', file_name, '.xlsx', sep = ''), sheetName = file_name, row.names = row_names, showNA = FALSE)
 if (with_rdata)
 {
  save(what, file = paste(where, '/', file_name, '.RData', sep = ''))
 }
 if (with_csv)
 {
  write.table(what, file = paste(where, '/', file_name, '.csv', sep = ''), sep = ',', dec = '.', row.names = row_names, quote = TRUE, fileEncoding = fileEncoding)
  write.table(what, file = paste(where, '/', file_name, '_IT', '.csv', sep = ''), sep = ';', dec = ',', row.names = row_names, quote = TRUE, fileEncoding = fileEncoding)  
 }
}

#
