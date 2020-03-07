#' load_stock_data
#' @description loads stock data for use in program
#' @return the stock data
#' @importFrom system file
#' @export
load_stock_data <- function(){
  target_path = system.file('extdata', 'stock_data.csv', package = 'StepRegression')
  return(read_csv(target_path))
}

#' stock_data
#' @docType data
#' @author Christopher Pearson
#' @note located in /inst/extdata/stock_data.rda
#' @references Data originally retrieved from https://www.kaggle.com/dgawlik/nyse
#' @keywords data
'stock_data'

