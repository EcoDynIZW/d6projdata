#' change columns of a dataframe.
#' @param path The path where the data lays
#' @return save the data to the original location
#' @export
#' @examples
#' \dontrun{
#' change_cols(path = ".")
#' }

change_cols <- function(path){
  doit <- "Yes"
  while(doit == "Yes"){
  data <- openxlsx::read.xlsx(path, sheet = 1)
  change <- base::names(data)[utils::menu(c(paste(base::names(data), base::lapply(data, function(x){utils::head(x, 1)}), sep = " - ")), title = "which column do you want to change?")]
  data[1, change] <- base::readline(paste0(change, ": "))
  doit <- c("Yes", "No")[utils::menu(c("Yes", "No"), title = "change another column?")]
  }
  return(data)
}
