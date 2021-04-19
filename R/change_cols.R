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
  data <- xlsx::read.xlsx(path, sheetIndex = 1)
  change <- base::names(data)[utils::menu(c(paste(base::names(data), base::lapply(data, function(x){utils::head(x, 1)}), sep = " - ")), title = "which column do you want to change?")]
  data[1, change] <- base::readline(paste0(change, ": "))
  doit <- c("Yes", "No")[utils::menu(c("Yes", "No"), title = "change another column?")]
  }
  return(data)
}

#devtools::install()

#dat <- d6projdata::change_cols(path = "C:/Users/wenzler/PopDynIZW Dropbox/Lab_Orga/D6_PopDynTeam/ProjectData/data-raw/2008_vulpes_vulpes_de_b_individual_gras/2008_vulpes_vulpes_de_b_individual_gras.xlsx")
