#' get meta info for overview plot
#' @param path where to find the xlsx meta file
#' @return a text file for ggplot
#' @export
#' @examples
#' \dontrun{
#'
#' }



get_meta_info <- function(path){
  data <- xlsx::read.xlsx(
    base::list.files(path,
               pattern = ".xlsx",
               full.names = TRUE),
    sheetIndex = 1)
  text <- base::paste0("\n  Project ID          : ", data$proj_id, "\n",
                 "    Project Editor      : ", data$proj_editor, "\n",
                 "    Department          : ", data$Dep, "\n",
                 "    Super Visor         : ", data$sup_vis, "\n",
                 "    Species             : ", data$species, "\n",
                 "    Type of sample      : ", data$type_of_sample, "\n",
                 "    Type of data        : ", data$type_of_data, "\n",
                 "    Type of mearuse     : ", data$type_of_measure, "\n",
                 "    # of species        : ", data$no_spec, "\n",
                 "    # of individuals    : ", data$no_ind, "\n",
                 "    publication name    : ", data$proj_editor, "\n",
                 "    doi                 : ", data$publication_doi, "\n"
                 )


  return(text)
}

# text = paste("\n   The following is text that'll appear in a plot window.\n",
#              "       As you can see, it's in the plot window\n",
#              "       One might imagine useful information here")
# ggplot() +
#   annotate("text", x = 4, y = 25, size=8, label = text) +
#   theme_void()
#
#
#
# path <- "C:/Users/wenzler/PopDynIZW Dropbox/Lab_Orga/D6_PopDynTeam/ProjectData/data-raw/2008_vulpes_vulpes_de_b_individual_gras"
