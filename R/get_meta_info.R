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
               pattern = "project_data_",
               full.names = TRUE), sheetIndex = 1)
    text <- data %>%
      dplyr::select(proj_id, proj_editor, Dep, sup_vis, species,
                    type_of_sample, type_of_data, type_of_measure,
                    no_spec, no_ind, publication, publication_doi) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      tidyr::pivot_longer(cols = c("proj_id", "proj_editor", "Dep", "sup_vis",
                                   "species", "type_of_sample", "type_of_data",
                                   "type_of_measure", "no_spec", "no_ind",
                                   "publication", "publication_doi"),
                          names_to = "column", values_to = "input") %>%

      flextable::flextable() %>% flextable::autofit()

    return(text)
}
