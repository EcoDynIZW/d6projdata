#' create xlsx for the project data.
#' @param data_name the name of the project
#' @param data_sf sf object
#' @return a tibble where you have to set the parameters by hand.
#' @export
#' @examples
#' \dontrun{
#' build_data_files()
#' }


build_data_files <- function(data_name, data_sf){

  data <- dplyr::tibble(proj_id = NA,
                        proj_name = NA,
                        proj_editor = NA,
                        Dep = NA,
                        sup_vis = NA,
                        data_file_name = NA,
                        data_name_archive = NA,
                        species = NA,
                        study_country = NA,
                        study_area = NA,
                        EPSG_code = NA,
                        projection = NA,
                        data_from = NA,
                        data_to = NA,
                        edited_unique_identifier = NA,
                        original_unique_identifier = NA,
                        storage_folder = NA,
                        type_of_sample = NA,
                        type_of_data = NA,
                        type_of_measure = NA,
                        repeated_measure = NA,
                        no_spec = NA,
                        no_ind = NA,
                        additional_info = NA,
                        publication = NA,
                        publication_doi = NA,
                        metadata_description = NA,
                        temporal_resolution_sec = NA,
                        temporal_resolution_min = NA,
                        IZW_archive_path = NA,
                        IZW_data_path = NA,
                        external_archives = NA,
                        sample_data = NA,
                        proc_data = NA,
                        cooperation = NA,
                        data_owner = NA,
                        data_type = NA)

  doit <- "Yes"

  while(doit == "Yes"){

    data <- data %>%
      dplyr::mutate(
        proj_name = data_name,
        proj_id = ifelse(is.na(data$proj_id), base::readline("project id:"), data$proj_id),
        proj_editor = unlist(stringi::stri_split(data_name, regex = "_"))[5],
        Dep = ifelse(is.na(data$Dep), fun_num("Department"), data$Dep),
        sup_vis = ifelse(is.na(data$sup_vis), base::readline("super visor:"), data$sup_vis),
        data_file_name = ifelse(is.na(data$data_file_name), base::readline("names of data files:"),data$data_file_name),
        data_name_archive = ifelse(is.na(data$data_name_archive), base::readline("original names of data files in archive:"), data$data_name_archive),
        edited_unique_identifier = ifelse(is.na(data$edited_unique_identifier), base::readline("edited unique identifier:"), data$edited_unique_identifier),
        original_unique_identifier = ifelse(is.na(data$original_unique_identifier), base::readline("original unique identifier:"), data$original_unique_identifier),
        study_country = ifelse(is.na(data$study_country), base::readline("study country:"), data$study_country),
        study_area = ifelse(is.na(data$study_area), base::tolower(base::readline("study area:")), data$study_area),
        species = unlist(stringi::stri_split(data_name, regex = "_"))[2],
        type_of_sample = ifelse(is.na(data$type_of_sample), base::readline("type of sample:"), data$type_of_sample),
        type_of_data = ifelse(is.na(data$type_of_data), base::readline("type of data:"), data$type_of_data),
        type_of_measure = ifelse(is.na(data$type_of_measure), base::readline("type of measure:"), data$type_of_measure),
        repeated_measure = c("yes", "no")[utils::menu(c("yes", "no"), title = "repeated measure?")],
        no_spec = ifelse(is.na(data$no_spec), fun_num("number of species"), data$no_spec),
        no_ind = ifelse(is.na(data$no_ind), fun_num("number of individuals"), data$no_ind),
        data_from = janitor::excel_numeric_to_date(min(data_sf$timestamp, na.rm = TRUE)),
        data_to = janitor::excel_numeric_to_date(max(data_sf$timestamp, na.rm = TRUE)),
        additional_info = ifelse(is.na(data$additional_info), base::readline("additional information:"), data$additional_info),
        publication = ifelse(is.na(data$publication), base::readline("publication name:"), data$publication),
        publication_doi = ifelse(is.na(data$publication_doi), base::readline("publication doi:"), data$publication_doi),
        metadata_description = ifelse(is.na(data$metadata_description), base::readline("metadata description:"), data$metadata_description),
        temporal_resolution_sec = ifelse(is.na(data$temporal_resolution_sec), fun_num("temporal resolution sec"), data$temporal_resolution_sec),
        temporal_resolution_min = ifelse(is.na(data$temporal_resolution_min), fun_num("temporal resolution min"), data$temporal_resolution_min),
        IZW_archive_path = ifelse(is.na(data$IZW_archive_path), base::readline("IZW archive path:"), data$IZW_archive_path),
        IZW_data_path = ifelse(is.na(data$IZW_data_path), base::readline("IZW data path:"), data$IZW_data_path),
        external_archives = ifelse(is.na(data$external_archives), base::readline("external archives:"), data$external_archives),
        sample_data = ifelse(is.na(data$sample_data), base::readline("sample data:"), data$sample_data),
        proc_data = ifelse(is.na(data$proc_data), base::readline("processed data:"), data$proc_data),
        cooperation = ifelse(is.na(data$cooperation), base::readline("cooperation:"), data$cooperation),
        data_owner = ifelse(is.na(data$data_owner), base::readline("data owner:"), data$data_owner),
        projection = sf::st_crs(data_sf)$proj4string,
        EPSG_code = as.character(sf::st_crs(data_sf)$epsg),
        data_type = ifelse(is.na(data$data_type), base::readline("data type ending:"), data$data_type)
      ) %>%
      dplyr::mutate_each(dplyr::funs(empty_as_na))


  doit <- c("Yes", "No")[utils::menu(c("Yes", "No"), title = "Do you want to change something?")]
  }


openxlsx::write.xlsx(x = data, file = here::here("data-raw", data$proj_name, paste0("meta_data_", data$proj_name, ".xlsx")),
                     overwrite = TRUE)
return(data)
}





#' function for numeric
#' @param x input
#' @return returns a number
#' @examples
#' \dontrun{
#' fun_date("1")
#' }

# function for numeric
fun_num <- function(x){
  num_in <- base::readline(paste0(x, ": "))
  return(num_in)
}

#' function for empty cols
#' @param x input
#' @return if x is "" then it will be converted to NA
#' @examples
#' \dontrun{
#' fun_date("")
#' }

# function for empty cols
empty_as_na <- function(x){
  base::ifelse(base::as.character(x)!="", x, NA)
}

#' function for epsg code
#' @return choose an epsg code or add a costum
#' @examples
#' \dontrun{
#' fun_epsg()
#' }

# function for epsg code
fun_epsg <- function(){
  epsg_in <- c("4326", "3035", "25833", "other")[utils::menu(c("4326", "3035", "25833", "other"), title = "choose epsg?")]
  if(epsg_in %in% "other"){
    epsg_in <- base::readline("enter epsg code: ")
    while(is.na(suppressWarnings(base::as.numeric(epsg_in)))){
      print("wrong format. Enter a numeric value")
      epsg_in <- base::readline("enter epsg code: ")
    }
  }
  return(epsg_in)
}
