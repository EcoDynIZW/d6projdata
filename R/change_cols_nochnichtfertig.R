








change_cols <- function(x){
  data <- x

  data <- data %>%
    mutate(names(data)[utils::menu(names(data), title = "which column do you want to change?")])

}
