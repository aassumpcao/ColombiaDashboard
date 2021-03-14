library(magrittr)
library(tidyverse)
library(rlang)

data <- DiasporaSurveyResults::prepare_data('q3_2')
prepared_data <- DiasporaSurveyResults::count_unique(data, 'q3_2', 'q3_3')

group <- 'q3_2'

extract_labels <- function(data, group = NULL){

  # apply group filter if assigned
  if (!is.null(group)) {

    # prepare data
    data <- prepared_data %>%
      dplyr::group_by(!!as.name(group)) %>%
      dplyr::arrange(dplyr::desc(.data$abs_total), .by_group = TRUE)

    # prepare labels
    labels <- data %>%
      dplyr::pull(!!as.name(group)) %>%
      unique()

  } else {

    # extract labels
    labels <- data %>%
      dplyr::pull(1) %>%
      unique()
  }

  # return data and labels
  return(list(data = data, labels = labels))
}


extract_labels(prepared_data, 'q3_3')


labels <- prepared_data %>%
  group_by(q3_3) %>%
  arrange(desc(abs_total), .by_group = TRUE) %>%
  pull(q3_3) %>%
  unique()

devtools::document()


