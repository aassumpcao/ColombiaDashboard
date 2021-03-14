#' helper function
#' @name prepare_data
#' @description function to prepare the variables of interest
#' @param question_analysis the main variable in the analysis
#' @param filter the variable to filter the data by
#' @param extra_filter an additional filter passed onto the dataset
#' @export
prepare_data <- function(
  question_analysis = NULL, filter = c('q3_2' = NULL),
  extra_filter = c(NULL = NULL)
){

  # load the data
  data <- DiasporaSurveyResults::survey_data

  # extra variable and value for filter
  filter_question <- names(filter)
  filter_selected <- unname(filter)

  # extra variable and value for filter
  extrafilter_question <- names(extra_filter)
  extrafilter_selected <- unname(extra_filter)

  # filter data using appropriate question and value
  if (!is.null(filter)) {
    data <- data %>%
      dplyr::filter(!!as.name(filter_question) == filter_selected)
  }

  # select variable
  prepared_data <- data %>%
    # dplyr::select(!!as.name(question_analysis)) %>%
    dplyr::filter(!is.na(!!as.name(question_analysis)))

  # apply additional fitler
  if (!is.null(extrafilter_selected)){
    prepared_data <- prepared_data %>%
      dplyr::filter(!is.na(!!as.name(extrafilter_question))) %>%
      dplyr::filter(!!as.name(extrafilter_question) == extrafilter_selected)
  }

  # return prepared data
  return(prepared_data)
}

#' helper function
#' @name count_unique
#' @description function to count the values of single-choice questions
#' @param data a dataset
#' @param question_analysis the main variable in the analysis
#' @param group the variable to use as the grouping variable
#' @param sort the variable to use as the sorting variable
#' @importFrom rlang .data
#' @export
count_unique <- function(data, question_analysis, group = NULL, sort = TRUE){

  # count data and create labels
  if (is.null(group)) {
  prepared_data <- data %>%
    dplyr::filter(!is.na(!!as.name(question_analysis))) %>%
    dplyr::count(!!as.name(question_analysis))
  } else {
  prepared_data <- data %>%
    dplyr::filter(
      !is.na(!!as.name(question_analysis)) & !is.na(!!as.name(group))
    ) %>%
    dplyr::count(!!as.name(question_analysis), !!as.name(group))
  }

  # transform counts for graph
  prepared_data <- prepared_data %>%
    dplyr::mutate(
      abs_total = .data$n,
      abs_label = !!as.name(question_analysis),
      perc_total = (.data$abs_total / sum(.data$abs_total)) * 100,
      perc_label = paste0(round(.data$perc_total, 2), '%')
    ) %>%
    dplyr::select(-.data$n)

  # apply sort if requested by user
  if (sort) {
    prepared_data <- prepared_data %>%
      dplyr::arrange(dplyr::desc(.data$perc_total))
  }

  # return prepared data
  return(prepared_data)
}

#' helper function
#' @name extract_labels
#' @description function to count the variables of interest
#' @param data a dataset
#' @param group the variable to use as the grouping variable
#' @importFrom rlang .data
#' @export
extract_labels <- function(data, group = NULL){

  # apply group filter if assigned
  if (!is.null(group)){

    # prepare data
    data <- data %>%
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

#' helper function
#' @name split_data
#' @description function to count the variables of interest
#' @param data the dataset to work on
#' @importFrom rlang .data
#' @export
split_data <- function(data){}#' helper function

# data <- DiasporaSurveyResults::survey_data

# data <- DiasporaSurveyResults::prepare_data(data)
