
#' helper function
#' @name plot_q5_3
#' @description function to prepare the variables of interest
#' @param country plot time of residency
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q5_3 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q5_3')

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # isolate the number of variables
  samples <- nrow(data)

  # prepare data for plot
  data_prepared <- data %>%
    DiasporaSurveyResults::count_unique('q5_3')

  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

  # reorder labels
  data_labels <- data_labels[c(2,5,3,4,6,1)]

  # assign labels
  data_final <- data_prepared %>%
    dplyr::mutate(question = ordered(.data$question, levels = data_labels))

  # set plot data
  p <- ggplot(
    data_final,
    aes(x = .data$question, y = .data$abs_total, fill = .data$question)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .8) +
    geom_text(
      aes(label = .data$abs_total, y = .075*max(.data$abs_total), size = 12),
      alpha = 1
    ) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 10)}) +
    labs(y = element_blank(), x = paste0('Respondents: ', samples)) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
      axis.ticks.y = element_blank(), panel.border = element_blank(),
      legend.position = 'none', axis.ticks.x = element_blank(),
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
      text = element_text(size = 18)
    )

  # return result
  return(p)
}
