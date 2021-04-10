#' helper function
#' @name plot_q6_2
#' @description function to prepare the variables of interest
#' @param country plot family members or friends who live in colombia
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q6_2 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q6_2')

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
    DiasporaSurveyResults::count_multiple('q6_2')

  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

  # data_labels %>%
  #   stringr::str_remove(' of the above|\\(i\\.e\\..*') %>%
  #   stringr::str_replace('None', 'No one') %>%



  # # reorder labels
  # data_labels <- c(
  #   'None of the above',
  #   'Some of my wider family (i.e. grandparents, cousins etc.)',
  #   'Some of my friends',
  #   'Most of my friends',
  #   'One or more of my siblings',
  #   'One or both of my parents',
  #   'Most of my wider family (i.e. grandparents, cousins etc.)'
  # )
  # data_labels <- data_labels[c(6,5,7,2,4,3,1)]

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
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 20)}) +
    labs(x = element_blank(), y = paste0('Respondents: ', samples)) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(), panel.border = element_blank(),
      legend.position = 'none', axis.ticks.x = element_blank(),
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
      text = element_text(size = 18)
    )

  # return result
  return(p)
}
