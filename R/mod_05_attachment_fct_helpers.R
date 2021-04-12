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

  # clean and extract labels
  data_prepared <- data_prepared %>%
    dplyr:::mutate(
      question = stringr::str_remove(.data$question, ' \\(.*\\)')
    )

  # create labels
  data_labels <- c(
    'None of the above',
    'Some of my wider family',
    'Some of my friends',
    'Most of my friends',
    'One or more of my siblings',
    'One or both of my parents',
    'Most of my wider family'
  )

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

#' helper function
#' @name plot_q6_3
#' @description how often respondents visit Colombia?
#' @param country filter by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q6_3 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q6_3')

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
    DiasporaSurveyResults::count_unique('q6_3')

  # clean and extract labels
  data_prepared <- data_prepared %>%
    dplyr:::mutate(
      question = stringr::str_remove_all(
        .data$question,
        paste0(
          ' \\(|\\)|',
          'Does not apply|my home country since I left/ I have never visited '
        )
      )
    )

  # create labels
  data_labels <- c(
    'I have not been abroad for that long',
    'I have not visited Colombia',
    'Once a year',
    'Once every two years',
    'Once in every two to five years',
    'Less than once every five years',
    'Twice a year or more'
  )

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

#' helper function
#' @name plot_q6_5
#' @description how often respondents talk to people in Colombia?
#' @param country filter by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q6_5 <- function(country = NULL){

  # filter data
  data <- DiasporaSurveyResults::prepare_data('q6_5')

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
    DiasporaSurveyResults::count_unique('q6_5')

  # create labels
  data_labels <- c(
    'Every day',
    'At least once a week, but not every day',
    'At least once a month, but not every week',
    'At least once in six months, but not every month',
    'Less than once in six months'
  )

  # assign labels
  data_final <- data_prepared %>%
    dplyr::mutate(question = ordered(.data$question, levels = rev(data_labels)))

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

#' helper function
#' @name plot_q6_6
#' @description do respondents have pension funds in Colombia?
#' @param country filter by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q6_6 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q6_6')

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
    dplyr::filter(!stringr::str_detect(.data$q6_6, '^(Other)')) %>%
    DiasporaSurveyResults::count_unique('q6_6') %>%
    dplyr::mutate(
      question = dplyr::case_when(
        stringr::str_detect(.data$question, '^(No)') ~ 'No',
        stringr::str_detect(.data$question, '^(Yes)') ~ 'Yes',
        TRUE ~ 'I don\'t know',
      )
    )

  # create labels
  data_labels <- c('I don\'t know', 'No', 'Yes')

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

#' helper function
#' @name plot_q6_7
#' @description do respondents own property in Colombia?
#' @param country filter by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q6_7 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q6_7')
  country <- 'Belgium'
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
    DiasporaSurveyResults::count_multiple('q6_7')

  data_prepared$question %>% unique() %>% sort()
  # create labels
  data_labels <- c(
    'I do not own property in Colombia',
    'I don\'t, but I wish to purchase property in the near future',
    'I don\'t, but my parents/grandparents do',
    'Yes, I own property for business reasons',
    'Yes, I own property for personal reasons'
  )

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
