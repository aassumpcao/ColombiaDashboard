#' helper function
#' @name plot_q3_3
#' @description function to prepare the variables of interest
#' @param country gender breakdown
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q3_3 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q3_3') %>%
    dplyr::mutate(
      q3_3 = stringr::str_replace(
        .data$q3_3, '^(P|O).*', 'Other/Prefer Not to Disclose'
      )
    )

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # isolate the number of variables
  samples <- nrow(data)

  # prepare data
  data_prepared <- DiasporaSurveyResults::count_unique(data, 'q3_3')
  data_labels <- c('Male', 'Female', 'Other/Prefer Not to Disclose')

  # prepare final data
  data_final <- data_prepared %>%
    dplyr::mutate(
      labels = ifelse(
        .data$perc_total >= 5,
        paste0(.data$question, '\n[', .data$perc_label, ']'),
        ''
      )
    )

  # set data for graph
  p <- ggplot(
    data_final,
    aes(x = 1, y = .data$perc_total, fill = .data$question)
  )

  # create graph
  p <- p +
    geom_bar(stat = 'identity', position = 'stack', color = 'black', size = .25) +
    scale_fill_brewer(palette = 'PuRd', direction = -1) +
    geom_text(
      aes(label = .data$labels), position = position_stack(vjust = .5), size = 5
    ) +
    coord_flip() +
    labs(y = paste0('Respondents: ', samples), x = element_blank()) +
    theme(
      axis.text = element_blank(),
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
      axis.ticks = element_blank(), panel.background = element_blank(),
      legend.title = element_blank(), legend.position = 'top',
      text = element_text(size = 18)
    )

  # return plot
  return(p)
}

#' helper function
#' @name plot_q3_4
#' @description function to prepare the variables of interest
#' @param country plot age distribution
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q3_4 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q3_4')

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
    dplyr::mutate(dob = ifelse(.data$q3_4 == 'Before 1940', 1939, .data$q3_4)) %>%
    dplyr::mutate(age = 2020 - as.numeric(.data$dob)) %>%
    dplyr::mutate(age = cut(.data$age, seq(0,90,10), include.lowest=TRUE)) %>%
    dplyr::count(.data$age)

  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

  # assign labels
  data_final <- data_prepared %>%
    dplyr::mutate(question = ordered(.data$age, levels = data_labels))

  # set plot data
  p <- ggplot(
    data_final,
    aes(x = .data$question, y = .data$n, fill = .data$question)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .8) +
    geom_text(aes(label = .data$n, y = .075*max(.data$n), size = 12), alpha=1) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
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
#' @name plot_q3_5
#' @description what are respondents' marital status?
#' @param country plot age distribution
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q3_5 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q3_5')

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # isolate the number of variables
  samples <- nrow(data)

  # prepare data for plot
  data_prepared <- DiasporaSurveyResults::count_unique(data, 'q3_5')

  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- c(
    'Single', 'In a relationship', 'Married', 'Divorced', 'Widowed'
  )

  # assign labels
  data_final <- data_prepared %>%
    dplyr::mutate(question = ordered(.data$question, levels = data_labels))

  # set plot data
  p <- ggplot(
    data_final,
    aes(x = .data$question, y = .data$perc_total, fill = .data$question)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .8) +
    geom_text(
      aes(label = .data$perc_label, y = .075*max(.data$perc_total), size = 12),
      alpha = 1
    ) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
    labs(y = element_blank(), x = paste0('Respondents: ', samples)) +
    # coord_flip() +
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


#' helper function
#' @name plot_q3_6
#' @description what languages do respondents speak?
#' @param country plot age distribution
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q3_6 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q3_6')

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # isolate the number of variables
  samples <- nrow(data)

  # prepare data for plot
  data_prepared <- DiasporaSurveyResults::count_multiple(data, 'q3_6')
  data_prepared <- data_prepared %>%
    dplyr::mutate(
      perc_total = (.data$abs_total / samples) * 100,
      perc_label = paste0(round(.data$perc_total, 2), '%')
    ) %>%
    dplyr::filter(!stringr::str_detect(.data$question, '^(Other)'))


  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

  # assign labels
  data_final <- data_prepared %>%
    dplyr::mutate(question = ordered(.data$question, levels = data_labels))

  # set plot data
  p <- ggplot(
    data_final,
    aes(x = .data$question, y = .data$perc_total, fill = .data$question)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .8) +
    geom_text(
      aes(label = .data$perc_label, y = .075*max(.data$perc_total), size = 12),
      alpha = 1
    ) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
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
#' @name plot_q3_7
#' @description function to prepare the variables of interest
#' @param country where have respondents acquired their degrees?
#' @import ggplot2
#' @import data.table
#' @importFrom rlang .data .env
#' @export
plot_q3_7 <- function(country = NULL){

  fifelse <- data.table::fifelse

  # load data
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(dplyr::matches('q3_7_[1-9]{1}'), .data$q128, .data$q3_2) %>%
    dplyr::mutate(respondent_id = dplyr::row_number())

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # create list of respondets who have worked on their degree somewhere else
  somewhere_else <- data %>%
    dplyr::filter_at(
      dplyr::vars(-.data$q128, -.data$respondent_id),
      dplyr::all_vars(is.na(.data$.))
    ) %>%
    dplyr::pull(.data$respondent_id)

  somewhere_else <- data %>%
    dplyr::filter_at(
      dplyr::vars(.data$q3_7_10, .data$q3_7_11),
      dplyr::any_vars(!is.na(.data$.))
    ) %>%
    dplyr::pull(.data$respondent_id) %>%
    c(somewhere_else) %>%
    unique() %>%
    sort()

  # transform data for editing education
  plot06_long <- data %>%
    tidyr::pivot_longer(
      -.data$respondent_id, names_to = 'education', values_to = 'degree'
    )

  # prepare data
  plot06a <- plot06_long %>%
    dplyr::filter(.data$education != 'q128') %>%
    dplyr::group_by(.data$respondent_id) %>%
    dplyr::filter(!is.na(.data$degree)) %>%
    dplyr::arrange(.data$education, .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::mutate(
      degree_location = stringi::stri_split_fixed(.data$degree, ',')
    ) %>%
    tidyr::unnest(.data$degree_location) %>%
    dplyr::mutate(
      education = tidyfast::dt_case_when(
        .data$education == 'q3_7_1' ~ 1,
        .data$education == 'q3_7_2' ~ 2,
        .data$education == 'q3_7_3' ~ 3,
        .data$education == 'q3_7_4' ~ 4,
        .data$education == 'q3_7_5' ~ 5,
        .data$education == 'q3_7_6' ~ 6,
        .data$education == 'q3_7_7' ~ 7,
        .data$education == 'q3_7_8' ~ 9,
        .data$education == 'q3_7_9' ~ 8,
        .data$education == 'q3_7_11'~ 11
      )
    ) %>%
    dplyr::select(-.data$degree) %>%
    dplyr::filter(!is.na(.data$degree_location))

  # create second panel
  plot06b <- plot06_long %>%
    dplyr::filter(
      .data$education == 'q128' & !stringr::str_detect(.data$degree, '^O')
    ) %>%
    dplyr::mutate(highest_degree = .data$degree) %>%
    dplyr::select(-.data$education, -.data$degree)

  # create location labels
  degree_labels <- c(
    'Less than high school',
    'High school',
    'Some college or technical studies, but no degree',
    'Technical associate degree',
    'Bachelor\'s degree',
    'Specialization degree',
    'Master\'s degree',
    'Professional degree (JD, MD)',
    'Doctoral degree'
  )

  # assign factors
  plot06a <- plot06a %>%
    dplyr::mutate(education = degree_labels[.data$education])

  # join data from two different datasets
  plot06_sliced <- dplyr::left_join(plot06a, plot06b) %>%
                   dplyr::ungroup()

  # find the right number of unique respondents
  samples <- length(unique(plot06_sliced$respondent_id))

  # fix degree location
  plot06_sliced <- plot06_sliced %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$respondent_id) %>%
    dplyr::mutate(
      highest_degree_location = ifelse(
        .data$highest_degree == .data$education, .data$degree_location, NA_real_
      )
    ) %>%
    dplyr::mutate(
      education = ordered(.data$education, levels = degree_labels),
      highest_degree = ordered(.data$highest_degree, levels = degree_labels)
    ) %>%
    dplyr::mutate(
      highest_degree_location = dplyr::case_when(
        is.na(.data$highest_degree_location) &
          .data$highest_degree > .data$education &
          stringr::str_detect(.data$degree_location, 'In Colombia') ~
          'In country of residence',
        is.na(.data$highest_degree_location) &
          .data$highest_degree > .data$education &
          stringr::str_detect(.data$degree_location, 'In country of residence') ~
          'In Colombia',
        TRUE ~ .data$highest_degree_location
      )
    )
  # define place where some respondents have gotten their higgest degree
  plot06_sliced[
    plot06_sliced$respondent_id %in% somewhere_else,
    'highest_degree_location'
  ] <- 'Somewhere else'

  # count final dataset
  plot06_final <- plot06_sliced %>%
    dplyr::filter(
      !is.na(.data$highest_degree_location) & !is.na(.data$highest_degree)
    ) %>%
    dplyr::select(.data$highest_degree, .data$highest_degree_location) %>%
    dplyr::count(.data$highest_degree, .data$highest_degree_location)

  # set plot data
  p <- ggplot(
    plot06_final,
    aes(
      x = .data$highest_degree,
      y = .data$n,
      fill = .data$highest_degree_location
    )
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', size = .5, alpha = .8) +
    geom_text(
      aes(label = ifelse(.data$n > 0.008*sum(.data$n), .data$n, '')),
      size = 3, position = position_stack(vjust = .5), alpha = 1
    ) +
    guides(
      fill = guide_legend(title = 'Where Respondents Earned Highest Degree:')
    ) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 20)}) +
    scale_fill_manual(values = c('#bfd3e6', '#8c6bb1', '#88419d')) +
    labs(x = element_blank(), y = paste0('Unique Respondents: ', samples)) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.background = element_blank(),
      panel.grid.major.x = element_line(color = 'grey'),
      panel.grid.major.y = element_blank(),
      axis.ticks = element_blank(), panel.border = element_blank(),
      legend.position = 'top', legend.title = element_blank(),
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
      text = element_text(size = 18)
    )

  # return result
  return(p)
}

#' helper function
#' @name plot_q3_8
#' @description function to prepare the variables of interest
#' @param country area of study
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q3_8 <- function(country = NULL){

  # load data
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$q3_8, .data$q3_2, .data$q3_8_43_text) %>%
    dplyr::mutate(
      q3_8 = ifelse(
        stringr::str_detect(.data$q3_8, '^Other'),
        .data$q3_8_43_text,
        .data$q3_8
      )
    ) %>%
    dplyr::filter(!is.na(.data$q3_8))

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # save number of respondents
  samples <- nrow(data)

  data_final <- data %>%
    dplyr::transmute(area_of_study = .data$q3_8) %>%
    dplyr::filter(!stringr::str_detect(.data$area_of_study, '^O')) %>%
    dplyr::mutate(
      area_of_study = ifelse(
        stringr::str_detect(.data$area_of_study, '^(Engi|Infor)'),
        'Engineering, Architecture, and IT',
        .data$area_of_study
      )
    ) %>%
    dplyr::count(.data$area_of_study) %>%
    dplyr::mutate(
      percentage = (.data$n / sum(.data$n)) * 100,
      labels = .data$area_of_study %>%
        paste0(' [', round(.data$percentage, 1), '%]') %>%
        stringr::str_wrap(width = 15)
    )

  # prepare data
  p <- ggplot(
    data_final,
    aes(area = .data$n, fill = .data$percentage, label = .data$labels)
  )

  # create plot
  p <- p +
    treemapify::geom_treemap(start = 'topleft', alpha = .8) +
    treemapify::geom_treemap_text(
      start = 'topleft', place = 'center', size = 16
    ) +
    labs(y = element_blank(), x = paste0('Unique Respondents: ', samples)) +
    scale_fill_distiller(palette = 'Reds', direction = 1) +
    theme(
      legend.position = 'none',
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1)
    )

  # return result
  return(p)
}


#' helper function
#' @name plot_q4_7
#' @description function to prepare the variables of interest
#' @param country why settle
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q4_7 <- function(country = NULL){

  # filter data for question q4_4
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$q4_7, .data$q4_8, .data$q3_2)

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # prepare data
  data_prepared <- data %>%
    dplyr::select(-.data$q3_2) %>%
    dplyr::transmute(
      Respondent = .data$q4_7, Parents = .data$q4_8, id = dplyr::row_number()
    ) %>%
    tidyr::pivot_longer(!.data$id, names_to = 'group', values_to = 'values')


  # compute sample sizes
  sample1 <- data_prepared %>%
    dplyr::filter(.data$group == 'Parents') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()
  sample2 <- data_prepared %>%
    dplyr::filter(.data$group == 'Respondent') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()

  # prepare data
  data_final <- data_prepared %>%
    DiasporaSurveyResults::count_multiple('values', 'group') %>%
    dplyr::filter(!stringr::str_detect(.data$question, '^(Other)')) %>%
    dplyr::mutate(
      perc_total = ifelse(
        .data$group == 'Parents',
        .data$abs_total / sample1,
        .data$abs_total / sample2
      ),
      perc_label = paste0(round(.data$perc_total, 2) * 100, '%')
    ) %>%
    dplyr::mutate(
      abs_label = ifelse(
        .data$group == 'Parents', -1*.data$abs_label, .data$abs_label
      ),
      group = factor(
        .data$group,
        levels = c('Parents', 'Respondent'),
        labels = c(
          paste0('Parents (n = ', sample1, ')'),
          paste0('Respondent (n = ', sample2, ')')
        )
      )
    ) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::arrange(.data$abs_label, .by_group = TRUE) %>%
    dplyr::mutate(
      question = stringr::str_replace_all(
        question,
        '(I don\'t|My parents).*',
        'Doesn\'t Know or Parents Made Decision'
      ),
      question = stringr::str_replace_all(
        question,
        '(Colombia,)( they were| opportunities )(.*)',
        '\\1 opportunities were better abroad'
      ),
      question = stringr::str_remove(question, ' \\(.*\\)')
    )

  # extract labels
  data_labels <- c(
    'Better quality of life',
    'Better future for children',
    'There were few or no employment opportunities in Colombia',
    'Rising crime rates',
    'Although they had employment opportunities in Colombia, opportunities were better',
    'Unstable political situation',
    'Better educational opportunities abroad',
    'Career advancement',
    'Due to family members, friends or a romantic partner',
    'Armed conflict victim/refugee',
    'Better business development opportunities abroad',
    'Insecure job contract',
    'Doesn\'t Know or Parents Made Decision'
  )

  # assign labels
  data_final <- data_final %>%
    dplyr::filter(.data$question %in% data_labels) %>%
    dplyr::mutate(question = ordered(.data$question, levels = rev(data_labels)))

  # set plot data
  p <- ggplot(
    data_final,
    aes(x = .data$question, y = .data$abs_label, fill = .data$group)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .8) +
    geom_text(
      aes(
        label = ifelse(
          .data$abs_label < 0, -1*.data$abs_label, .data$abs_label
        ),
        y = ifelse(
          .data$abs_label < 0,.5*min(.data$abs_label),.05*max(.data$abs_label)
        )
      ),
      alpha = 1
    ) +
    scale_y_continuous(labels = function(x){ifelse(x < 0, -1*x, x)}) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 28)}) +
    geom_hline(aes(yintercept = 0)) +
    labs(x = element_blank(), y = element_blank()) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(), panel.border = element_blank(),
      legend.title = element_blank(), legend.position = 'top',
      axis.ticks.x = element_blank(), text = element_text(size = 12)
    )

  # return chart
  return(p)
}
