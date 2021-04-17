#' helper function
#' @name prepare_plot_q2_3
#' @description function to prepare the variables of interest
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q2_3 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q2_3')

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
    DiasporaSurveyResults::count_multiple('q2_3') %>%
    dplyr::arrange(dplyr::desc(.data$abs_total)) %>%
    dplyr::filter(!stringr::str_detect(.data$x_label, '^(Other)')) %>%
    dplyr::mutate(
      perc_total = (.data$abs_total / samples) * 100,
      perc_label = paste0(round(.data$perc_total, 2), '%')
    )

  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

  # assign labels
  data_final <- data_prepared %>%
    dplyr::mutate(question = ordered(.data$question, levels = data_labels))

  # average for plot
  var <- mean(data_final$perc_total) %>% round(2)
  label <- paste0('Average: ', var, '%')
  library(ggplot2)
  # set plot data
  p <- ggplot(
    data_final,
    aes(x = .data$question, y = .data$perc_total, fill = .data$question)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(aes(label = .data$perc_label, y = 10, size = 12), alpha = 1) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
    labs(x = element_blank(), y = paste0('Respondents: ', samples)) +
    geom_hline(yintercept = var, color = 'grey47') +
    geom_text(aes(y = 1.25*var, x = 1, label = label, size = 12), alpha = 1) +
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
#' @name prepare_plot_q136
#' @description where do they live
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q136 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q136')

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
    DiasporaSurveyResults::count_unique('q136') %>%
    dplyr::arrange(dplyr::desc(.data$abs_total)) %>%
    dplyr::filter(nchar(.data$question) > 0) %>%
    dplyr::slice(1:10)

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
    geom_bar(stat = 'identity', alpha = .7) +
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
#' @name prepare_plot_q200
#' @description cross-tab region and consulate coverage
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q200 <- function(country = NULL){

  # filter data for question q200
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$embassy, .data$region, .data$q3_2)

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # compute the number of consulates reported in each country
  nconsulates <- data %>%
    dplyr::pull(embassy) %>%
    unique() %>%
    length()

  # select the variable for analysis
  if (nconsulates > 1) {

    # select variable
    data_prepared <- data %>%
      dplyr::transmute(question = .data$embassy) %>%
      dplyr::filter(!is.na(.data$question))

    # define title for graph
    title <- 'Consulates'
  } else {

    # select variable
    data_prepared <- data %>%
      dplyr::transmute(question = .data$region) %>%
      dplyr::filter(!is.na(.data$question))

    # define title for graph
    title <- 'Regions'
  }

  # isolate the number of variables
  samples <- nrow(data_prepared)

  # prepare data
  data_final <- data_prepared %>%
    dplyr::count(.data$question) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::filter(nchar(.data$question) > 0) %>%
    dplyr::slice(1:10)

  # assign labels
  data_final <- data_final %>%
    dplyr::mutate(
      question = ordered(.data$question, levels = rev(unique(.data$question)))
    )

  # set plot data
  p <- ggplot(
    data_final,
    aes(x = .data$question, y = .data$n, fill = .data$question)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(
      aes(label = .data$n, y = .05*max(.data$n), size = 12),
      alpha = 1
    ) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
    labs(
      title = paste0('Respondents by ', title, ':'),
      x = element_blank(),
      y = paste0('Respondents: ', samples)
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(), panel.border = element_blank(),
      legend.position = 'none', axis.ticks.x = element_blank(),
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
      plot.title = element_text(size = 14, hjust = 1),
      text = element_text(size = 18)
    )

  # return plot
  return(p)
}


#' helper function
#' @name prepare_plot_q2_4
#' @description ask about how many years respondents had been living in Colombia
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q2_4 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q2_4')

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
    DiasporaSurveyResults::count_unique('q2_4') %>%
    dplyr::arrange(dplyr::desc(.data$abs_total))

  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- c(
    'Less than a year',
    'Between one and 3 years',
    'Between 3 and 5 years',
    'I have never lived in Colombia',
    'Between 5 and 10 years',
    'More than 10 years'
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
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(aes(label = .data$perc_label, y = 10, size = 12), alpha = 1) +
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
#' @name plot_q3_2
#' @description function to prepare the variables of interest
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q3_2 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q3_2')

  # prepare data
  data_prepared <- data %>%
    dplyr::transmute(
      Nationality = .data$q137, Residency = .data$q3_2, id = dplyr::row_number()
    ) %>%
    tidyr::pivot_longer(!.data$id, names_to = 'group', values_to = 'values')

  # compute sample sizes
  sample1 <- data_prepared %>%
    dplyr::filter(.data$group == 'Residency') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()
  sample2 <- data_prepared %>%
    dplyr::filter(.data$group == 'Nationality') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()

  # prepare data
  data_final <- data_prepared %>%
    DiasporaSurveyResults::count_unique('values', 'group') %>%
    dplyr::mutate(
      abs_label = ifelse(
        .data$group == 'Residency', -1*.data$abs_label, .data$abs_label
      ),
      group = factor(
        .data$group,
        levels = c( 'Residency', 'Nationality'),
        labels = c(
          paste0('Residency (n = ', sample1, ')'),
          paste0('Nationality (n = ', sample2, ')')
        )
      )
    ) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::arrange(.data$abs_label, .by_group = TRUE)

  # extract labels
  data_labels <- data_final %>%
    dplyr::filter(stringr::str_detect(.data$group, 'Residency')) %>%
    dplyr::slice(1:10) %>%
    dplyr::pull(.data$question)

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
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(
      aes(
        label = ifelse(
          .data$abs_label < 0, -1 * .data$abs_label, .data$abs_label
        ),
        y = ifelse(
          .data$abs_label < 0, -150 + .data$abs_label, 150 + .data$abs_label
        )
      ),
      size = 5,
      alpha = 1
    ) +
    scale_y_continuous(labels = function(x){ifelse(x < 0, -1*x, x)}) +
    geom_hline(aes(yintercept = 0)) +
    labs(x = element_blank(), y = element_blank()) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(), panel.border = element_blank(),
      legend.title = element_blank(), legend.position = 'top',
      axis.ticks.x = element_blank(), text = element_text(size = 18)
    )

  # return result
  return(p)
}

#' helper function
#' @name plot_q5_2
#' @description function to prepare the variables of interest
#' @param country what have they stopped before final destination
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q5_2 <- function(country = NULL){

  # create StatStratum
  StatStratum <- ggalluvial::StatStratum

  # filter data
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(SOURCE = .data$q5_2, TARGET = .data$q3_2, q3_2 = .data$q3_2)

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # prepare data
  data_prepared <- data %>%
    dplyr::select(-.data$q3_2) %>%
    dplyr::filter(!is.na(.data$SOURCE) & !is.na(.data$TARGET)) %>%
    dplyr::filter_all(dplyr::all_vars(!stringr::str_detect(.data$., 'Other'))) %>%
    dplyr::count(.data$SOURCE, .data$TARGET) %>%
    dplyr::filter(.data$SOURCE != .data$TARGET)

  # add another filter here
  if (!is.null(country)) {
    if (country == 'All') {
      data_prepared <- dplyr::filter(data_prepared, .data$n >= 20)
    }
    else {
      data_prepared <- data_prepared %>%
        dplyr::arrange(desc(.data$n)) %>%
        dplyr::slice(1:10)
    }
  } else {
    data_prepared <- dplyr::filter(data_prepared, .data$n >= 20)
  }


  # isolate the number of variables
  samples <- sum(data_prepared$n)

  # prepare data
  data_final <- data_prepared %>%
    dplyr::mutate(fill = as.character(dplyr::row_number())) %>%
    dplyr::group_by(.data$SOURCE) %>%
    dplyr::mutate(SOURCE_n = sum(as.integer(.data$n))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(SOURCE = paste0(.data$SOURCE, ' [', .data$SOURCE_n, ']')) %>%
    dplyr::group_by(.data$TARGET) %>%
    dplyr::mutate(TARGET_n = sum(as.integer(.data$n))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TARGET = paste0(.data$TARGET, ' [', .data$TARGET_n, ']')) %>%
    {ggalluvial::to_lodes_form(data.frame(.), key = 'where', axes = 1:2)}

  # prepare data for plot
  p <- ggplot(
    data = data_final, aes(
      x        = .data$where,
      stratum  = .data$stratum,
      alluvium = .data$alluvium,
      y        = as.integer(.data$n),
      fill     = .data$stratum,
      label    = .data$stratum
    )
  )

  # create graph
  p <- p +
    scale_y_continuous() +
    scale_x_discrete(expand = c(.2, .2)) +
    ggalluvial::geom_flow(width = .1, color = 'white') +
    ggalluvial::geom_stratum(alpha = .7, width = .1, color = 'white') +
    ggrepel::geom_text_repel(
      aes(
        label = ifelse(
          as.numeric(.data$where) == 1,
          stringr::str_wrap(as.character(.data$stratum), width = 20),
          NA
        )
      ),
      stat = StatStratum, size = 5, direction = 'y', nudge_x = -.1, hjust = 1,
      segment.color = NA
    ) +
    ggrepel::geom_text_repel(
      aes(
        label = ifelse(
          as.numeric(.data$where) == 2,
          stringr::str_wrap(as.character(.data$stratum), width = 20),
          NA
        )
      ),
      stat = StatStratum, size = 5, direction = 'y', nudge_x = .1, hjust = 0,
      segment.color = NA
    ) +
    labs(y = element_blank(), x = paste0('Respondents: ', samples)) +
    theme(
      legend.position = 'none', panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.ticks.y = element_blank(), axis.text.y = element_blank(),
      axis.ticks.x = element_blank(), axis.text.x = element_blank(),
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1)
    )
p
  # return chart
  return(p)
}

