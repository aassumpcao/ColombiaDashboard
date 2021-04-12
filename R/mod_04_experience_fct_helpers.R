#' helper function
#' @name plot_q3_9
#' @description function to prepare the variables of interest
#' @param country area of study
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q3_9 <- function(country = NULL){

  # filter data for question q3_9
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(dplyr::matches('q3_9'), .data$q3_2)

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
    tidyr::pivot_longer(
      -.data$q3_2, names_to = 'question', values_to = 'where'
    ) %>%
    dplyr::mutate(where = stringr::str_split(.data$where, ',')) %>%
    tidyr::unnest(.data$where) %>%
    dplyr::mutate(
      where = cut(
        as.numeric(.data$where),
        breaks = c(0, 5, 10, 20, 50),
        include.lowest = TRUE)
    ) %>%
    dplyr::mutate(question = dplyr::case_when(
      .data$question == 'q3_9_1' ~ 1,
      .data$question == 'q3_9_2' ~ 2,
      .data$question == 'q3_9_3' ~ 3
    )) %>%
    dplyr::filter(!is.na(.data$where)) %>%
    dplyr::count(.data$question, .data$where) %>%
    dplyr::group_by(.data$question) %>%
    dplyr::mutate(
      percentage = round((.data$n / sum(.data$n)) * 100, 1),
      labels = ifelse(.data$percentage > 5, paste0(.data$percentage, '%'), ''),
      nsample = paste0('(n = ', sum(.data$n), ')')
    ) %>%
    dplyr::ungroup()

    # assign labels
  data_final <- data_prepared %>%
    dplyr::mutate(where = ordered(.data$where))

  levels <- c('In Colombia', 'In country of residence', 'In other countries')

  # set plot data
  p <- ggplot(
    data_final,
    aes(
      x = .data$question,
      y = .data$percentage,
      fill = .data$where,
      group = .data$where
    )
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .8) +
    geom_text(
      aes(label = .data$labels), position = position_stack(vjust = .5), size = 5
    )+
    guides(fill = guide_legend(title = 'Years of Experience:')) +
    scale_y_reverse() +
    scale_x_continuous(
      breaks = 1:3,
      labels = stringr::str_wrap(levels, width = 20),
      sec.axis = dup_axis(labels = .data$nsample)
    ) +
    # scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
    scale_fill_brewer(direction = -1) +
    labs(x = element_blank(), y = paste0('Unique Respondents: ', samples)) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      panel.border = element_blank(), axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(), text = element_text(size = 14),
      legend.position = 'top'
    )

  # return result
  return(p)
}

#' helper function
#' @name plot_q4_4
#' @description function to prepare the variables of interest
#' @param country what were they doing before leaving and currently
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q4_4 <- function(country = NULL){

  # create StatStratum
  StatStratum <- ggalluvial::StatStratum

  # filter data
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(SOURCE = .data$q4_4, TARGET = .data$q5_6, .data$q3_2)

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
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
      text = element_text(size = 14)
    )

  # return chart
  return(p)
}

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

#' helper function
#' @name plot_q5_4
#' @description function to prepare the variables of interest
#' @param country why settle
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q5_4 <- function(country = NULL){

  # filter data for question q4_4
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$q5_4, .data$q5_5, .data$q3_2)

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
      Respondent = .data$q5_4, Parents = .data$q5_5, id = dplyr::row_number()
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
    dplyr::arrange(.data$abs_label, .by_group = TRUE)

  # extract labels
  data_labels <- c(
    'Better quality of life',
    'Better employment opportunities',
    'Family ties, friends or romantic partner',
    'Better personal security',
    'Better educational opportunities',
    'More political stability',
    'Cultural or language familiarity',
    'Scholarship offer',
    'Better business/investment opportunities',
    'My parents or grandparents made the decision',
    'I don\'t know'
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
          .data$abs_label < 0,.75*min(.data$abs_label),.05*max(.data$abs_label)
        )
      ),
      size = 5,
      alpha = 1
    ) +
    scale_y_continuous(labels = function(x){ifelse(x < 0, -1*x, x)}) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
    geom_hline(aes(yintercept = 0)) +
    labs(x = element_blank(), y = element_blank()) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(), panel.border = element_blank(),
      legend.title = element_blank(), legend.position = 'top',
      axis.ticks.x = element_blank(), text = element_text(size = 14)
    )

  # return chart
  return(p)
}

#' helper function
#' @name plot_q5_8
#' @description function to prepare the variables of interest
#' @param country what educational leve are their pursuing
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q5_8 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q5_8')

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # isolate the number of variables
  samples <- nrow(data)

  # prepare data for plot
  data_prepared <- DiasporaSurveyResults::count_unique(data, 'q5_8')
  data_prepared <- data_prepared %>%
    dplyr::mutate(
      perc_total = (.data$abs_total / samples) * 100,
      perc_label = paste0(round(.data$perc_total, 2), '%')
    ) %>%
    dplyr::filter(!stringr::str_detect(.data$question, '^(Other)'))


  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- c(
   'High school degree',
   'Technical associate degree',
   'Bachelor\'s degree in college',
   'Specialization degree',
   'Master\'s degree',
   'Professional degree (JD, MD)',
   'Doctoral degree'
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
      aes(label = .data$perc_label, y = .075*max(.data$perc_total), size = 14),
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
#' @name plot_q5_9
#' @description function to prepare the variables of interest
#' @param country what area of study are they in?
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q5_9 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q5_9')

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # compute sample size
  samples <- nrow(data)

  # prepare data for plot
  data_prepared <- DiasporaSurveyResults::count_unique(data, 'q5_9')
  data_prepared <- data_prepared %>%
    dplyr::mutate(
      labels = .data$question %>%
        paste0(' [', round(.data$perc_total, 1), '%]') %>%
        stringr::str_wrap(width = 15)
    ) %>%
    dplyr::filter(!stringr::str_detect(.data$question, '^(Other)'))

  # set plot data
  p <- ggplot(
    data_prepared,
    aes(
      area = .data$abs_label, fill = .data$perc_total, label = .data$labels
    )
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
#' @name plot_q5_12
#' @description function to prepare the variables of interest
#' @param country role in main job
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q5_11 <- function(country = NULL){

  # filter data for question q4_4
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$q122, .data$q5_11, .data$q3_2) %>%
    tidyr::drop_na()

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
      `In Colombia` = .data$q122,
      Currently = .data$q5_11,
      id = dplyr::row_number()
    ) %>%
    tidyr::pivot_longer(!.data$id, names_to = 'group', values_to = 'values')

  # compute sample sizes
  sample1 <- data_prepared %>%
    dplyr::filter(.data$group == 'In Colombia') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()
  sample2 <- data_prepared %>%
    dplyr::filter(.data$group == 'Currently') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()

  # prepare data
  data_final <- data_prepared %>%
    DiasporaSurveyResults::count_multiple('values', 'group') %>%
    dplyr::filter(!stringr::str_detect(.data$question, '^(Other)')) %>%
    dplyr::mutate(
      perc_total = ifelse(
        .data$group == 'In Colombia',
        .data$abs_total / sample1,
        .data$abs_total / sample2
      ),
      perc_label = paste0(round(.data$perc_total, 2) * 100, '%')
    ) %>%
    dplyr::mutate(
      abs_label = ifelse(
        .data$group == 'In Colombia', -1*.data$abs_label, .data$abs_label
      ),
      group = factor(
        .data$group,
        levels = c('In Colombia', 'Currently'),
        labels = c('In Colombia', 'Currently')
      )
    ) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::arrange(.data$abs_label, .by_group = TRUE)

  # extract labels
  data_labels <- c(
    'Employee',
    'Self-Employed or freelancer',
    'Business Owner',
    'Trainee or intern',
    'Unpaid Employee'
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
          .data$abs_label < 0,.13*min(.data$abs_label),.13*max(.data$abs_label)
        )
      ),
      alpha = 1, size = 5
    ) +
    scale_y_continuous(labels = function(x){ifelse(x < 0, -1*x, x)}) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
    geom_hline(aes(yintercept = 0)) +
    labs(x = element_blank(), y = element_blank()) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(), panel.border = element_blank(),
      legend.title = element_blank(), legend.position = 'top',
      axis.ticks.x = element_blank(), text = element_text(size = 16)
    )

  # return chart
  return(p)
}

#' helper function
#' @name plot_q5_12
#' @description function to prepare the variables of interest
#' @param country entrepreneurship background
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q5_12 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q5_12')

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # isolate the number of variables
  samples <- nrow(data)

  # prepare data for plot
  data_prepared <- DiasporaSurveyResults::count_multiple(data, 'q5_12')
  data_prepared <- data_prepared %>%
    dplyr::mutate(
      question = stringr::str_remove(.data$question, ' \\(please specify\\)')
    )

  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- c(
    'I have taught classes on entrepreneurship',
    'I have taken classes on entrepreneurship',
    'I used to run a business',
    'I am currently running a business',
    'I have a history of investing in startup businesses',
    'I currently invest in startup businesses',
    'Other background with entrepreneurship',
    'I do not have a background with entrepreneurship'
  )

  # assign labels
  data_final <- data_prepared %>%
    dplyr::mutate(question = ordered(.data$question, levels = data_labels))

  library(ggplot2)
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
#' @name plot_q5_13
#' @description function to prepare the variables of interest
#' @param country other background
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q5_13 <- function(country = NULL){

  # filter data
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::filter(!is.na(.data$q5_12_8_text) & .data$q5_12_8_text != 'NA') %>%
    dplyr::select(background = .data$q5_12_8_text, .data$q3_2)

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # compute sample size
  samples <- nrow(data)

  # reorganize
  data_prepared <- data %>%
    dplyr::count(.data$background) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::filter(!stringr::str_detect(.data$background, '^(Other)$')) %>%
    dplyr::slice(1:10) %>%
    dplyr::mutate(
      background = ordered(.data$background, levels = rev(.data$background))
    )

  # prepare final data
  p <- ggplot(
    data_prepared,
    aes(x = .data$background, y = .data$n, fill = .data$background)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .8) +
    geom_text(
      aes(label = .data$n, y = .075*max(.data$n), size = 12),
      alpha = 1
    ) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 30)}) +
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

  # return plot
  return(p)
}
