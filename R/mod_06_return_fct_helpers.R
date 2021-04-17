#' helper function
#' @name plot_q7_2
#' @description function to prepare the variables of interest
#' @param country plot family members or friends who live in colombia
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q7_2 <- function(country = NULL){

  # filter data for question q2_3
  data <- DiasporaSurveyResults::prepare_data('q7_2')

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
    DiasporaSurveyResults::count_unique('q7_2')

  # extract labels
  data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
  data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

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
    geom_bar(stat = 'identity', alpha = .7) +
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
#' @name prepare_plot_q2_5
#' @description engagement with colombia for people who returned due to COVID
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q2_5 <- function(country = NULL){

  # filter data for questions of interest
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::filter(.data$q2_5 == 'Yes')

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # store sample size
  samples <- nrow(data)

  # select variables of interest
  data_prepared <- data %>%
    dplyr::select(.data$q7_12, .data$q7_13)

  # define data on engamgenet and plans
  data_engagement <- data_prepared %>%
    dplyr::transmute(
      response = stringr::str_replace(.data$q7_12, 'Yes, it makes me ','Yes, '),
      type = '...impact their engagement in activities?'
    )

  data_return <- data_prepared %>%
    dplyr::transmute(
      response = stringr::str_replace(.data$q7_13, 'Yes, it makes me ','Yes, '),
      type = '...change their willingness to return permanently?'
    )

  # create plot
  data_final <- dplyr::bind_rows(data_engagement, data_return) %>%
                dplyr::group_by(.data$type) %>%
                dplyr::count(.data$response) %>%
                dplyr::filter(!stringr::str_detect(.data$response, 'Other')) %>%
                dplyr::mutate(
                  fill = dplyr::case_when(
                    stringr::str_detect(.data$response, '^(No)$') ~ '1',
                    stringr::str_detect(.data$response, '^(Not sure)$') ~ '2',
                    stringr::str_detect(.data$response, 'less likely') ~ '3',
                    stringr::str_detect(.data$response, 'more likely') ~ '4',
                  )
                  #as.character(1:4)
                )

  # isolate levels
  p <- ggplot(
    data_final,
    aes(x = .data$response, y = .data$n, group = .data$type, fill = .data$fill)
  )

  p <- p +
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(
      aes(label = .data$n, y = .075*max(.data$n), size = 12), alpha = 1
    ) +
    facet_grid(. ~ type, scales = 'free_x') +
    scale_x_discrete(labels = function(x){str_wrap(x, width = 15)}) +
    scale_fill_manual(
      values = c('#41ab5d', '#238b45', '#006d2c', '#00441b')
    ) +
    labs(y = element_blank(), x = paste0('Respondents: ', samples)) +
    theme_linedraw() +
    theme(
      panel.grid.major.y = element_line(colour = 'grey15'),
      panel.grid.major.x = element_blank(), legend.position = 'none',
      axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
      text = element_text(size = 14)
    )

    # return plot
    return(p)
}

#' helper function
#' @name prepare_plot_q7_8
#' @description engagement with colombia for people who returned due to COVID
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q7_8 <- function(country = NULL){

  # filter data for question q4_4
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$q7_8, .data$q7_9, .data$q3_2)

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
      Currently = .data$q7_8, `In the Future` = .data$q7_9,
      id = dplyr::row_number()
    ) %>%
    tidyr::pivot_longer(!.data$id, names_to = 'group', values_to = 'values')


  # compute sample sizes
  sample1 <- data_prepared %>%
    dplyr::filter(.data$group == 'In the Future') %>%
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
        .data$group == 'In the Future',
        .data$abs_total / sample1,
        .data$abs_total / sample2
      ),
      perc_label = paste0(round(.data$perc_total, 2) * 100, '%')
    ) %>%
    dplyr::mutate(
      abs_label = ifelse(
        .data$group == 'Currently', -1*.data$abs_label, .data$abs_label
      ),
      group = factor(
        .data$group,
        levels = c('Currently', 'In the Future'),
        labels = c(
          paste0('Currently (n = ', sample2, ')'),
          paste0('In the Future (n = ', sample1, ')')
        )
      )
    ) %>%
    dplyr::mutate(
      question = stringr::str_remove(.data$question,' in Colombia'),
      question = dplyr::case_when(
        stringr::str_detect(question, '[Vv]olunteer') ~ 'Volunteering',
        stringr::str_detect(question, 'Mentoring') ~ 'Mentoring youth',
        TRUE ~ question
      )
    )


  # extract labels
  data_labels <- c(
    'Investing',
    'Long distance professional support',
    'Starting a business',
    'Humanitarian financial support',
    'Mentoring youth',
    'Volunteering',
    'Pursuing studies or coursework',
    'Political engagement',
    'Funding or mentoring entrepreneurs',
    'Funding or mentoring Colombian entrepreneurs overseas',
    'Pursuing a few months of professional exchange',
    'Pursuing an internship',
    'No, I am satisfied with my current engagement'
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
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(
      aes(
        label = ifelse(
          .data$abs_label < 0, -1*.data$abs_label, .data$abs_label
        ),
        y = ifelse(
          .data$abs_label < 0,.1*min(.data$abs_label),.1*max(.data$abs_label)
        )
      ),
      alpha = 1
    ) +
    scale_y_continuous(labels = function(x){ifelse(x < 0, -1*x, x)}) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 35)}) +
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

#' helper function
#' @name prepare_plot_q7_11
#' @description engagement with colombia for people who returned due to COVID
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q7_11 <- function(country = NULL){

  # filter data
  data <- DiasporaSurveyResults::prepare_data('q7_11')

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
    DiasporaSurveyResults::count_multiple('q7_11')

  # clean and extract labels
  data_prepared <- data_prepared %>%
    dplyr::mutate(
      question = stringr::str_remove(.data$question, ' in Colombia'),
      question = dplyr::case_when(
        stringr::str_detect(question, '[Vv]olunteer') ~ 'Volunteering',
        stringr::str_detect(question, 'Mentoring') ~ 'Mentoring youth',
        TRUE ~ question
      )
    ) %>%
    dplyr::filter(!stringr::str_detect(.data$question, '^(Other)'))

  # extract labels
  data_labels <- c(
    'Investing',
    'Long distance professional support',
    'Starting a business',
    'Humanitarian financial support',
    'Mentoring youth',
    'Volunteering',
    'Pursuing studies or coursework',
    'Political engagement',
    'Funding or mentoring entrepreneurs',
    'Funding or mentoring Colombian entrepreneurs overseas',
    'Pursuing a few months of professional exchange',
    'Pursuing an internship'
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
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(
      aes(label = .data$abs_total, y = .075*max(.data$abs_total), size = 12),
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

#' helper function
#' @name prepare_plot_q7_14
#' @description relevant policy for family reasons
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q7_14 <- function(country = NULL){

  # filter data for question q4_4
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$q7_14, .data$q5_6, .data$q3_2) %>%
    dplyr::mutate(
      group = ifelse(
        stringr::str_detect(.data$q5_6, 'Working'),
        'Working Professionals',
        'Students'
      ),
      values = .data$q7_14
    )

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # compute sample sizes
  sample1 <- data %>%
    dplyr::filter(.data$group == 'Working Professionals') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()
  sample2 <- data %>%
    dplyr::filter(.data$group == 'Students') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()

  # prepare data
  data_final <- data %>%
    DiasporaSurveyResults::count_multiple('values', 'group') %>%
    dplyr::filter(!stringr::str_detect(.data$question, '^(Other)')) %>%
    dplyr::mutate(
      perc_total = ifelse(
        .data$group == 'Working Professionals',
        .data$abs_total / sample1,
        .data$abs_total / sample2
      ),
      perc_label = paste0(round(.data$perc_total, 2) * 100, '%')
    ) %>%
    dplyr::mutate(
      abs_label = ifelse(
        .data$group == 'Students', -1*.data$abs_label, .data$abs_label
      ),
      group = factor(
        .data$group,
        levels = c('Students', 'Working Professionals'),
        labels = c(
          paste0('Students (n = ', sample2, ')'),
          paste0('Working Professionals (n = ', sample1, ')')
        )
      )
    )

  # extract labels
  data_labels <- c(
    'Start up grants for returnee migrants looking to start a business',
    'Facilitated internship programs in Colombia',
    'Facilitated internship programs outside of Colombia',
    'Annual general diaspora summit',
    'Professional exchange programs in your area of work',
    'Support for returnee migrants to purchase homes',
    'Programs for health-care support to returning retirees',
    'Programs for high quality retirement homes for returnees',
    'I do not believe I could benefit from programs by the Colombian governemnt'
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
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(
      aes(
        label = ifelse(
          .data$abs_label < 0, -1*.data$abs_label, .data$abs_label
        ),
        y = ifelse(
          .data$abs_label < 0,.1*min(.data$abs_label),.1*max(.data$abs_label)
        )
      ),
      alpha = 1
    ) +
    scale_y_continuous(labels = function(x){ifelse(x < 0, -1*x, x)}) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 35)}) +
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

#' helper function
#' @name prepare_plot_q7_15
#' @description relevant policy for professional reasons
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q7_15 <- function(country = NULL){

  # filter data for question q4_4
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::select(.data$q7_15, .data$q5_6, .data$q3_2) %>%
    dplyr::mutate(
      group = ifelse(
        stringr::str_detect(.data$q5_6, 'Working'),
        'Working Professionals',
        'Students'
      ),
      values = .data$q7_15
    )

  # add country filter
  if (!is.null(country)) {
    if (country != 'All') {
      data <- dplyr::filter(data, .data$q3_2 == country)
    }
  }

  # compute sample sizes
  sample1 <- data %>%
    dplyr::filter(.data$group == 'Working Professionals') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()
  sample2 <- data %>%
    dplyr::filter(.data$group == 'Students') %>%
    tidyr::drop_na(.data$values) %>%
    nrow()

  # prepare data
  data_final <- data %>%
    DiasporaSurveyResults::count_multiple('values', 'group') %>%
    dplyr::filter(!stringr::str_detect(.data$question, '^(Other)')) %>%
    dplyr::mutate(
      perc_total = ifelse(
        .data$group == 'Working Professionals',
        .data$abs_total / sample1,
        .data$abs_total / sample2
      ),
      perc_label = paste0(round(.data$perc_total, 2) * 100, '%')
    ) %>%
    dplyr::mutate(
      abs_label = ifelse(
        .data$group == 'Students', -1*.data$abs_label, .data$abs_label
      ),
      group = factor(
        .data$group,
        levels = c('Students', 'Working Professionals'),
        labels = c(
          paste0('Students (n = ', sample2, ')'),
          paste0('Working Professionals (n = ', sample1, ')')
        )
      )
    ) %>%
    dplyr::mutate(
      question = dplyr::case_when(
        stringr::str_detect(.data$question, 'Tax incentives') ~
          'Tax incentives for FDI',
        stringr::str_detect(.data$question, 'R&D') ~
          'Funding for R&D',
        stringr::str_detect(.data$question, 'projects in partnership') ~
          'Funding for non-R&D projects',
        stringr::str_detect(.data$question, 'forums') ~
          'Business forums or networking programs',
        stringr::str_detect(.data$question, 'explore investment') ~
          'Investment-focused programs',
        stringr::str_detect(.data$question, 'Small grants') ~
          'Small grants for professional visits',
        stringr::str_detect(.data$question,'benefit') ~
          'No program would benefit my professional activites.'
      )
    )

  # extract labels
  data_labels <- c(
    'Tax incentives for FDI',
    'Funding for R&D',
    'Funding for non-R&D projects',
    'Business Forums or networking programs',
    'Investment-focused programs',
    'Small grants for professional visits',
    'No program would benefit my professional activites.'
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
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(
      aes(
        label = ifelse(
          .data$abs_label < 0, -1*.data$abs_label, .data$abs_label
        ),
        y = ifelse(
          .data$abs_label < 0,.1*min(.data$abs_label),.1*max(.data$abs_label)
        )
      ),
      alpha = 1
    ) +
    scale_y_continuous(labels = function(x){ifelse(x < 0, -1*x, x)}) +
    scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 35)}) +
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

#' helper function
#' @name prepare_plot_q2_5
#' @description policies used in the past
#' @param country filter analysis by country
#' @import ggplot2
#' @importFrom rlang .data .env
#' @export
plot_q135 <- function(country = NULL){

  # filter data
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::filter(!is.na(.data$q135)) %>%
    dplyr::select(programs = .data$q135)


  # create list of programs
  policies <- c(
    'agricultur[ae]|aliment', 'beca|colfuturo|colciencia',
    'icetex|(credito.*educaci)', 'tribut|tax', 'procolombia'
  )

  # prepare data
  data_prepared <- data %>%
    dplyr::mutate(programs = stringr::str_to_lower(.data$programs)) %>%
    dplyr::mutate(
      agriculture = as.integer(
        stringr::str_detect(.data$programs, policies[1])
      ),
      scholarship = as.integer(
        stringr::str_detect(.data$programs, policies[2])
      ),
      education_loan = as.integer(
        stringr::str_detect(.data$programs, policies[3])
      ),
      tax_credits = as.integer(
        stringr::str_detect(.data$programs, policies[4])
      ),
      procolombia = as.integer(
        stringr::str_detect(.data$programs, policies[5])
      )
    )

  # produce final dataset
  data_final <- data_prepared %>%
    dplyr::filter_at(dplyr::vars(-.data$programs), dplyr::any_vars(. == 1)) %>%
    dplyr::summarize_at(dplyr::vars(-.data$programs), sum) %>%
    t() %>%
    tibble::as_tibble(rownames = 'program', .name_repair = 'unique') %>%
    dplyr::rename(V1 = `...1`) %>%
    dplyr::mutate(
      group = ordered(
        .data$program,
        levels = sort(.data$program),
        labels = c(
          'Agriculture Incentives', 'Scholarships', 'Educational Loans',
          'Tax Credits', 'ProColombia'
        )
      )
    )


  # compute number of observations
  samples <- sum(data_final$V1)

  # set plot data
  p <- ggplot(
    data_final,
    aes(x = .data$group, y = .data$V1, fill = .data$group)
  )

  # create plot
  p <- p +
    geom_bar(stat = 'identity', alpha = .7) +
    geom_text(
      aes(label = .data$V1, y = .075*max(.data$V1), size = 12),
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
