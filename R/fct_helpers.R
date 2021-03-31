# #' helper function
# #' @name prepare_plot_q2_3
# #' @description function to prepare the variables of interest
# #' @param country filter analysis by country
# #' @import ggplot2
# #' @importFrom rlang .data .env
# #' @export
# plot_q2_3 <- function(country = NULL){
#   # library(ggplot2)
#   # filter data for question q2_3
#   data <- DiasporaSurveyResults::prepare_data('q2_3')

#   # add country filter
#   if (!is.null(country)) {
#     if (country != 'All') {
#       data <- dplyr::filter(data, .data$q3_2 == country)
#     }
#   }

#   # isolate the number of variables
#   samples <- nrow(data)

#   # prepare data for plot
#   data_prepared <- data %>%
#     DiasporaSurveyResults::count_multiple('q2_3') %>%
#     dplyr::arrange(dplyr::desc(.data$abs_total)) %>%
#     dplyr::filter(!stringr::str_detect(.data$x_label, '^(Other)')) %>%
#     dplyr::mutate(
#       perc_total = (.data$abs_total / samples) * 100,
#       perc_label = paste0(round(.data$perc_total, 2), '%')
#     )

#   # extract labels
#   data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
#   data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

#   # assign labels
#   data_final <- data_prepared %>%
#     dplyr::mutate(question = ordered(.data$question, levels = data_labels))

#   # set plot data
#   p <- ggplot(
#     data_final,
#     aes(x = .data$question, y = .data$perc_total,fill = .data$question)
#   )

#   # create plot
#   p <- p +
#     geom_bar(stat = 'identity', alpha = .8) +
#     geom_text(aes(label = .data$perc_label, y = 10, size = 12), alpha = 1) +
#     scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
#     labs(x = element_blank(), y = paste0('Respondents: ', samples)) +
#     coord_flip() +
#     theme_bw() +
#     theme(
#       panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
#       axis.ticks.y = element_blank(), panel.border = element_blank(),
#       legend.position = 'none', axis.ticks.x = element_blank(),
#       axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
#       text = element_text(size = 18)
#     )

#   # return result
#   return(p)
# }

# #' helper function
# #' @name plot_q3_2
# #' @description function to prepare the variables of interest
# #' @param country filter analysis by country
# #' @import ggplot2
# #' @importFrom rlang .data .env
# #' @export
# plot_q3_2 <- function(country = NULL){

#   # filter data for question q2_3
#   data <- DiasporaSurveyResults::prepare_data('q3_2')

#   # add country filter
#   if (!is.null(country)){data <- dplyr::filter(data, .data$q3_2 == country)}

#   # prepare data
#   data_prepared <- data %>%
#     dplyr::transmute(
#       Nationality = .data$q137, Residency = .data$q3_2, id = dplyr::row_number()
#     ) %>%
#     tidyr::pivot_longer(!.data$id, names_to = 'group', values_to = 'values')

#   # compute sample sizes
#   sample1 <- data_prepared %>%
#     dplyr::filter(.data$group == 'Residency') %>%
#     tidyr::drop_na(.data$values) %>%
#     nrow()
#   sample2 <- data_prepared %>%
#     dplyr::filter(.data$group == 'Nationality') %>%
#     tidyr::drop_na(.data$values) %>%
#     nrow()

#   # prepare data
#   data_final <- data_prepared %>%
#     DiasporaSurveyResults::count_unique('values', 'group') %>%
#     dplyr::mutate(
#       abs_label = ifelse(
#         .data$group == 'Residency', -1*.data$abs_label, .data$abs_label
#       ),
#       group = factor(
#         .data$group,
#         levels = c( 'Residency', 'Nationality'),
#         labels = c(
#           paste0('Residency (n = ', sample1, ')'),
#           paste0('Nationality (n = ', sample2, ')')
#         )
#       )
#     ) %>%
#     dplyr::group_by(.data$group) %>%
#     dplyr::arrange(.data$abs_label, .by_group = TRUE)

#   # extract labels
#   data_labels <- data_final %>%
#     dplyr::filter(stringr::str_detect(.data$group, 'Residency')) %>%
#     dplyr::slice(1:10) %>%
#     dplyr::pull(.data$question)

#   # assign labels
#   data_final <- data_final %>%
#     dplyr::filter(.data$question %in% data_labels) %>%
#     dplyr::mutate(question = ordered(.data$question, levels = rev(data_labels)))

#   # set plot data
#   p <- ggplot(
#     data_final,
#     aes(x = .data$question, y = .data$abs_label, fill = .data$group)
#   )

#   # create plot
#   p <- p +
#     geom_bar(stat = 'identity', alpha = .8) +
#     geom_text(
#       aes(
#         label = ifelse(
#           .data$abs_label < 0, -1 * .data$abs_label, .data$abs_label
#         ),
#         y = ifelse(
#           .data$abs_label < 0, -150 + .data$abs_label, 150 + .data$abs_label
#         )
#       ),
#       alpha = 1
#     ) +
#     scale_y_continuous(labels = function(x){ifelse(x < 0, -1*x, x)}) +
#     geom_hline(aes(yintercept = 0)) +
#     labs(x = element_blank(), y = element_blank()) +
#     coord_flip() +
#     theme_bw() +
#     theme(
#       panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
#       axis.ticks.y = element_blank(), panel.border = element_blank(),
#       legend.title = element_blank(), legend.position = 'top',
#       axis.ticks.x = element_blank(), text = element_text(size = 18)
#     )

#   # return result
#   return(p)
# }

# #' helper function
# #' @name plot_q3_4
# #' @description function to prepare the variables of interest
# #' @param country plot age distribution
# #' @import ggplot2
# #' @importFrom rlang .data .env
# #' @export
# plot_q3_4 <- function(country = NULL){

#   # filter data for question q2_3
#   data <- DiasporaSurveyResults::prepare_data('q3_4')

#   # add country filter
#   if (!is.null(country)) {
#     if (country != 'All') {
#       data <- dplyr::filter(data, .data$q3_2 == country)
#     }
#   }

#   # isolate the number of variables
#   samples <- nrow(data)

#   # prepare data for plot
#   data_prepared <- data %>%
#     dplyr::mutate(dob = ifelse(.data$q3_4 == 'Before 1940', 1939, .data$q3_4)) %>%
#     dplyr::mutate(age = 2020 - as.numeric(.data$dob)) %>%
#     dplyr::mutate(age = cut(.data$age, seq(0,90,10), include.lowest=TRUE)) %>%
#     dplyr::count(.data$age)

#   # extract labels
#   data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
#   data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

#   # assign labels
#   data_final <- data_prepared %>%
#     dplyr::mutate(question = ordered(.data$age, levels = data_labels))

#   # set plot data
#   p <- ggplot(
#     data_final,
#     aes(x = .data$question, y = .data$n, fill = .data$question, alpha = .95)
#   )

#   # create plot
#   p <- p +
#     geom_bar(stat = 'identity', alpha = .8) +
#     geom_text(aes(label = .data$n, y = .075*max(.data$n), size = 12), alpha=1) +
#     scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 25)}) +
#     labs(x = element_blank(), y = paste0('Respondents: ', samples)) +
#     coord_flip() +
#     theme_bw() +
#     theme(
#       panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
#       axis.ticks.y = element_blank(), panel.border = element_blank(),
#       legend.position = 'none', axis.ticks.x = element_blank(),
#       axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
#       text = element_text(size = 18)
#     )

#   # return result
#   return(p)
# }


# #' helper function
# #' @name plot_q5_3
# #' @description function to prepare the variables of interest
# #' @param country plot time of residency
# #' @import ggplot2
# #' @importFrom rlang .data .env
# #' @export
# plot_q5_3 <- function(country = NULL){

#   # filter data for question q2_3
#   data <- DiasporaSurveyResults::prepare_data('q5_3')

#   # add country filter
#   if (!is.null(country)) {
#     if (country != 'All') {
#       data <- dplyr::filter(data, .data$q3_2 == country)
#     }
#   }

#   # isolate the number of variables
#   samples <- nrow(data)

#   # prepare data for plot
#   data_prepared <- data %>%
#     DiasporaSurveyResults::count_unique('q5_3')

#   # extract labels
#   data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
#   data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

#   # reorder labels
#   data_labels <- data_labels[c(2,5,3,4,6,1)]

#   # assign labels
#   data_final <- data_prepared %>%
#     dplyr::mutate(question = ordered(.data$question, levels = data_labels))

#   # set plot data
#   p <- ggplot(
#     data_final,
#     aes(x = .data$question, y = .data$abs_total, fill = .data$question)
#   )

#   # create plot
#   p <- p +
#     geom_bar(stat = 'identity', alpha = .8) +
#     geom_text(
#       aes(label = .data$abs_total, y = .075*max(.data$abs_total), size = 12),
#       alpha = 1
#     ) +
#     scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 10)}) +
#     labs(y = element_blank(), x = paste0('Respondents: ', samples)) +
#     theme_bw() +
#     theme(
#       panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
#       axis.ticks.y = element_blank(), panel.border = element_blank(),
#       legend.position = 'none', axis.ticks.x = element_blank(),
#       axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
#       text = element_text(size = 18)
#     )

#   # return result
#   return(p)
# }

# #' helper function
# #' @name plot_q6_2
# #' @description function to prepare the variables of interest
# #' @param country plot family members or friends who live in colombia
# #' @import ggplot2
# #' @importFrom rlang .data .env
# #' @export
# plot_q6_2 <- function(country = NULL){

#   library(tidyverse)
#   library(magrittr)
#   library(rlang)
#   library(ggplot2)

#   # filter data for question q2_3
#   data <- DiasporaSurveyResults::prepare_data('q6_2')

#   # add country filter
#   if (!is.null(country)) {
#     if (country != 'All') {
#       data <- dplyr::filter(data, .data$q3_2 == country)
#     }
#   }

#   # isolate the number of variables
#   samples <- nrow(data)

#   # prepare data for plot
#   data_prepared <- data %>%
#     DiasporaSurveyResults::count_multiple('q6_2')

#   # extract labels
#   data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
#   data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

#   # data_labels %>%
#   #   stringr::str_remove(' of the above|\\(i\\.e\\..*') %>%
#   #   stringr::str_replace('None', 'No one') %>%



#   # # reorder labels
#   # data_labels <- c(
#   #   'None of the above',
#   #   'Some of my wider family (i.e. grandparents, cousins etc.)',
#   #   'Some of my friends',
#   #   'Most of my friends',
#   #   'One or more of my siblings',
#   #   'One or both of my parents',
#   #   'Most of my wider family (i.e. grandparents, cousins etc.)'
#   # )
#   # data_labels <- data_labels[c(6,5,7,2,4,3,1)]

#   # assign labels
#   data_final <- data_prepared %>%
#     dplyr::mutate(question = ordered(.data$question, levels = data_labels))

#   # set plot data
#   p <- ggplot(
#     data_final,
#     aes(x = .data$question, y = .data$abs_total, fill = .data$question)
#   )

#   # create plot
#   p <- p +
#     geom_bar(stat = 'identity', alpha = .8) +
#     geom_text(
#       aes(label = .data$abs_total, y = .075*max(.data$abs_total), size = 12),
#       alpha = 1
#     ) +
#     scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 20)}) +
#     labs(x = element_blank(), y = paste0('Respondents: ', samples)) +
#     coord_flip() +
#     theme_bw() +
#     theme(
#       panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
#       axis.ticks.y = element_blank(), panel.border = element_blank(),
#       legend.position = 'none', axis.ticks.x = element_blank(),
#       axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
#       text = element_text(size = 18)
#     )

#   # return result
#   return(p)
# }

# #' helper function
# #' @name plot_q7_2
# #' @description function to prepare the variables of interest
# #' @param country plot family members or friends who live in colombia
# #' @import ggplot2
# #' @importFrom rlang .data .env
# #' @export
# plot_q7_2 <- function(country = NULL){

#   # filter data for question q2_3
#   data <- DiasporaSurveyResults::prepare_data('q7_2')

#   # add country filter
#   if (!is.null(country)) {
#     if (country != 'All') {
#       data <- dplyr::filter(data, .data$q3_2 == country)
#     }
#   }

#   # isolate the number of variables
#   samples <- nrow(data)

#   # prepare data for plot
#   data_prepared <- data %>%
#     DiasporaSurveyResults::count_unique('q7_2')

#   # extract labels
#   data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
#   data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

#   # assign labels
#   data_final <- data_prepared %>%
#     dplyr::mutate(question = ordered(.data$question, levels = data_labels))

#   # set plot data
#   p <- ggplot(
#     data_final,
#     aes(x = .data$question, y = .data$abs_total, fill = .data$question)
#   )

#   # create plot
#   p <- p +
#     geom_bar(stat = 'identity', alpha = .8) +
#     geom_text(
#       aes(label = .data$abs_total, y = .075*max(.data$abs_total), size = 12),
#       alpha = 1
#     ) +
#     scale_x_discrete(labels = function(x){stringr::str_wrap(x, width = 20)}) +
#     labs(x = element_blank(), y = paste0('Respondents: ', samples)) +
#     coord_flip() +
#     theme_bw() +
#     theme(
#       panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
#       axis.ticks.y = element_blank(), panel.border = element_blank(),
#       legend.position = 'none', axis.ticks.x = element_blank(),
#       axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1),
#       text = element_text(size = 18)
#     )

#   # return result
#   return(p)
# }
