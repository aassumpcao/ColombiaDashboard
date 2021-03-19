rm(list = ls())
devtools::document()

library(magrittr)
library(ggplot2)
library(tidyverse)
library(rlang)

# check if functions are working
data <- DiasporaSurveyResults::prepare_data('q2_3')
data_prepared <- DiasporaSurveyResults::count_multiple(data, 'q2_3', 'q3_3')
data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)

# load original data
load('data/survey_data.rda')

# filter data for question q2_3
data <- DiasporaSurveyResults::prepare_data('q2_3')
samples <- nrow(data)

# prepare data for plot
data_prepared <- data %>%
  DiasporaSurveyResults::count_multiple('q2_3') %>%
  dplyr::arrange(dplyr::desc(abs_total)) %>%
  dplyr::filter(!stringr::str_detect(x_label, '^(Other)')) %>%
  dplyr::mutate(
    perc_total = (abs_total / samples) * 100,
    perc_label = paste0(round(perc_total, 2), '%')
  )

# extract labels
data_prepared <- DiasporaSurveyResults::extract_labels(data_prepared)$data
data_labels <- DiasporaSurveyResults::extract_labels(data_prepared)$labels

# assign labels
data_final <- data_prepared %>%
  dplyr::mutate(question = ordered(question, levels = data_labels))

# create plot
p01 <- ggplot(data_final, aes(x = question, y = perc_total, fill = question))
p01 <- p01 +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = perc_label, y = 10)) +
  scale_x_discrete(labels = function(x){str_wrap(x, width = 30)}) +
  labs(x = element_blank(), y = paste0('Respondents: ', samples)) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
    axis.ticks.y = element_blank(), panel.border = element_blank(),
    legend.position = 'none', axis.ticks.x = element_blank(),
    axis.title.x = element_text(margin = margin(10,0,0,0), hjust = 1)
  )
p01

