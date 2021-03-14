# test whether we can prepare the data properly
testthat::test_that('prepare data function', {

  # create first data
  data <- DiasporaSurveyResults::survey_data %>%
    dplyr::filter(!is.na(q3_2))

  # test whether the filtering works
  testthat::expect_true(
    dplyr::all_equal(DiasporaSurveyResults::prepare_data('q3_2'), data)
  )

})

# # test whether the count works properly
# testthat::test_that('count data function', {


# })
