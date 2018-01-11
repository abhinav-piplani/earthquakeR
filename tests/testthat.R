library(testthat)
library(earthquakeR)
library(lubridate)

# Check eq_data
test_that("eq_data", {
  data <- eq_data()
  expect_true(is.data.frame(data))
})


# Check eq_clean_data
test_that("eq_clean_data", {
  raw_data <- data.frame(251,7,9,"25.500","35.500","GREECE:  CRETE")
  colnames(raw_data) <- c("YEAR", "MONTH", "DAY", "LONGITUDE",
                          "LATITUDE", "LOCATION_NAME")
  data <- raw_data %>% eq_clean_data()
  expect_equal(data$neg.date[1], 0)
})


# Check eq_location_clean
test_that("eq_location_clean", {
  raw_data <- data.frame(251,7,9,"25.500","35.500","GREECE:  CRETE")
  colnames(raw_data) <- c("YEAR", "MONTH", "DAY", "LONGITUDE",
                          "LATITUDE", "LOCATION_NAME")
  data <- raw_data %>% eq_clean_data() %>% eq_location_clean()
  expect_equal(data$LOCATION_NAME[1], "Greece  Crete")

})


# Check geom_timeline
test_that("geom_timeline", {
  g <-  eq_clean_data(eq_data()) %>% eq_location_clean() %>%
    dplyr::filter(year(DATE) > 2000  & COUNTRY %in% c("CANADA")) %>%
    dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 colour = TOTAL_DEATHS,
                                 size = EQ_PRIMARY
    )) +
    geom_timeline()

  expect_is(g, "ggplot")
})

# Check geom_timeline_label
test_that("geom_timeline_label",{
  g <- eq_clean_data(eq_data()) %>% eq_location_clean() %>%
    dplyr::filter(year(DATE) > 2000  & COUNTRY %in% c("CANADA")) %>%
    dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
                  EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 colour = TOTAL_DEATHS,
                                 size = EQ_PRIMARY
    )) +
    geom_timeline() +
    geom_timeline_label()

  expect_is(g, "ggplot")

})


# Check eq_map
test_that("eq_map", {
  temp.object <- eq_clean_data(eq_data()) %>% eq_location_clean() %>%
    dplyr::filter(year(DATE) > 2000 & !neg.date & COUNTRY %in% c("MEXICO")) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_is(temp.object, "leaflet")
  expect_is(temp.object, "htmlwidget")
})

# Check eq_create_label
test_that("eq_create_label", {
  data <- data.frame("XYZ", 3.2, 47)
  colnames(data) <- c("LOCATION_NAME", "EQ_PRIMARY", "TOTAL_DEATHS")
  labels <- data %>% eq_create_label()
  expect_equal(length(labels), 1)
  expect_equal(labels, "<b>Location: </b> XYZ <br/> <b>Magnitude: </b> 3.2 <br/> ")
})
