
# rm(list= ls())
# setwd("F:/Coursera/Mastering Software Development in R/5 - Capstone Project/R-package/earthquakeR/")

#' Read the NOAA dataset
#'
#' @param file.name Path for raw data for NOAA dataset(default - signif.txt)
#' @return A data.frame for NOAA dataset
#'
#' @importFrom magrittr %>%
#' @importFrom readr read_delim
#'
#'
#' @examples \dontrun{
#'   clean_data <- eq_data()
#' }
#' @export
eq_data <-function(file.name = # paste0("./inst/extdata/signif.txt")
                   file.path(system.file("extdata",package="earthquakeR"),"signif.txt")
                              ) {
  readr::read_delim(file.name, "\t") %>% eq_clean_data
}


#' Takes raw NOAA data frame and returns a clean data frame
#'
#' @param df The raw uncleaned version of the data.frame
#' @return A clean version of the data.frame, after pre-processing
#'
#' @details BC years are marked with the 'neg.date' column
#' @examples \dontrun{
#'   raw_data <- eq_data()
#'   clean_data <- eq_clean_data(raw_data)
#' }
#' @export
eq_clean_data <-function(df) {

  df$MONTH <- ifelse(is.na(df$MONTH), "01", df$MONTH)
  df$DAY <- ifelse(is.na(df$DAY), "01", df$DAY)
  df$neg.date <- ifelse(df$YEAR < 0, 1, 0)
  df$YEAR <- ifelse(df$YEAR < 0, (-1 * df$YEAR), df$YEAR)
  df$DATE <- ISOdate(year = df$YEAR, month = df$MONTH, day = df$DAY )
  df$LATITUDE <- as.numeric(df$LATITUDE)
  df$LONGITUDE <- as.numeric(df$LONGITUDE)
  return(df)
}

#' CLeans the 'LOCATION_NAME' column as per the requirement (Removing colons and semi-colons)
#' @param df The uncleaned data.frame
#' @return A clean data.frame
#'
#' @importFrom stringi stri_trans_totitle
#' @examples \dontrun{
#'   data <- eq_clean_data(eq_data()) %>% eq_location_clean()
#' }
eq_location_clean <- function(df) {

  df <- as.data.frame(df)
  mgsub <- function(pattern, replacement, x, ...) {
    if (length(pattern)!=length(replacement)) {
      stop("pattern and replacement do not have the same length.")
    }
    result <- x
    for (i in 1:length(pattern)) {
      result <- gsub(pattern[i], replacement[i], result, ...)
    }
    result
  }

  index <- which(colnames(df) == "LOCATION_NAME")
  df[,index] <- mgsub(pattern = c(":", ";"), replacement = c("", ""),
                              df[,index] )
  df[,index] <- stringi::stri_trans_totitle(df[,index] )
  return(df)

}




#' Geom for the timeline
#'
#' @importFrom dplyr group_by_ top_n ungroup
#' @importFrom ggplot2 aes draw_key_point Geom ggproto
#' @importFrom grid addGrob gList gpar gTree pointsGrob polylineGrob textGrob unit
#'
#' @details This is a helper function for geom_timeline and is kept for
#'         internal use only
GeomTimeline <-
  ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                   required_aes = c("x"),
                   default_aes = ggplot2::aes(y = "NA", size = 3, colour = "grey",
                                              fill = "grey", alpha = 0.5, stroke = 0.5, shape = 21),
                   draw_key = ggplot2::draw_key_point,
                   draw_panel = function(data, panel_scales, coord) {

                     ## Check for missing optional aesthetic
                     if (!("y" %in% colnames(data))) {
                       data$y <- 0.1
                     }

                     ## Data transformation
                     coordinates <- coord$transform(data, panel_scales)

                     ## Grid grob construction
                     tree = grid::gTree()
                     tree <- grid::addGrob(  tree,
                                             grid::pointsGrob( coordinates$x,  coordinates$y,
                                                               pch = coordinates$shape,
                                                               size = grid::unit(0.5*coordinates$size, "char"),
                                                               gp = grid::gpar(col = coordinates$colour,
                                                                               fill = coordinates$colour,  alpha = coordinates$alpha)
                                             )
                     )
                     ys = unique(coordinates$y)
                     xs = c(min(coordinates$x), max(coordinates$x))

                     tree <- grid::addGrob( tree,
                                            grid::polylineGrob( x = unit(rep(xs, each = length(ys)), "npc"),
                                                                y = unit(c(ys, ys), "npc"), id = rep(seq_along(ys), 2),
                                                                gp = grid::gpar(col = "grey",lwd = .pt)
                                            )
                     )
                     tree
                   })




#' Function to draw the earthquake timeline
#'
#' @inheritParams ggplot2::geom_point
#' @importFrom ggplot2 layer
#'
#' @details Requires the DATE as the x aesthetic, a grouping variable,
#'     e.g country for y, and 2 other variables for color and size, e.g. TOTAL_DEATHS and
#'     EQ_PRIMARY.
#' @examples \dontrun{
#'     eq_clean_data(eq_data()) %>% eq_location_clean()  %>%
#'       dplyr::filter(YEAR > 2000 & !neg.date & COUNTRY %in% c("USA", "CANADA")) %>%
#'       dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
#'                     EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
#'       ggplot2::ggplot(ggplot2::aes(x = DATE,
#'                                    y = COUNTRY,
#'                                    color = TOTAL_DEATHS,
#'                                    size = EQ_PRIMARY
#'                                    )) +
#'       geom_timeline()
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#' Geom  for timeline labels
#' @importFrom dplyr group_by_ top_n ungroup
#' @importFrom grid addGrob gList gpar pointsGrob polylineGrob textGrob unit
#' @importFrom ggplot2 aes draw_key_blank draw_key_point Geom ggproto
#'
#' @details This is a helper function for geom_timeline_label and is kept for
#'         internal use only
GeomTimelineLabel <- ggplot2::ggproto( "GeomTimelineLabel", ggplot2::Geom,
                                       required_aes = c("x", "label"),
                                       draw_key = ggplot2::draw_key_blank,
                                       draw_panel = function(data, panel_scales, coord, n_max) {
                                         if (!is.null(n_max)) {
                                           data <- data %>%
                                             dplyr::group_by_("group") %>% dplyr::top_n(n_max, size) %>%
                                             dplyr::ungroup()
                                         }

                                         if (!("y" %in% colnames(data))) {
                                           data$y <- 0.1
                                         }

                                         coordinates <- coord$transform(data, panel_scales)
                                         group_count <- length(unique(data$group))
                                         offset_value <- 0.1 / group_count

                                         lines <- grid::polylineGrob(
                                           x = unit(c(coordinates$x, coordinates$x), "npc"),
                                           y = unit(c(coordinates$y, coordinates$y + offset_value), "npc"),
                                           id = rep(1:dim(coordinates)[1], 2),
                                           gp = grid::gpar(
                                             col = "grey"
                                           )
                                         )

                                         names <- grid::textGrob(
                                           label = coordinates$label,
                                           x = unit(coordinates$x, "npc"),
                                           y = unit(coordinates$y + offset_value, "npc"),
                                           just = c("left", "bottom"),
                                           rot = 25
                                         )

                                         grid::gList(lines, names)
                                       }
)




#' Function to draw the earthquake timeline label
#'
#'
#' @inheritParams ggplot2::geom_point
#' @importFrom ggplot2 layer
#' @param n_max Integeral value correspoding to the maximum numer of labels to display
#' @details Only requires the dimension to be used as label, as well as 'n_max'
#'
#' @examples \dontrun{
#'     eq_clean_data(eq_data()) %>% eq_location_clean()  %>%
#'         dplyr::filter(YEAR > 2000 & !neg.date & COUNTRY %in% c("USA", "CANADA")) %>%
#'         dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
#'                       EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
#'         ggplot2::ggplot(ggplot2::aes(x = DATE,
#'                                      y = COUNTRY,
#'                                      colour = TOTAL_DEATHS,
#'                                      size = EQ_PRIMARY
#'                                      )) +
#'         geom_timeline() +
#'         labs(size = "Richter scale value", color = "# deaths") +
#'         geom_timeline_label(ggplot2::aes(label = LOCATION_NAME), n_max = 3)
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, n_max = NULL,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}



#' Map visualization of earthquakes
#'
#' @param data A data.frame containing the earthquake data
#' @param annot_col A column name indicating the column to be used for annotation pop up
#'
#' @details Only requires DATE column in the dataset
#' @importFrom magrittr %>%
#' @importFrom leaflet addCircleMarkers addTiles leaflet
#'
#' @examples \dontrun{
#'    eq_clean_data(eq_data()) %>% eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% eq_map()
#' }
#'
#' @export
eq_map <- function(data, annot_col = "DATE") {

  # Checking presence of required variables
  vars <- colnames(data)
  if("DATE" %in% vars * "LONGITUDE" %in% vars *
     "LATITUDE" %in% vars * "EQ_PRIMARY" %in% vars == 0) {
    stop("\n All the label variables are missing from the given dataset.\n") }

  # Leaflet
  leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      radius = ifelse(is.na(data[["EQ_PRIMARY"]]), 1, data[["EQ_PRIMARY"]]),
      color = ifelse(is.na(data[["EQ_PRIMARY"]]), "grey", "blue"),
      stroke = TRUE,
      fillOpacity = 0.5,
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      popup = data[[annot_col]]
    )
  }


#' Function to generate HTML code used in the marker's popup of a leaflet map.
#' It includes  location, magnitude and total deaths.
#' If any of the variables are NA, it is not displayed
#'
#' @param data a data frame from which information is required
#'
#' @return character vector containing HTML code
#'
#' @examples
#' \dontrun{
#'   eq_clean_data(eq_data()) %>% eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.))
#'
#' }
#'
#' @export
eq_create_label <- function(data) {

  vars <- colnames(data)

  # Generating the HTML code
  annotation <- paste(
    ifelse("LOCATION_NAME" %in% vars & !is.na(data$LOCATION_NAME), paste("<b>Location: </b>", data$LOCATION_NAME, "<br/>"), ""),
    ifelse("EQ_PRIMARY" %in% vars & !is.na(data$EQ_PRIMARY), paste("<b>Magnitude: </b>", data$EQ_PRIMARY, "<br/>"), ""),
    ifelse("DEATHS" %in% vars & !is.na(data$DEATHS), paste("<b>Total deaths: </b>", data$DEATHS, "<br/>"), ""))

  # Checking presence of required variables
  if(
    "LOCATION_NAME" %in% vars +
     "DEATHS" %in% vars + "EQ_PRIMARY" %in% vars == 0) {
    warnings("\n All the label variables are missing from the given dataset.\n") }

  return(annotation)
  }


# Example to check

#  eq_data("inst/extdata/signif.txt") %>%
#   # dplyr::filter(YEAR > 2000 & !neg.date & COUNTRY %in% c("MEXICO")) %>%
#   dplyr::filter(YEAR > 2000 & neg.date == 0 & COUNTRY %in% c("MEXICO")) %>%
#      dplyr::mutate(popup_text = eq_create_label(.)) %>%
#   eq_map(annot_col = "popup_text")
